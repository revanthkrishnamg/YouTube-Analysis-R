install.packages("shiny")

# Loading necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud)

# I. IMPORTING AND SEGMENTING THE DATA

# Specifying column types
col_types <- cols(
  video_id = col_character(),
  title = col_character(),
  description = col_character(),
  upload_date = col_datetime(format = ""),
  duration = col_character(),
  definition = col_character(),
  video_status = col_character(),
  view_count = col_double(),
  like_count = col_double(),
  comment_count = col_character() # Treat comment_count as character for consistency
)

file_paths <- c(
  "C:/Users/Administrator/Documents/R_2/2noob4u_Codm_Videos.csv", 
  "C:/Users/Administrator/Documents/R_2/Carnage_Codm_Videos.csv", 
  "C:/Users/Administrator/Documents/R_2/Ednox_Codm_Videos.csv", 
  "C:/Users/Administrator/Documents/R_2/PrinceRez_Codm_Videos.csv"
)

# Read, combine all datasets with explicit column types, and add channel name
merged_data <- lapply(file_paths, function(x) {
  channel_name <- tools::file_path_sans_ext(basename(x)) %>% gsub("_Codm_Videos", "", .)
  temp_data <- read_csv(x, col_types = col_types) %>% mutate(channel_name = channel_name)
  return(temp_data)
}) %>% bind_rows()

# Convert comment_count to numeric, handling non-numeric values gracefully
merged_data$comment_count <- as.numeric(as.character(merged_data$comment_count))

# Segmenting the merged data for different models
data_princerez <- merged_data %>% filter(channel_name == "PrinceRez")
data_other_channels <- merged_data %>% filter(channel_name != "PrinceRez")
# merged_data is already the combined dataset for the Combined Model

View(merged_data)
View(data_princerez)
View(data_other_channels)

# II. DATA CLEANING (applied to each dataset)

library(dplyr)

clean_data <- function(dataset) {
  # 1. Handle missing values
  dataset$description[is.na(dataset$description)] <- "Empty"
  dataset$comment_count <- ifelse(is.na(as.numeric(as.character(dataset$comment_count))), 0, as.numeric(as.character(dataset$comment_count)))
  
  # 2. Drop the 'video_status' column
  dataset <- dataset %>% select(-video_status)
  
  # Custom function to parse ISO 8601 durations and convert to total seconds
  parse_duration <- function(duration) {
    total_seconds <- 0
    if (!is.na(duration) && duration != "") {
      # Match groups for hours, minutes, and seconds
      matches <- regmatches(duration, gregexpr("([0-9]+)([HMS])", duration, perl = TRUE))
      for (match in matches[[1]]) {
        value <- as.numeric(sub("([HMS])", "", match))
        unit <- gsub("[0-9]+", "", match)
        
        if (unit == "H") {
          total_seconds <- total_seconds + value * 3600
        } else if (unit == "M") {
          total_seconds <- total_seconds + value * 60
        } else if (unit == "S") {
          total_seconds <- total_seconds + value
        }
      }
    }
    return(total_seconds)
  }
  
  # Apply the custom function to the 'duration' column
  dataset$duration <- sapply(dataset$duration, parse_duration)
  dataset$duration <- as.integer(dataset$duration)
  
  # 4. Remove duplicate rows
  dataset <- dataset[!duplicated(dataset), ]
  
  return(dataset)
}

# Apply cleaning function to each dataset
data_princerez <- clean_data(data_princerez)
data_other_channels <- clean_data(data_other_channels)
merged_data <- clean_data(merged_data)

# III. FEATURE ENGINEERING (applied to each dataset)

feature_engineering <- function(dataset) {
  # Time of Upload
  dataset$upload_hour <- format(dataset$upload_date, "%H")
  dataset$upload_day <- format(dataset$upload_date, "%d")
  dataset$upload_month <- format(dataset$upload_date, "%m")
  dataset$upload_year <- format(dataset$upload_date, "%Y")
  dataset$day_of_week <- as.integer(format(dataset$upload_date, "%u")) - 1
  dataset$is_weekend <- ifelse(dataset$day_of_week >= 5, 1, 0)
  
  # Video Length Category
  dataset$duration_category <- cut(dataset$duration,
                                   breaks = c(0, 60, 300, Inf),
                                   labels = c("short", "medium", "long"),
                                   right = FALSE)
  
  # Log Transformation with an epsilon to avoid log(0)
  epsilon <- 1
  dataset$log_view_count <- log(dataset$view_count + epsilon)
  dataset$log_like_count <- log(dataset$like_count + epsilon)
  dataset$log_comment_count <- log(dataset$comment_count + epsilon)
  
  # Z-Score Normalization
  z_score_normalization <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  dataset$z_view_count <- z_score_normalization(dataset$log_view_count)
  dataset$z_like_count <- z_score_normalization(dataset$log_like_count)
  dataset$z_comment_count <- z_score_normalization(dataset$log_comment_count)
  
  # Engagement Score Calculation
  calculate_z_weighted_engagement_score <- function(z_view, z_like, z_comment) {
    score <- z_view * 65 + z_like * 25 + z_comment * 10
    return(score)
  }
  
  temp_scores <- mapply(calculate_z_weighted_engagement_score, 
                        dataset$z_view_count, dataset$z_like_count, dataset$z_comment_count)
  
  # Rescaling the temporary scores to a 0-100 range
  dataset$engagement_score <- (temp_scores - min(temp_scores, na.rm = TRUE)) / 
    (max(temp_scores, na.rm = TRUE) - min(temp_scores, na.rm = TRUE)) * 100
  
  # Video Definition Category
  dataset$definition_numeric <- ifelse(dataset$definition == 'hd', 1, 0)
  
  # Length of Title and Description
  dataset$title_length <- nchar(as.character(dataset$title))
  dataset$description_length <- nchar(as.character(dataset$description))
  
  # Keyword Analysis (for title and description)
  library(tm) # For stopwords()
  library(wordcloud) # For wordcloud generation and also provides stopwords()
  library(stringr) # For string operations
  
  # Define the extract_keywords function
  extract_keywords <- function(text) {
    # Ensure text is a character string
    text <- as.character(text)
    # Tokenize words by splitting on non-word characters
    words <- unlist(strsplit(text, "\\W+"))
    # Load stopwords
    stopwords_list <- stopwords("en")
    # Filter out stopwords and non-alpha characters
    keywords <- words[words %in% stopwords_list == FALSE & grepl("^[[:alpha:]]+$", words)]
    return(keywords)
  }
  
  # Apply the extract_keywords function to both titles and descriptions
  dataset$title_keywords <- lapply(dataset$title, extract_keywords)
  dataset$description_keywords <- lapply(dataset$description, extract_keywords)
  
  # Convert lists of keywords back to strings for both titles and descriptions
  dataset$title_keywords_str <- sapply(dataset$title_keywords, paste, collapse=" ")
  dataset$description_keywords_str <- sapply(dataset$description_keywords, paste, collapse=" ")
  
  
  # Categorizing Engagement Score
  dataset$engagement_category <- cut(
    dataset$engagement_score,
    breaks = c(-Inf, 50, Inf), # Using 50 as the threshold between Low and High
    labels = c("Low", "High"),
    right = FALSE
  )
  
  return(dataset)
}

# Apply feature engineering to each dataset
data_princerez <- feature_engineering(data_princerez)
data_other_channels <- feature_engineering(data_other_channels)
merged_data <- feature_engineering(merged_data)

## IV. Exploratory Data Analaysis and Visualization:

# 1.Plotting numerical features distribution 
library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Numerical Features Distribution"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetInput", "Choose a dataset:",
                  choices = c("Merged Data" = "merged", 
                              "PrinceRez Data" = "princerez", 
                              "Other Channels Data" = "other_channels")),
      selectInput("featureInput", "Choose a feature:",
                  choices = c("View Count" = "view_count", 
                              "Like Count" = "like_count", 
                              "Comment Count" = "comment_count", 
                              "Duration" = "duration", 
                              "Engagement Score" = "engagement_score"))
    ),
    mainPanel(
      plotOutput("featurePlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$featurePlot <- renderPlot({
    dataset <- switch(input$datasetInput,
                      "merged" = merged_data,
                      "princerez" = data_princerez,
                      "other_channels" = data_other_channels)
    
    ggplot(dataset, aes_string(x = input$featureInput)) +
      geom_histogram(bins = 50, fill = "blue", color = "black") +
      ggtitle(paste("Distribution of", input$featureInput)) +
      xlab(input$featureInput) +
      ylab("Frequency")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# EDA 2 TO 9 done using one app
library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Assuming datasets and preprocessing steps are completed

# UI Definition
ui <- fluidPage(
  titlePanel("Video Engagement Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetInput", "Choose a dataset:",
                  choices = c("Merged Data" = "merged", 
                              "PrinceRez Data" = "princerez", 
                              "Other Channels Data" = "other_channels")),
      selectInput("analysisType", "Choose an analysis type:",
                  choices = c("Class Imbalance" = "class_imbalance", # 2
                              "Weekday vs Weekend Engagement" = "weekday_weekend", # 3
                              "Title Length vs Engagement" = "title_length", # 4
                              "Description Length vs Engagement" = "description_length", # 4
                              "Video Definition vs Engagement" = "video_definition", # 5
                              "Video Length Category vs Engagement" = "video_length_category", # 6
                              "Keyword Analysis - High Engagement Titles" = "keyword_analysis")) # 7
    ),
    mainPanel(
      plotOutput("plotOutput"),
      uiOutput("wordCloudOutput")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive expression to select dataset
  selectedDataset <- reactive({
    switch(input$datasetInput,
           "merged" = merged_data,
           "princerez" = data_princerez,
           "other_channels" = data_other_channels)
  })
  
  # Output for plots
  output$plotOutput <- renderPlot({
    req(selectedDataset()) # Ensure the dataset is selected
    
    dataset <- selectedDataset()
    analysisType <- input$analysisType
    
    # 2. Checking for class imbalance
    if (analysisType == "class_imbalance") {
      ggplot(dataset, aes(x=engagement_category)) +
        geom_bar() +
        ggtitle("Class Imbalance in Engagement Category") +
        xlab("Engagement Category") +
        ylab("Count")
    }
    
    # 3. Weekday vs Weekend Engagement Score Analysis
    else if (analysisType == "weekday_weekend") {
      ggplot(dataset, aes(x=factor(is_weekend), y=engagement_score, fill=factor(is_weekend))) +
        geom_bar(stat="summary", fun="mean") +
        scale_fill_discrete(name="Day Type", labels=c("Weekday", "Weekend")) +
        labs(title="Average Engagement Score by Day Type", y="Average Engagement Score", x="Day Type") +
        theme_minimal()
    }
    
    # 4. Title and Description Length vs Engagement Analysis
    else if (analysisType == "title_length") {
      ggplot(dataset, aes(x=title_length, y=engagement_score)) +
        geom_point() +
        geom_smooth(method="lm") +
        labs(title="Engagement Score by Title Length", y="Engagement Score", x="Title Length")
    }
    else if (analysisType == "description_length") {
      ggplot(dataset, aes(x=description_length, y=engagement_score)) +
        geom_point() +
        geom_smooth(method="lm") +
        labs(title="Engagement Score by Description Length", y="Engagement Score", x="Description Length")
    }
    
    # 5. Video Definition vs Engagement Analysis
    else if (analysisType == "video_definition") {
      ggplot(dataset, aes(x=definition, y=engagement_score, fill=definition)) +
        geom_boxplot() +
        labs(title="Engagement Score by Video Definition", y="Engagement Score", x="Video Definition")
    }
    
    # 6. Video Length Category vs Engagement Analysis
    else if (analysisType == "video_length_category") {
      ggplot(dataset, aes(x=duration_category, y=engagement_score, fill=duration_category)) +
        geom_boxplot() +
        labs(title="Engagement Score by Video Length Category", y="Engagement Score", x="Video Length Category")
    }
    
    # Return NULL if the plot output is not suitable for the selected analysis
    else {
      return(NULL)
    }
  })
  
  # Output for word cloud - only displayed for Keyword Analysis
  output$wordCloudOutput <- renderUI({
    if (input$analysisType == "keyword_analysis") {
      # The actual word cloud generation is handled in an observer due to its nature
      return(NULL)
    } else {
      return(NULL)
    }
  })
  
  # 7. Keyword Analysis - Observing and dynamically generating word cloud
  observe({
    if (input$analysisType == "keyword_analysis") {
      high_engagement_titles <- selectedDataset()$title[selectedDataset()$engagement_category == "High"]
      
      corpus <- Corpus(VectorSource(high_engagement_titles))
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      tdm <- TermDocumentMatrix(corpus)
      m <- as.matrix(tdm)
      word_counts <- sort(rowSums(m), decreasing = TRUE)
      df_word_freq <- data.frame(word = names(word_counts), freq = word_counts)
      
      # Display word cloud in a separate observer
      wordcloud(words = df_word_freq$word, freq = df_word_freq$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

## 5. MODELLING
