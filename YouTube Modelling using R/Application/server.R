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
  "data/2noob4u_Codm_Videos.csv", 
  "data/Carnage_Codm_Videos.csv", 
  "data/Ednox_Codm_Videos.csv", 
  "data/PrinceRez_Codm_Videos.csv"
)


# Merging the datasets
merged_data <- lapply(file_paths, function(x) {
  channel_name <- tools::file_path_sans_ext(basename(x)) %>% gsub("_Codm_Videos", "", .)
  temp_data <- read_csv(x, col_types = col_types) %>% mutate(channel_name = channel_name)
  return(temp_data)
}) %>% bind_rows()

# Convert comment_count to numeric
merged_data$comment_count <- as.numeric(as.character(merged_data$comment_count))

# Segmenting the merged data for different models
data_princerez <- merged_data %>% filter(channel_name == "PrinceRez")
data_other_channels <- merged_data %>% filter(channel_name != "PrinceRez")

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

## III. FEATURE ENGINEERING (applied to each dataset)

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
  
  # Log Transformation with epsilon -> 1 to avoid log(0)
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
  
  # Rescaling to 0-100 range
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

library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)  

# Server Logic
server <- function(input, output) {
  selectedDataset <- reactive({
    switch(input$datasetInput,
           "merged" = merged_data,
           "princerez" = data_princerez,
           "other_channels" = data_other_channels)
  })
  
  output$plotOutput <- renderPlot({
    req(selectedDataset()) 
    dataset <- selectedDataset()
    analysisType <- input$analysisType
    
    plot <- NULL  # Initialize plot variable
    
    if (analysisType == "num_features_dist") {
      plot <- ggplot(dataset, aes_string(x = input$featureInput)) +
        geom_histogram(bins = 50, fill = "blue", color = "black") +
        ggtitle(paste("Distribution of", input$featureInput)) +
        xlab(input$featureInput) +
        ylab("Frequency")
    } else if (analysisType == "class_imbalance") {
      plot <- ggplot(dataset, aes(x = engagement_category)) +
        geom_bar() +
        ggtitle("Class Imbalance in Engagement Category") +
        xlab("Engagement Category") +
        ylab("Count")
    } else if (analysisType == "weekday_weekend") {
      # Assuming 'is_weekend' is a binary variable in your dataset
      plot <- ggplot(dataset, aes(x = as.factor(is_weekend), y = engagement_score, fill = as.factor(is_weekend))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("0" = "skyblue", "1" = "orange")) +
        labs(title = "Engagement Score by Day Type", x = "Day Type", y = "Average Engagement Score")
    } else if (analysisType %in% c("title_length", "description_length")) {
      feature <- ifelse(analysisType == "title_length", "title_length", "description_length")
      plot <- ggplot(dataset, aes_string(x = feature, y = "engagement_score")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(title = paste("Engagement Score by", gsub("_", " ", feature)), y = "Engagement Score", x = gsub("_", " ", feature))
    } else if (analysisType == "video_definition") {
      plot <- ggplot(dataset, aes(x = definition, y = engagement_score, fill = definition)) +
        geom_boxplot() +
        labs(title = "Engagement Score by Video Definition", x = "Video Definition", y = "Engagement Score")
    } else if (analysisType == "video_length_category") {
      plot <- ggplot(dataset, aes(x = duration_category, y = engagement_score, fill = duration_category)) +
        geom_boxplot() +
        labs(title = "Engagement Score by Video Length Category", x = "Video Length Category", y = "Engagement Score")
    }
    
    if (!is.null(plot)) {
      print(plot)
    }
  })
  
  # Keyword Analysis
  output$wordcloudOutput <- renderUI({
    req(input$analysisType == "keyword_analysis")
    dataset <- selectedDataset()
    high_engagement_titles <- dataset$title[dataset$engagement_category == "High"]
    
    words <- tolower(unlist(strsplit(high_engagement_titles, " ")))
    words <- words[!words %in% stopwords("en")]
    word_freq <- table(words)
    word_freq <- as.data.frame(word_freq)
    names(word_freq) <- c("word", "freq")
    
    wordcloud2(word_freq)
  })
}






