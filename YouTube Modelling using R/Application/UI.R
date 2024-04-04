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


# UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #wordcloudOutput { 
        margin-top: -25px; 
      }
      .shiny-output-error { 
        display: none; 
      }
    "))
  ),
  titlePanel("Exploratory Data Analysis and Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetInput", "Choose a dataset:",
                  choices = c("Merged Data" = "merged",
                              "PrinceRez Data" = "princerez",
                              "Other Channels Data" = "other_channels")),
      selectInput("analysisType", "Choose an analysis type:",
                  choices = c("Numerical Features Distribution" = "num_features_dist",
                              "Class Imbalance" = "class_imbalance",
                              "Weekday vs Weekend Engagement" = "weekday_weekend",
                              "Title Length vs Engagement" = "title_length",
                              "Description Length vs Engagement" = "description_length",
                              "Video Definition vs Engagement" = "video_definition",
                              "Video Length Category vs Engagement" = "video_length_category",
                              "Keyword Analysis - High Engagement Titles" = "keyword_analysis")),
      conditionalPanel(
        condition = "input.analysisType == 'num_features_dist'",
        selectInput("featureInput", "Choose a feature:",
                    choices = c("View Count" = "view_count",
                                "Like Count" = "like_count",
                                "Comment Count" = "comment_count",
                                "Duration" = "duration",
                                "Engagement Score" = "engagement_score"))
      )
    ),
    mainPanel(
      plotOutput("plotOutput"),
      htmlOutput("wordcloudOutput", style = "align: center;")
    )
  )
)




