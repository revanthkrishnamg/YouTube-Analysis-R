library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

## I. IMPORTING THE DATA

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
  comment_count = col_character() # Treat comment_count as character to ensure consistency
)

file_paths <- c("C:/Users/Administrator/Documents/R_2/2noob4u_Codm_Videos.csv", 
                "C:/Users/Administrator/Documents/R_2/Carnage_Codm_Videos.csv", 
                "C:/Users/Administrator/Documents/R_2/Ednox_Codm_Videos.csv", 
                "C:/Users/Administrator/Documents/R_2/PrinceRez_Codm_Videos.csv")

# Read and combine all datasets with explicit column types
merged_data <- lapply(file_paths, function(x) read_csv(x, col_types = col_types)) %>% bind_rows()

# Convert comment_count to numeric, handling non-numeric values gracefully
merged_data$comment_count <- as.numeric(as.character(merged_data$comment_count))

head(merged_data)

## II. DATA CLEANING:

# 1.Checking if there's any missing values for all columns:
missing_value <- colSums(is.na(merged_data))
print(missing_value)
### From output, we see there are missing data in description. Placing a string "Empty" for the missing values.
merged_data$description <- ifelse(is.na(merged_data$description), "Empty", merged_data$description)
### From output, we see missing values in comment_count as well, placing 0 for the missing values here
merged_data$comment_count[is.na(merged_data$comment_count)] <- 0

# 2.Checking on the categorical data: 
# definition
print(unique(merged_data$definition))
# video_status
print(unique(merged_data$video_status))
# Dropping video_status
merged_data$video_status <- NULL

# 3. Checking the data types of all the columns
sapply(merged_data, class)

# Handling the duration
library(dplyr) # Load dplyr, which also makes `%>%` available 
library(lubridate)

# Custom function to parse ISO 8601 durations and convert to total seconds
parse_duration <- function(duration) {
  # Replace "PT" and then split the string by "H", "M", and "S"
  parts <- gsub("PT|H|M|S", " ", duration) %>% trimws() %>% strsplit(" ")
  # Initialize variables
  hours <- minutes <- seconds <- 0
  # Assign the parts to hours, minutes, seconds depending on their presence
  if (length(parts[[1]]) == 3) {
    hours <- as.numeric(parts[[1]][1])
    minutes <- as.numeric(parts[[1]][2])
    seconds <- as.numeric(parts[[1]][3])
  } else if (length(parts[[1]]) == 2) {
    minutes <- as.numeric(parts[[1]][1])
    seconds <- as.numeric(parts[[1]][2])
  } else if (length(parts[[1]]) == 1) {
    seconds <- as.numeric(parts[[1]][1])
  }
  # Return total duration in seconds
  return (hours * 3600 + minutes * 60 + seconds)
}

# Apply the custom function to the 'duration' column of merged_data
merged_data$duration <- sapply(merged_data$duration, parse_duration)
# Convert the 'duration' column to integer in merged_data
merged_data$duration <- as.integer(merged_data$duration)

# 4.Checking if there are any duplicate rows
# Check the number of rows and columns before removing duplicates
nrow(merged_data)
ncol(merged_data)

# Identify duplicated rows (except for the first occurrence)
duplicates <- duplicated(merged_data)
# Subset the dataframe to only include the duplicated rows
merged_data_duplicates <- merged_data[duplicates, ]
# Print the duplicated rows
print(merged_data_duplicates)
# Show the number of duplicated rows
num_duplicated_rows <- nrow(merged_data_duplicates)
print(paste("Number of duplicated rows:", num_duplicated_rows))

# Removing the duplicated values
merged_data <- merged_data[!duplicated(merged_data), ]

# Checking rows and columns again
nrow(merged_data)
ncol(merged_data)

# 5. Checking for negative values for duration, view_count, like_count and removing them if found.
# Check for negative values in specified columns
negative_duration <- any(merged_data$duration < 0)
negative_view_count <- any(merged_data$view_count < 0)
negative_like_count <- any(merged_data$like_count < 0)

# Print results for negative values
print(paste("Negative values in duration:", negative_duration))
print(paste("Negative values in view_count:", negative_view_count))
print(paste("Negative values in like_count:", negative_like_count))

## III. Feature Engineering: 

# 1. Time of Upload (multiple columns based on the upload time):

# Extracting components from the 'upload_date' column in merged_data
merged_data$upload_hour <- format(merged_data$upload_date, "%H")
merged_data$upload_day <- format(merged_data$upload_date, "%d")
merged_data$upload_month <- format(merged_data$upload_date, "%m")
merged_data$upload_year <- format(merged_data$upload_date, "%Y")
merged_data$day_of_week <- as.integer(format(merged_data$upload_date, "%u")) - 1
# Identifying the weekends in merged_data
merged_data$is_weekend <- ifelse(merged_data$day_of_week >= 5, 1, 0)

# 2. Video Length Category (Short, Medium, Long):
# Categorizing video duration into Short, Medium, Long in merged_data
merged_data$duration_category <- cut(merged_data$duration,
                                     breaks = c(0, 60, 300, Inf),
                                     labels = c("short", "medium", "long"),
                                     right = FALSE)

# 3. Engagement Score (0-100)
# Creating a score based on views, likes and comments of a video where normalization was done 
# and weights were added to give views 65% weight, likes - 25% weight, comments - 10% weight.

# Ensuring the columns are numeric
merged_data$view_count <- as.numeric(merged_data$view_count)
merged_data$like_count <- as.numeric(merged_data$like_count)
merged_data$comment_count <- as.numeric(merged_data$comment_count)

# Calculating maximum values to use for normalization
max_view_count <- max(merged_data$view_count, na.rm = TRUE)
max_like_count <- max(merged_data$like_count, na.rm = TRUE)
max_comment_count <- max(merged_data$comment_count, na.rm = TRUE)

# Defining a function to calculate the weighted engagement score with numeric checks
calculate_weighted_engagement_score <- function(view_count, like_count, comment_count) {
  normalized_view <- (view_count / max_view_count) * 65
  normalized_like <- (like_count / max_like_count) * 25
  normalized_comment <- (comment_count / max_comment_count) * 10
  return(normalized_view + normalized_like + normalized_comment)
}

# Applying the function using mapply to calculate engagement scores for merged_data
merged_data$engagement_score <- mapply(calculate_weighted_engagement_score, merged_data$view_count, merged_data$like_count, merged_data$comment_count)

# Ensure the engagement score is within the range 0-100
merged_data$engagement_score <- pmin(pmax(merged_data$engagement_score, 0), 100)

# 4. Video Definition Category (HD=1, SD=0)
# Categorize video definition into HD (1) and SD (0)
merged_data$definition_numeric <- ifelse(merged_data$definition == 'hd', 1, 0)

# 5. Length of Title and Description
# Calculate the length of title and description in merged_data
merged_data$title_length <- nchar(as.character(merged_data$title))
merged_data$description_length <- nchar(as.character(merged_data$description))

# 6. Keyword Analysis (for title and description):
# Load necessary libraries
library(tm)
library(wordcloud) # For stopwords
library(stringr)

# Define the function to extract keywords
extract_keywords <- function(text) {
  # Ensure text is a character string
  text <- as.character(text)
  # Tokenize words
  words <- unlist(strsplit(text, "\\W"))
  # Load stopwords from the 'tm' package
  stopwords <- stopwords("en")
  # Filter out stopwords and non-alpha characters
  keywords <- words[words %in% stopwords == FALSE & grepl("^[[:alpha:]]+$", words)]
  return(keywords)
}

# Apply the function to extract keywords from titles and descriptions
merged_data$title_keywords <- lapply(merged_data$title, extract_keywords)
merged_data$description_keywords <- lapply(merged_data$description, extract_keywords)

# Convert lists of keywords back to strings for easier analysis or visualization
merged_data$title_keywords_str <- sapply(merged_data$title_keywords, paste, collapse=" ")
merged_data$description_keywords_str <- sapply(merged_data$description_keywords, paste, collapse=" ")

View(merged_data)




