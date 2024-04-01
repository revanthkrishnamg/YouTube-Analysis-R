# List of packages to install
packages <- c("lubridate", "stringr", "dplyr", 
              "wordcloud", "tm", "RColorBrewer", 
              "ggplot2", "reshape2")
# Installing the packages
install.packages(packages)


## I. Importing the dataset to R Studio
library(readr)
YT_df <- read_csv("R/YT_Data_Collected.csv")
head(YT_df)


## II. DATA CLEANING:

# 1.Checking if there's any missing values for all columns:
missing_value <- colSums(is.na(YT_df))
print(missing_value)
### From output, we see there are missing data in description. Placing a string "Empty" for the missing values.
YT_df$description <- ifelse(is.na(YT_df$description), "Empty", YT_df$description)

# 2.Checking on the categorical data: 
# category_id
print(unique(YT_df$category_id))
### Category 20 is Gaming Category in YouTube which the channel is based of, since it's a gaming channel this makes sense. But Category 24 is the Comedy Category where some funny gaming videos might have been tagged as comedy, so this doesn't contribute to any valuable insight. Hence will drop this column.
# Dropping category_id
YT_df$category_id <- NULL
# definition
print(unique(YT_df$definition))
# video_status
print(unique(YT_df$video_status))
# dropping video_status
YT_df$video_status <- NULL

# Checking the data types of all the columns
sapply(YT_df, class)

YT_df

library(dplyr) # Load dplyr, which also makes `%>%` available 
# ISO 8601 duration strings - column 'duration'
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
# Apply the custom function to the 'duration' column
YT_df$duration <- sapply(YT_df$duration, parse_duration)
# Convert the 'duration' column to integer
YT_df$duration <- as.integer(YT_df$duration)

YT_df

# Converting Comment count to numeric - but there are some non-numeric value "Not Available"
# Replace "Not Available" and any other non-numeric values with NA
YT_df$comment_count <- as.numeric(as.character(YT_df$comment_count))
# Replace NA values with 0
YT_df$comment_count[is.na(YT_df$comment_count)] <- 0
# Convert to integer
YT_df$comment_count <- as.integer(YT_df$comment_count)

# 3.Checking if there are any duplicate rows
nrow(YT_df)
ncol(YT_df)

# Identify duplicated rows (except for the first occurrence)
duplicates <- duplicated(YT_df)
# Subset the data frame to only include the duplicated rows
YT_df_duplicates <- YT_df[duplicates, ]
# View the duplicated rows
print(YT_df_duplicates)

# Removing the duplicated values
YT_df <- unique(YT_df)
 
# 4.Checking for negative values for duration, view_count, like_count, average_view_duration, subscribers_gained, subscribers_lost and removing them if found.
# Check for negative values in specified columns
negative_duration <- any(YT_df$duration < 0)
negative_view_count <- any(YT_df$view_count < 0)
negative_like_count <- any(YT_df$like_count < 0)
negative_average_view_duration <- any(YT_df$average_view_duration < 0)
negative_subscribers_gained <- any(YT_df$subscribers_gained < 0)
negative_subscribers_lost <- any(YT_df$subscribers_lost < 0)

# Print results
print(paste("Negative values in duration:", negative_duration))
print(paste("Negative values in view_count:", negative_view_count))
print(paste("Negative values in like_count:", negative_like_count))
print(paste("Negative values in average_view_duration:", negative_average_view_duration))
print(paste("Negative values in subscribers_gained:", negative_subscribers_gained))
print(paste("Negative values in subscribers_lost:", negative_subscribers_lost))


## III. Feature Engineering: 

# 1. Time of Upload (multiple columns based on the upload time):

# Taking components from the 'upload_date' column
YT_df$upload_hour <- format(YT_df$upload_date, "%H")
YT_df$upload_day <- format(YT_df$upload_date, "%d")
YT_df$upload_month <- format(YT_df$upload_date, "%m")
YT_df$upload_year <- format(YT_df$upload_date, "%Y")
YT_df$day_of_week <- as.integer(format(YT_df$upload_date, "%u")) - 1 # Adjusted to have Monday=0 to match Python's dayofweek
# Identifying the weekends
YT_df$is_weekend <- ifelse(YT_df$day_of_week >= 5, 1, 0)

# 2. Video Length Category (Short, Medium, Long):
YT_df$duration_category <- cut(YT_df$duration,
                               breaks = c(0, 60, 300, Inf),
                               labels = c("short", "medium", "long"),
                               right = FALSE)


# 3. Net Subscriber Gain per video
YT_df$net_subscribers_gained <- YT_df$subscribers_gained - YT_df$subscribers_lost

# 4. Engagement Score (0-100)
# Creating a score based on views, likes and comments of a video where normalization was done 
# and weights were added to give views 65% weight, likes - 25% weight, comments - 10% weight.

# Ensuring the columns are numeric
YT_df$view_count <- as.numeric(YT_df$view_count)
YT_df$like_count <- as.numeric(YT_df$like_count)
YT_df$comment_count <- as.numeric(YT_df$comment_count)

# Calculating maximum values in case of type conversion
max_view_count <- max(YT_df$view_count, na.rm = TRUE)
max_like_count <- max(YT_df$like_count, na.rm = TRUE)
max_comment_count <- max(YT_df$comment_count, na.rm = TRUE)

# Defining a function to calculate weighted engagement score with numeric checks
calculate_weighted_engagement_score <- function(view_count, like_count, comment_count) {
  if(is.numeric(view_count) & is.numeric(like_count) & is.numeric(comment_count)) {
    normalized_view <- (view_count / max_view_count) * 65
    normalized_like <- (like_count / max_like_count) * 25
    normalized_comment <- (comment_count / max_comment_count) * 10
    return(normalized_view + normalized_like + normalized_comment)
  } else {
    return(NA) # Return NA if any input is not numeric
  }
}
# Applying the function using mapply
YT_df$engagement_score <- mapply(calculate_weighted_engagement_score, YT_df$view_count, YT_df$like_count, YT_df$comment_count)

YT_df

# 5. Engagement Category
# Keeping top 30% as high engagement videos 
quantile_threshold <- quantile(YT_df$engagement_score, 0.70, na.rm = TRUE)
# Categorizing videos based on the threshold
YT_df$engagement_category <- ifelse(YT_df$engagement_score >= quantile_threshold, 'High', 'Low')

# 6. Video Definition Category (HD=1, SD=0)
YT_df$definition_numeric <- ifelse(YT_df$definition == 'hd', 1, 0)

# 7. Length of Title and Description
YT_df$title_length <- nchar(as.character(YT_df$title))
YT_df$description_length <- nchar(as.character(YT_df$description))

# 8. Keyword Analysis (for title and description):
library(tm)
library(wordcloud) # For stopwords
library(stringr)

extract_keywords <- function(text) {
  # Ensure text is a character string
  text <- as.character(text)
  # Tokenize words
  words <- unlist(strsplit(text, "\\W"))
  # Load stopwords
  stopwords <- stopwords("en")
  # Filter out stopwords and non-alpha characters
  keywords <- words[words %in% stopwords == FALSE & grepl("^[[:alpha:]]+$", words)]
  return(keywords)
}

YT_df$title_keywords <- lapply(YT_df$title, extract_keywords)
YT_df$description_keywords <- lapply(YT_df$description, extract_keywords)

# Convert lists of keywords back to strings
YT_df$title_keywords_str <- sapply(YT_df$title_keywords, paste, collapse=" ")
YT_df$description_keywords_str <- sapply(YT_df$description_keywords, paste, collapse=" ")


## IV. EDAV (Exploratory Data analysis & visualization):

# NOTE: 
# The dataset contains YT videos and YT live videos, removing the live videos (with duration >5000 seconds):
YT_df <- subset(YT_df, duration <= 5000)
YT_df <- YT_df[YT_df$duration <= 5000, ]


# 1. Engagement Score Distribution:
library(ggplot2)

ggplot(YT_df, aes(x = engagement_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  labs(title = 'Distribution of Engagement Scores', x = 'Engagement Score', y = 'Density') +
  theme_minimal()

# 2. Time Series Analysis:
# Plotting engagement score trend over time
ggplot(YT_df, aes(x = upload_date, y = engagement_score)) +
  geom_line() +
  theme_minimal() +
  labs(title = 'Engagement Score Trend Over Time', x = 'Upload Date', y = 'Engagement Score') +
  theme(plot.title = element_text(hjust = 0.5)) 

# 3. Analysis of Video Length Categories:
  ggplot(YT_df, aes(x = duration_category, y = engagement_score)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = 'Average Engagement Score by Video Length Category', 
       x = 'Video Length Category', y = 'Average Engagement Score') +
  theme_minimal()

# 4. Low and High Engagement Range Intervals (for likes, comments and views):
# Splitting the DataFrame into high and low engagement DataFrames
high_engagement_df <- subset(YT_df, engagement_category == 'High')
low_engagement_df <- subset(YT_df, engagement_category == 'Low')

calculate_range_interval <- function(dataframe, column) {
  min_value <- min(dataframe[[column]], na.rm = TRUE)
  max_value <- max(dataframe[[column]], na.rm = TRUE)
  return(paste(min_value, "-", max_value))
}

range_intervals_high_engagement <- list(
  view_count_range = calculate_range_interval(high_engagement_df, 'view_count'),
  comment_count_range = calculate_range_interval(high_engagement_df, 'comment_count'),
  like_count_range = calculate_range_interval(high_engagement_df, 'like_count'),
  engagement_score_range = calculate_range_interval(high_engagement_df, 'engagement_score')
)

range_intervals_low_engagement <- list(
  view_count_range = calculate_range_interval(low_engagement_df, 'view_count'),
  comment_count_range = calculate_range_interval(low_engagement_df, 'comment_count'),
  like_count_range = calculate_range_interval(low_engagement_df, 'like_count'),
  engagement_score_range = calculate_range_interval(low_engagement_df, 'engagement_score')
)

cat("Range Intervals for High Engagement Videos:\n")
for (key in names(range_intervals_high_engagement)) {
  cat(key, ": ", range_intervals_high_engagement[[key]], "\n")
}

cat("\nRange Intervals for Low Engagement Videos:\n")
for (key in names(range_intervals_low_engagement)) {
  cat(key, ": ", range_intervals_low_engagement[[key]], "\n")
}


# 5. Analysis of Upload Time

## Temporal Analysis - Engagement Score by Day of Week
# Define days vector to match your dataset's day_name ordering
days <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
# Create the day_name column based on day_of_week
YT_df$day_name <- days[YT_df$day_of_week + 1]
# Now, let's try plotting again
ggplot(YT_df, aes(x = day_name, y = engagement_score, group = 1)) +
  geom_line(color = 'lightgreen') +
  geom_point(color = 'darkgreen') +
  labs(title = 'Temporal Analysis - Engagement Score by Day of Week',
       x = 'Day of Week', y = 'Average Engagement Score') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_x_discrete(limits = days)

## Engagement Scores: Weekday vs Weekend
YT_df$weekend_weekday <- ifelse(YT_df$is_weekend == 0, 'Weekday', 'Weekend')
YT_df_avg <- aggregate(engagement_score ~ weekend_weekday, data = YT_df, mean)
ggplot(YT_df_avg, aes(x = weekend_weekday, y = engagement_score, fill = weekend_weekday)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_viridis_d(begin = 0.5, end = 1, direction = -1, name = "Day Type", guide = FALSE) + # Adjusted for viridis palette
  labs(title = 'Engagement Scores: Weekday vs Weekend',
       x = 'Type of Day', y = 'Average Engagement Score') +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")

## Average Engagement Score by Hour of Upload
YT_df$upload_hour <- as.factor(YT_df$upload_hour)
# Calculate the average engagement score for each upload hour
YT_df_avg <- aggregate(engagement_score ~ upload_hour, data = YT_df, mean)
# Order data by upload_hour to ensure line continuity
YT_df_avg$upload_hour <- factor(YT_df_avg$upload_hour, levels = unique(YT_df_avg$upload_hour))
# Plotting
ggplot(YT_df_avg, aes(x = upload_hour, y = engagement_score)) +
  geom_line(color = 'skyblue') +
  geom_point(color = 'blue') + # Add points to the line
  labs(title = 'Average Engagement Score by Hour of Upload',
       x = 'Hour of Day', y = 'Average Engagement Score') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
        plot.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

# 6. Video Definition (HD/SD) Analysis:
YT_df$definition_numeric <- factor(YT_df$definition_numeric, labels = c("SD", "HD"))
ggplot(YT_df, aes(x = definition_numeric, y = engagement_score)) +
  geom_boxplot() +
  labs(title = 'Engagement Score by Video Definition (HD=1, SD=0)', 
       x = 'Video Definition', y = 'Engagement Score') +
  theme_minimal()


# 7. Comparison of Average Duration in High and Low Engagement Videos:
high_engagement_videos <- subset(YT_df, engagement_category == "High")
low_engagement_videos <- subset(YT_df, engagement_category == "Low")
avg_duration_high <- mean(high_engagement_videos$duration, na.rm = TRUE)
avg_duration_low <- mean(low_engagement_videos$duration, na.rm = TRUE)
cat("Average Duration - High Engagement:", avg_duration_high, ", Low Engagement:", avg_duration_low, "\n")

# 8. Analysis of Title Impact on Engagement Score
# High Engagement Titles
library(wordcloud)
library(tm)
library(RColorBrewer)
high_engagement_titles <- YT_df$title[YT_df$engagement_category == "High"]
low_engagement_titles <- YT_df$title[YT_df$engagement_category == "Low"]
# High Engagement Titles
png("high_engagement_titles.png", width=800, height=400)
wordcloud(words = high_engagement_titles, scale = c(5,2), max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"), min.freq = 1)
dev.off()
# Low Engagement Titles
png("low_engagement_titles.png", width=800, height=400)
wordcloud(words = low_engagement_titles, scale = c(5,2), max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"), min.freq = 1)
dev.off()


