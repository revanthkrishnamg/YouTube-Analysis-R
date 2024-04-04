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
library(shiny)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2) 
library(plotly)

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
      uiOutput("dynamicOutput") # Dynamic output for either plot or word cloud
    )
  )
)



# Server Logic
server <- function(input, output) {
  selectedDataset <- reactive({
    switch(input$datasetInput,
           "merged" = merged_data,
           "princerez" = data_princerez,
           "other_channels" = data_other_channels)
  })
  
  output$dynamicOutput <- renderUI({
    if (input$analysisType == "keyword_analysis") {
      wordcloud2Output("wordcloudPlot", width = "100%", height = "400px")
    } else {
      plotlyOutput("plotOutput", height = "400px")
    }
  })
  
  output$plotOutput <- renderPlotly({ 
    req(selectedDataset()) 
    dataset <- selectedDataset()
    analysisType <- input$analysisType
    plot <- NULL
    
    if (analysisType == "num_features_dist") {
      feature <- input$featureInput
      plot <- ggplot(dataset, aes_string(x = feature)) +
        geom_histogram(bins = 50, fill = "blue", color = "black") +
        ggtitle(paste("Distribution of", feature)) +
        xlab(feature) +
        ylab("Frequency")
    } else if (analysisType == "class_imbalance") {
      plot <- ggplot(dataset, aes(x = engagement_category)) +
        geom_bar(fill = "coral") +
        ggtitle("Class Imbalance in Engagement Category") +
        xlab("Engagement Category") +
        ylab("Count")
    } else if (analysisType == "weekday_weekend") {
      dataset$is_weekend <- as.factor(ifelse(dataset$is_weekend == 1, "Weekend", "Weekday"))
      plot <- ggplot(dataset, aes(x = is_weekend, fill = is_weekend)) +
        geom_bar(stat = "count") +
        scale_fill_manual(values = c("Weekday" = "skyblue", "Weekend" = "orange"),
                          name = "Day Type",
                          labels = c("Weekday", "Weekend")) +
        labs(title = "Engagement Score by Day Type", x = "", y = "Count") +
        theme_minimal()
    } else if (analysisType == "title_length" || analysisType == "description_length") {
      feature <- ifelse(analysisType == "title_length", "title_length", "description_length")
      plot <- ggplot(dataset, aes_string(x = feature, y = "engagement_score")) +
        geom_point(color = "dodgerblue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = paste("Engagement Score by", gsub("_", " ", feature)), y = "Engagement Score", x = feature)
    } else if (analysisType == "video_definition") {
      plot <- ggplot(dataset, aes(x = definition, fill = definition)) +
        geom_bar() +
        scale_fill_brewer(palette = "Pastel1") +
        labs(title = "Count by Video Definition", x = "Video Definition", y = "Count")
    } else if (analysisType == "video_length_category") {
      plot <- ggplot(dataset, aes(x = duration_category, fill = duration_category)) +
        geom_bar() +
        scale_fill_brewer(palette = "Set3") +
        labs(title = "Count by Video Length Category", x = "Video Length Category", y = "Count")
    }
    
    if (!is.null(plot)) {
      ggplotly(plot)
    }
  })
  
  output$wordcloudPlot <- renderWordcloud2({
    req(input$analysisType == "keyword_analysis")
    dataset <- selectedDataset()
    high_engagement_titles <- dataset$title[dataset$engagement_category == "High"]
    
    words <- tolower(unlist(strsplit(high_engagement_titles, "\\s+")))
    words <- words[!words %in% stopwords("en")] 
    word_freq <- table(words) 
    word_freq <- as.data.frame(word_freq, stringsAsFactors = FALSE)
    colnames(word_freq) <- c("word", "freq")
    wordcloud2(word_freq, size = 0.7)
  })
}


# Run the app
shinyApp(ui = ui, server = server)


## V. Model Building
# Approach:

library(dplyr)

prepare_dataset <- function(dataset) {
  # Applying one-hot encoding
  dataset <- dataset %>%
    mutate(definition_hd = as.integer(definition == "hd"),
           is_weekend = as.integer(is_weekend == 1)) %>%
    select(-definition) # Remove the original 'definition' column
  
  # Convert to factors to ensure categorical treatment
  dataset$duration_category <- as.factor(dataset$duration_category)
  dataset$day_of_week <- as.factor(dataset$day_of_week)
  dataset$engagement_category <- as.factor(dataset$engagement_category)
  
  # Using model.matrix to create one-hot encoded columns
  dataset_with_dummies <- cbind(dataset, model.matrix(~ duration_category + day_of_week + engagement_category - 1, data = dataset))

  dataset_with_dummies <- select(dataset_with_dummies, -c(duration_category, day_of_week, engagement_category))
  
  relevant_features <- c("duration", "definition_hd", "upload_hour", "upload_day", "upload_month", "upload_year", "is_weekend",
                         grep("duration_category", names(dataset_with_dummies), value = TRUE),
                         grep("day_of_week", names(dataset_with_dummies), value = TRUE),
                         grep("engagement_category", names(dataset_with_dummies), value = TRUE))
  
  # Keeping only the relevant features
  dataset_final <- dataset_with_dummies[, relevant_features]
  
  return(dataset_final)
}

# Applying the function to the datasets
data_princerez_prepared <- prepare_dataset(data_princerez)
data_other_channels_prepared <- prepare_dataset(data_other_channels)
merged_data_prepared <- prepare_dataset(merged_data)

# Removing some columns which are not important for the modelling
data_princerez_prepared <- data_princerez_prepared %>%
  select(-upload_year, -duration_categorylong, -upload_day, -upload_month)
data_other_channels_prepared <- data_other_channels_prepared %>%
  select(-upload_year, -duration_categorylong, -upload_day, -upload_month)
merged_data_prepared <- merged_data_prepared %>%
  select(-upload_year, -duration_categorylong, -upload_day, -upload_month)

data_princerez_prepared <- data_princerez_prepared %>%
  mutate(upload_hour = as.integer(upload_hour))
data_other_channels_prepared <- data_other_channels_prepared %>%
  mutate(upload_hour = as.integer(upload_hour))
merged_data_prepared <- merged_data_prepared %>%
  mutate(upload_hour = as.integer(upload_hour))

sapply(data_princerez_prepared, class)

## MODELLING 

library(caret)
library(dplyr)
library(e1071) 
library(plyr)
library(pROC)


train_and_evaluate_models <- function(dataset, target_column) {
  # Ensure the target column is a factor with valid R variable names for levels
  dataset[[target_column]] <- as.factor(dataset[[target_column]])
  levels(dataset[[target_column]]) <- make.names(levels(dataset[[target_column]]))
  
  # Split the data into features and target
  y <- dataset[[target_column]]
  X <- dataset %>% select(-which(names(dataset) == target_column))
  
  # Creating training and test sets - 70% train, 30% test
  set.seed(0)
  trainIndex <- createDataPartition(y, p = .7, list = FALSE)
  X_train <- X[trainIndex, ]
  y_train <- y[trainIndex]
  X_test <- X[-trainIndex, ]
  y_test <- y[-trainIndex]
  
  models <- list(
    LogisticRegression = list(model = "glm", preProcess = TRUE),
    KNN = list(model = "knn", preProcess = TRUE),
    DecisionTree = list(model = "rpart", preProcess = FALSE),
    RandomForest = list(model = "rf", preProcess = FALSE),
    GaussianNB = list(model = "nb", preProcess = TRUE),
    SVM = list(model = "svmLinear", preProcess = TRUE)
  )
  
  results_list <- list()
  
  for (model_name in names(models)) {
    cat("Training and evaluating model:", model_name, "\n")
    
    current_model <- models[[model_name]]
    model_info <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
    
    if (current_model$preProcess) {
      preProcess_params <- preProcess(X_train, method = c("center", "scale"))
      X_train_processed <- predict(preProcess_params, X_train)
      X_test_processed <- predict(preProcess_params, X_test)
    } else {
      X_train_processed <- X_train
      X_test_processed <- X_test
    }
      trained_model <- train(x = X_train_processed, y = y_train, method = current_model$model, trControl = model_info)
      predictions <- predict(trained_model, newdata = X_test_processed)
    
    cm <- confusionMatrix(predictions, y_test)
    
    results_list[[model_name]] <- list(
      Model = model_name,
      Accuracy = cm$overall['Accuracy'],
      Recall = cm$byClass['Sensitivity'],
      F1_Score = cm$byClass['F1']
    )
  }
  
  results_df <- ldply(results_list, data.frame)
  
  return(results_df)
}

# Applying the function to the datasets
results_princerez <- train_and_evaluate_models(data_princerez_prepared, "engagement_categoryHigh")
results_other_channels <- train_and_evaluate_models(data_other_channels_prepared, "engagement_categoryHigh")
results_merged <- train_and_evaluate_models(merged_data_prepared, "engagement_categoryHigh")

print(results_princerez)
print(results_other_channels)
print(results_merged)

### TAKING THE BEST MODELS FOR THE 3 DATASETS AND BUILDING THEM

train_model_and_evaluate <- function(dataset, model_type, target_column, save_filename) {
  # Ensuring the target column is a factor
  dataset[[target_column]] <- as.factor(dataset[[target_column]])
  levels(dataset[[target_column]]) <- make.names(levels(dataset[[target_column]]))
  
  # Splitting the data into features and target
  y <- dataset[[target_column]]
  X <- dataset %>% select(-which(names(dataset) == target_column))
  
  # Splitting into training and test sets, similar as above, 70% train and 30% test
  set.seed(0) 
  trainIndex <- createDataPartition(y, p = .7, list = FALSE)
  X_train <- X[trainIndex, ]
  y_train <- y[trainIndex]
  X_test <- X[-trainIndex, ]
  y_test <- y[-trainIndex]
  
  # Training the model
  model_info <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
  if(model_type == "SVM") {
    model <- train(x = X_train, y = y_train, method = "svmLinear", trControl = model_info)
  } else {
    model <- train(x = X_train, y = y_train, method = model_type, trControl = model_info)
  }
  
  # Predicting on the test set
  predictions <- predict(model, X_test)
  prob_predictions <- predict(model, X_test, type = "prob")[,2] # Get class probabilities for ROC
  
  # Confusion Matrix
  cm <- confusionMatrix(predictions, y_test)
  print(cm)
  
  # ROC curve and AUC
  roc_result <- roc(response = y_test, predictor = as.numeric(prob_predictions), levels = rev(levels(y_test)))
  plot(roc_result, main = paste("ROC Curve for", model_type))
  print(auc(roc_result))
  
  # Saving the model
  saveRDS(model, save_filename)
}

# Applying it to our datasets
train_model_and_evaluate(data_princerez_prepared, "rf", "engagement_categoryHigh", "model_princerez.rds") # RandomForest for Princerez
train_model_and_evaluate(data_other_channels_prepared, "rpart", "engagement_categoryHigh", "model_other_channels.rds") # DecisionTree for other_channels
train_model_and_evaluate(merged_data_prepared, "svmLinear", "engagement_categoryHigh", "model_merged.rds") # SVM for merged


## MAKING A PREDICTION

# Loading the models
model_princerez <- readRDS("model_princerez.rds")
model_other_channels <- readRDS("model_other_channels.rds")
model_merged <- readRDS("model_merged.rds")

# 3 row s of Test Data
test_data <- data.frame(
  duration = c(60, 120, 180), 
  definition_hd = c(1, 1, 0), 
  upload_hour = c(15, 8, 22), 
  is_weekend = c(0, 1, 0), 
  duration_categoryshort = c(1, 0, 0), 
  duration_categorymedium = c(0, 1, 0),
  day_of_week1 = c(0, 0, 1),
  day_of_week2 = c(1, 0, 0),
  day_of_week3 = c(0, 1, 0),
  day_of_week4 = c(0, 0, 0),
  day_of_week5 = c(0, 0, 0),
  day_of_week6 = c(0, 0, 0)
)

# Making predictions with each model 
predictions_princerez <- predict(model_princerez, test_data, type = "raw")
predictions_other_channels <- predict(model_other_channels, test_data, type = "raw")
predictions_merged <- predict(model_merged, test_data, type = "raw")

# Combining predictions to get the majority vote 
get_majority_vote <- function(princerez, other_channels, merged) {
  votes <- c(princerez, other_channels, merged)
  majority_vote <- ifelse(sum(votes == "X1") >= 2, "X1", "X0")
  return(majority_vote)
}

final_predictions <- mapply(get_majority_vote, predictions_princerez, predictions_other_channels, predictions_merged)

# Print final predictions
print(final_predictions)



