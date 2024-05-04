# YouTube Channel Analysis Project

## Overview
This project aims to enhance viewer engagement on the "PrinceRez Codm" YouTube channel and other CODM community channels through comprehensive data analytics and machine learning. The focus is on understanding unique engagement patterns, identifying general trends across the community, and optimizing content strategy using a combined model approach.

## Business Case
The project analyzes engagement data from "PrinceRez Codm" alongside aggregated data from other CODM channels to discern patterns that drive viewer interaction, aiming to use these insights for content optimization.

## Data Collection
Data for each channel was collected using the YouTube API, involving detailed engagement metrics such as views, likes, and comments. The Jupyter Notebook for data collection can be found in the `data collection` folder.

## Importing and Segmenting the Data
Four separate datasets were prepared and segmented based on the channel's data to which they pertain:
- PrinceRez Codm Dataset
- Carnage Codm Dataset
- Ednox Codm Dataset
- 2noob4u Codm Dataset

## Data Cleaning
Implemented a modular function to clean each dataset, handling missing values, removing unnecessary columns, converting time durations, and eliminating duplicates.

## Feature Engineering
Developed features to enhance the predictive capability of models, including time decomposition, video length categorization, and engagement score enhancements.

## Exploratory Data Analysis and Visualization
A Shiny app was deployed to facilitate interactive EDA, available in the `YouTube Modelling with R` folder. This app allows for dynamic exploration of data across multiple dimensions.
You can access the app here: [Shiny App for YouTube Data Analysis](https://revanthkrishnamg.shinyapps.io/application/).

## Model Building
Models were built for each segmented dataset using traditional machine learning algorithms. Model performance was evaluated through accuracy, recall, and F1 scores.

## Final Test Predictions
Final predictions were made using the best-performing models from the testing phase. Predictions were combined and analyzed to determine the overall accuracy of the engagement prediction models.

## Conclusion
This project demonstrated how targeted data analysis and machine learning can significantly improve content strategy and increase viewer engagement on YouTube. The combined model approach proved particularly effective, underscoring the importance of integrating diverse data sources.

## Key Folders and Files
- **Initial Analysis Folder**: Contains preliminary EDA files for the PrinceRez Codm channel.
- **Data Collection Folder**: Includes the Jupyter Notebook used for data collection from the four YouTube channels.
- **YouTube Modelling with R Folder**: Contains all R scripts, Quarto generated HTML files, and the source code for the Shiny EDA app.
