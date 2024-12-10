
# Load necessary libraries
library(tidyverse)
library(viridis)
library(syuzhet)
library(openxlsx)
# Load necessary libraries
library(dplyr)
library(writexl)  # For exporting to Excel


tests_df <- read_csv("Data_tests_df.csv") # has the data for all the testing set with their predicted topics (from the best LDA model identified), emotions and sentiment score using syuzhet
data <- readRDS("Data_all_training_data.rds") #Stores information on the LDA models
best_sample_model <- data$sample_3[[8]]$lda_model #Data from the best LDA model as calculated by perplexity and coherence
text_df <- read_csv("Data_text_df.csv") # has the data for all the training set with their predicted topics (from the best LDA model identified), emotions and sentiment score using syuzhet


# Extract terms from the best LDA model
terms <- as.data.frame(terms(best_sample_model, 100))  # Extract top 10 terms for each topic
colnames(terms_per_topic) <- paste0("Topic_", seq_len(ncol(terms_per_topic)))
terms <- terms %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Word") 

write_xlsx(terms, "LDA_Best_terms.xlsx") #export data for manual annotation

# Calculate average normalized emotion scores per topic
emotions_testing_set <- tests_df %>%
  group_by(dominant_topic) %>%
  summarise(    
    avg_positive = mean(positive, na.rm = TRUE),
    avg_negative = mean(negative, na.rm = TRUE),
    avg_anger = mean(anger, na.rm = TRUE),
    avg_anticipation = mean(anticipation, na.rm = TRUE),
    avg_disgust = mean(disgust, na.rm = TRUE),
    avg_fear = mean(fear, na.rm = TRUE),
    avg_joy = mean(joy, na.rm = TRUE),
    avg_sadness = mean(sadness, na.rm = TRUE),
    avg_surprise = mean(surprise, na.rm = TRUE),
    avg_trust = mean(trust, na.rm = TRUE),
    avg_norm_anger = mean(norm_anger, na.rm = TRUE),
    avg_norm_anticipation = mean(norm_anticipation, na.rm = TRUE),
    avg_norm_disgust = mean(norm_disgust, na.rm = TRUE),
    avg_norm_fear = mean(norm_fear, na.rm = TRUE),
    avg_norm_joy = mean(norm_joy, na.rm = TRUE),
    avg_norm_sadness = mean(norm_sadness, na.rm = TRUE),
    avg_norm_surprise = mean(norm_surprise, na.rm = TRUE),
    avg_norm_trust = mean(norm_trust, na.rm = TRUE)
  )

# Calculate average normalized emotion scores per topic
emotions_training_set <- text_df %>%
  group_by(dominant_topic) %>%
  summarise(    
    avg_positive = mean(positive, na.rm = TRUE),
    avg_negative = mean(negative, na.rm = TRUE),
    avg_anger = mean(anger, na.rm = TRUE),
    avg_anticipation = mean(anticipation, na.rm = TRUE),
    avg_disgust = mean(disgust, na.rm = TRUE),
    avg_fear = mean(fear, na.rm = TRUE),
    avg_joy = mean(joy, na.rm = TRUE),
    avg_sadness = mean(sadness, na.rm = TRUE),
    avg_surprise = mean(surprise, na.rm = TRUE),
    avg_trust = mean(trust, na.rm = TRUE),
    avg_norm_anger = mean(norm_anger, na.rm = TRUE),
    avg_norm_anticipation = mean(norm_anticipation, na.rm = TRUE),
    avg_norm_disgust = mean(norm_disgust, na.rm = TRUE),
    avg_norm_fear = mean(norm_fear, na.rm = TRUE),
    avg_norm_joy = mean(norm_joy, na.rm = TRUE),
    avg_norm_sadness = mean(norm_sadness, na.rm = TRUE),
    avg_norm_surprise = mean(norm_surprise, na.rm = TRUE),
    avg_norm_trust = mean(norm_trust, na.rm = TRUE)
  )


# View the emotions per topic
View(emotions_training_set)

# Save emotions per topic to an Excel file
write_xlsx(emotions_testing_set, "emotions_testing_set.xlsx")
write_xlsx(emotions_training_set, "emotions_training_set.xlsx")




