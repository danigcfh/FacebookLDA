# Load necessary libraries
library(topicmodels)  # For LDA modeling
library(FactoMineR)   # For PCA and clustering
library(factoextra)   # For visualization of PCA results
library(tidyr)
library(proxy)
library(dplyr)
library(tm)  # For text preprocessing
library(text2vec)  # For text vectorization
library(syuzhet)

# Load LDA model and test data
data <- readRDS("Data_all_training_data.rds")
best_sample_model <- data$sample_3[[8]]$lda_model
best_sample_text <- data$sample_3$sample_data

# Summarize combined data for text processing
text_df <- best_sample_text %>%
  group_by(account_handle) %>%
  summarize(text = paste(text, collapse = "\n"), Country = unique(account_page_admin_top_country))
View(text_df)

## Start with the LDA topic identification
# Preprocess text using the tm package
corpus <- Corpus(VectorSource(text_df$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("es")) %>%
  tm_map(stripWhitespace)

# Filter out any empty documents
non_empty_corpus <- tm_filter(corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)

# Vectorize text using TF-IDF
dtm <- DocumentTermMatrix(non_empty_corpus, control = list(minDocFreq = 1))
dtm <- dtm[rowSums(as.matrix(dtm) > 0) > 0, ]

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Compute posterior topics for test documents using LDA model
text_topics <- posterior(best_sample_model, newdata = as.matrix(dtm))$topics

# Convert document-topic matrix to a data frame
doc_topic_df <- as.data.frame(text_topics)

# Extract top 6 dominant topics for each document
dominant_topics_df <- as.data.frame(t(apply(doc_topic_df, 1, function(x) order(x, decreasing = TRUE)[1:6])))
names(dominant_topics_df) <- paste0("V", 1:6)
text_df <- cbind(text_df, dominant_topics_df)

## Follow with Syuzet for emotion analysis

# Apply sentiment analysis on each post's text
sentiment_results <- get_nrc_sentiment(text_df$text, language = "spanish")

# Combine sentiment results with the original dataframe
text_df <- cbind(text_df, sentiment_results)

# Define emotion columns
emotion_columns <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

# Normalize emotion scores
text_df <- text_df %>%
  rowwise() %>%
  mutate(
    # Calculate total emotion score for normalization
    total_emotion_score = sum(c_across(all_of(emotion_columns))),
    # Normalize each emotion score by the total
    across(all_of(emotion_columns), ~ ifelse(total_emotion_score == 0, 0, . / total_emotion_score), .names = "norm_{col}")
  ) %>%
  ungroup()

# Calculate the mean of the total emotion scores across all observations
mean_total_emotion_score <- mean(text_df$total_emotion_score, na.rm = TRUE)

# Add "highly_emotional" column based on whether the total emotion score is above the mean
text_df <- text_df %>%
  mutate(highly_emotional = total_emotion_score > mean_total_emotion_score)

# Rank normalized emotions for each row and store in separate columns
text_df <- text_df %>%
  mutate(
    # Rank normalized emotions for each row
    ranked_emotions = pmap(select(., starts_with("norm_")), ~ {
      norm_emotion_scores <- c(...)
      ranked <- names(sort(norm_emotion_scores, decreasing = TRUE))
      ranked
    })
  ) %>%
  # Expand ranked_emotions into separate columns for each rank
  mutate(
    emotion_1 = sapply(ranked_emotions, `[`, 1),
    emotion_2 = sapply(ranked_emotions, `[`, 2),
    emotion_3 = sapply(ranked_emotions, `[`, 3),
    emotion_4 = sapply(ranked_emotions, `[`, 4),
    emotion_5 = sapply(ranked_emotions, `[`, 5),
    emotion_6 = sapply(ranked_emotions, `[`, 6),
    emotion_7 = sapply(ranked_emotions, `[`, 7),
    emotion_8 = sapply(ranked_emotions, `[`, 8)
  ) %>%
  # Drop the list column if no longer needed
  select(-ranked_emotions)

## Analyze topic and emotions together


# Identify dominant topic for each page
text_df <- cbind(text_df, dominant_topic = apply(text_topics, 1, which.max))

# Calculate the dominant emotion by identifying the highest normalized emotion score for each page
text_df <- text_df %>%
  rowwise() %>%
  mutate(dominant_emotion = emotion_columns[which.max(c_across(starts_with("norm_")))])

#Save final df with all the information

write.csv(text_df,"Data_text_df.csv", row.names = FALSE)

