# Load necessary libraries
library(topicmodels)  # For LDA modeling
library(FactoMineR)   # For PCA and clustering
library(factoextra)   # For visualization of PCA results
library(tidyr)
library(proxy)
library(gplots)
library(dplyr)
library(tm)  # For text preprocessing
library(text2vec)  # For text vectorization
library(ggplot2)  # For visualization
library(topicmodels)
library(viridis)
library(purrr)
library(syuzhet)
library(tidyverse)
library(dplyr)
library(gplots)
library(viridis)

# Load data
tests_df <- read_csv("tests_df.csv")

library(gplots)
library(viridis)

# Define color palettes
sentiment_palette <- c("blue", "red")  # Positive and negative colors for columns

# Assign dominant emotion colors to rows
emotion_colors <- setNames(emotion_palette, unique(tests_df$emotion_1))
# Assign sentiment colors to columns
column_colors <- ifelse(tests_df$positive > tests_df$negative, sentiment_palette[1], sentiment_palette[2])

# Initialize confusion matrix
num_pages <- nrow(tests_df)
confusion_matrix_emotion <- matrix(0, nrow = num_pages, ncol = num_pages,
                           dimnames = list(tests_df$account_handle, tests_df$account_handle))

# Calculate similarity scores based on ranked emotions
for (i in 1:num_pages) {
  for (j in 1:num_pages) {
    if (i != j) {
      # Calculate weighted similarity score based on ranked emotions
      similarity_score <- sum(tests_df$emotion_1[i] == tests_df$emotion_1[j]) + 
        1/2 * sum(tests_df$emotion_2[i] == tests_df$emotion_2[j]) + 
        1/3 * sum(tests_df$emotion_3[i] == tests_df$emotion_3[j]) + 
        1/4 * sum(tests_df$emotion_4[i] == tests_df$emotion_4[j]) + 
        1/5 * sum(tests_df$emotion_5[i] == tests_df$emotion_5[j]) + 
        1/6 * sum(tests_df$emotion_6[i] == tests_df$emotion_6[j]) + 
        1/7 * sum(tests_df$emotion_7[i] == tests_df$emotion_7[j]) + 
        1/8 * sum(tests_df$emotion_8[i] == tests_df$emotion_8[j])
      
      # Assign similarity score to matrix
      confusion_matrix_emotion[i, j] <- similarity_score
    }
  }
}

saveRDS(confusion_matrix, "confusion_matrix_emotion.rds")

# Plot heatmap with row and column colors
png("confusion_matrix_heatmap_emotions_sentiments.png", width = 1200, height = 1200)
heatmap.2(confusion_matrix,
          trace = "none",
          col = viridis(100),
          RowSideColors = column_colors,  # Row colors for dominant emotion
          labRow = tests_df$account_handle,
          labCol = tests_df$account_handle,
          key.title = "Similarity Score",
          main = "Emotion & Sentiment-based Confusion Matrix Heatmap",
          density.info = "none",
          cexRow = 0.5,
          cexCol = 0.5)

# Legend for row colors (emotions) and column colors (sentiment)
legend("topright", legend = c("Positive Sentiment", "Negative Sentiment"), fill = sentiment_palette, title = "Sentiment")
dev.off()


# Convert confusion matrix to a distance matrix for hierarchical clustering
distance_matrix <- as.dist(1 - confusion_matrix)
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram of page similarity
plot(hc, labels = rownames(confusion_matrix), main = "Dendrogram of Page Similarity", xlab = "", sub = "", cex = 0.7)



