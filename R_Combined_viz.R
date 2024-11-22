
#Combined Vizualization
# Load necessary libraries
library(gplots)
library(viridis)
library(proxy)
library(readr)

#Load df with emotion and LDA analysis
tests_df <- read_csv("Data_tests_df.csv")

# Step 1: Generate Similarity Matrices
# Calculate similarity matrices for topics and emotions
topic_similarity_matrix <- proxy::simil(as.matrix(test_topics), method = "manhattan")
emotion_similarity_matrix <- proxy::simil(as.matrix(tests_df[emotion_columns]), method = "manhattan")

# Convert similarities to matrices for easy manipulation
topic_similarity_matrix <- as.matrix(topic_similarity_matrix)
emotion_similarity_matrix <- as.matrix(emotion_similarity_matrix)

# Step 2: Combine Similarity Matrices
# Average the topic and emotion similarity matrices to get a combined similarity matrix
combined_similarity_matrix <- (topic_similarity_matrix + emotion_similarity_matrix) / 2

#Set colors for topics and sentiment
sentiment_palette <- c("blue", "red")  # Positive and negative colors for columns
sentiment_colors <- ifelse(tests_df$positive > tests_df$negative, sentiment_palette[1], sentiment_palette[2])
topic_palette <- colorRampPalette(c("white", "darkblue"))(7)
topic_colors <- setNames(topic_palette, paste0("Topic_", 1:7))
page_colors <- topic_colors[paste0("Topic_", dominant_topics_df$V1)]

# Step 3: Plot Heatmap of Combined Similarity Matrix
png("Viz_Combined_manhattan.png", width = 1200, height = 1200)
heatmap.2(combined_similarity_matrix,
          trace = "none",
          col = viridis(50),
          RowSideColors = sentiment_colors,  # Custom topic colors for rows
          ColSideColors = page_colors, 
          labRow = tests_df$account_handle,   # Row labels as account handles
          labCol = tests_df$account_handle,   # Column labels as account handles
          key.title = "Combined Similarity",
          main = "Combined Topic and Emotion Similarity Heatmap",
          density.info = "none",
          cexRow = 0.5,
          cexCol = 0.5
)
legend("bottomright", legend = c("Positive Sentiment", "Negative Sentiment"), fill = sentiment_palette, title = "Sentiment")
legend("topright", legend = paste0("Topic ", 1:7), fill = topic_palette, title = "Dominant Topics")

dev.off()


# Convert confusion matrix to a distance matrix for hierarchical clustering
distance_matrix <- as.dist(1 - combined_similarity_matrix)
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram of page similarity
plot(hc, labels = rownames(confusion_matrix), main = "Dendrogram of Page Similarity", xlab = "", sub = "", cex = 0.7)


