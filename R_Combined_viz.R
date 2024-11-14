
#Combined Vizualization
# Load necessary libraries
library(gplots)
library(viridis)
library(proxy)
library(readr)

#Load df with emotion and LDA analysis
tests_df <- read_csv("tests_df.csv")

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

# Step 3: Plot Heatmap of Combined Similarity Matrix
png("combined_topic_emotion_similarity_heatmap_manhattan.png", width = 2400, height = 2400)
heatmap.2(combined_similarity_matrix,
          trace = "none",
          col = viridis(50),
          labRow = tests_df$account_handle,   # Row labels as account handles
          labCol = tests_df$account_handle,   # Column labels as account handles
          key.title = "Combined Similarity",
          main = "Combined Topic and Emotion Similarity Heatmap",
          density.info = "none",
          cexRow = 0.5,
          cexCol = 0.5
)
dev.off()


# Convert confusion matrix to a distance matrix for hierarchical clustering
distance_matrix <- as.dist(1 - combined_similarity_matrix)
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram of page similarity
plot(hc, labels = rownames(confusion_matrix), main = "Dendrogram of Page Similarity", xlab = "", sub = "", cex = 0.7)


