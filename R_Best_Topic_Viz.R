# Load necessary libraries
library(topicmodels)  # For LDA modeling
library(FactoMineR)   # For PCA and clustering
library(factoextra)   # For visualization of PCA results
library(tidyverse)
library(proxy)
library(gplots)
library(dplyr)
library(tm)  # For text preprocessing
library(text2vec)  # For text vectorization
library(ggplot2)  # For visualization
library(topicmodels)
library(viridis)

# Load data
tests_df <- read_csv("Data_tests_df.csv")
dominant_topics_df <- read_csv("dominant_topics_df.csv")


# Initialize confusion matrix with specific topic match levels
num_pages <- nrow(tests_df)
confusion_matrix_topic <- matrix(0, nrow = num_pages, ncol = num_pages,
                           dimnames = list(dominant_topics_df$page, dominant_topics_df$page))

# Loop through each pair of pages
for (i in 1:num_pages) {
  for (j in 1:num_pages) {
    if (i != j) {
      # Calculate individual matching scores for each rank of topic
      similarity_score <- sum(tests_df$V1[i] == tests_df$V1[j]) + 
        1/2 * sum(tests_df$V2[i] == tests_df$V2[j]) + 
        1/3 * sum(tests_df$V3[i] == tests_df$V3[j]) + 
        1/4 * sum(tests_df$V4[i] == tests_df$V4[j]) + 
        1/5 * sum(tests_df$V5[i] == tests_df$V5[j]) + 
        1/6 * sum(tests_df$V6[i] == tests_df$V6[j])
      
      # Assign the computed similarity score to the confusion matrix
      confusion_matrix_topic[i, j] <- similarity_score
    }
  }
}



# Define a color gradient for topic clusters
topic_palette <- colorRampPalette(c("white", "darkblue"))(7)
topic_colors <- setNames(topic_palette, paste0("Topic_", 1:7))
page_colors <- topic_colors[paste0("Topic_", tests_df$V1)]


# Plot heatmap of confusion matrix
# Save heatmap as a PNG
png("Viz_heatmap_topic.png", width = 1200, height = 1200)
heatmap.2(confusion_matrix_topic,
          trace = "none",
          col = viridis(100),
          RowSideColors = page_colors,  # Custom topic colors for rows
          ColSideColors = page_colors,  # Custom topic colors for columns
          labRow = dominant_topics_df$page,
          labCol = dominant_topics_df$page,
          key.title = "Similarity Score",
          main = "Confusion Matrix Heatmap",
          density.info = "none",
          cexRow = 0.5,
          cexCol = 0.5)
legend("topright", legend = paste0("Topic ", 1:7), fill = topic_palette, title = "Dominant Topics")
dev.off() 

# Convert confusion matrix to a distance matrix for hierarchical clustering
distance_matrix <- as.dist(1 - confusion_matrix_topic)
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram of page similarity
plot(hc, labels = rownames(confusion_matrix_topic), main = "Dendrogram of Page Similarity", xlab = "", sub = "", cex = 0.7)

# Add a legend for topic colors
legend("topright", legend = paste0("Topic ", 1:7), fill = topic_palette, title = "Dominant Topics")
