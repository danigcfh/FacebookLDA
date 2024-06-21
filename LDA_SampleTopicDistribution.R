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

# Load your LDA model 
data <- readRDS("all_training_data.rds")
best_sample_model <- data$sample_3[[8]]$lda_model
terms(best_sample_model, 10)

# Load test data
test_data <- readRDS("test_data.rds")

# Extract the raw data from random samples
tests <- rbind(test_data$posts$sample_data, data$sample_5$sample_data, data$sample_1$sample_data, data$sample_2$sample_data)

# Ensure no training data is being used in this evaluation
tests <- anti_join(tests, data$sample_3$sample_data)

# Eliminate double observations
tests <- tests %>% distinct()

# Create a new data frame with cuisine and reviews
tests_df <- tests %>%
  group_by(account_handle) %>%
  summarize(text = paste(text, collapse = "\n"), Country = unique(account_page_admin_top_country))
View(tests_df)


# Assuming cuisine_df contains your preprocessed data

corpus <- Corpus(VectorSource(tests_df$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("es"))
corpus <- tm_map(corpus, stripWhitespace)
non_empty_corpus <- tm_filter(corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)

# Text Vectorization using TF-IDF
dtm <- DocumentTermMatrix(non_empty_corpus, control = list(minDocFreq = 1))
dtm <- dtm[rowSums(as.matrix(dtm) > 0) > 0, ]

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Train an LDA model
test_topics <- posterior(best_sample_model, newdata = dtm_matrix)$topics

# Convert the document-topic matrix from LDA to a data frame
doc_topic_df <- as.data.frame(test_topics)

View(doc_topic_df)

# Step 1: Sort and extract dominant topics
dominant_topics <- t(apply(doc_topic_df, 1, function(x) {
  order(x, decreasing = TRUE)[1:7]
}))

# Step 2: Create a new dataframe with dominant topics
dominant_topics_df <- data.frame(V1 = dominant_topics[,1],
                                 V2 = dominant_topics[,2],
                                 V3 = dominant_topics[,3],
                                 V4 = dominant_topics[,4],
                                 V5 = dominant_topics[,5],
                                 V6 = dominant_topics[,6])

# Add the page column from original doc_topic_df
dominant_topics_df <- cbind(page = tests_df$account_handle, tests_df$Country, dominant_topics_df)


# Assuming doc_topic_df contains the document-topic matrix and cuisine names

# Count the number of unique topics
unique_topics <- unique(dominant_topics_df$V1)

# Create a custom color palette based on the number of unique topics
num_colors <- length(unique_topics)+1
custom_colors <- c("white", "green", "orange", "red")
my_palette <- viridis(30)

# Initialize the confusion matrix
num_pages <- nrow(dominant_topics_df)
confusion_matrix <- matrix(0, nrow = num_pages, ncol = num_pages,
                           dimnames = list(dominant_topics_df$page, dominant_topics_df$page))

# Loop through each pair of pages
for (i in 1:num_pages) {
  for (j in 1:num_pages) {
    if (i != j) {
      # Initialize similarity score
      similarity_score <- 0
      
      # Check if pages i and j share each dominant topic and accumulate similarity score
      if (dominant_topics_df$V1[i] == dominant_topics_df$V1[j]) {
        similarity_score <- similarity_score + 1
      }
      if (dominant_topics_df$V2[i] == dominant_topics_df$V2[j]) {
        similarity_score <- similarity_score + 1/2
      }
      if (dominant_topics_df$V3[i] == dominant_topics_df$V3[j]) {
        similarity_score <- similarity_score + 1/3
      }
      if (dominant_topics_df$V4[i] == dominant_topics_df$V4[j]) {
        similarity_score <- similarity_score + 1/4
      }
      if (dominant_topics_df$V5[i] == dominant_topics_df$V5[j]) {
        similarity_score <- similarity_score + 1/5
      }
      if (dominant_topics_df$V6[i] == dominant_topics_df$V6[j]) {
        similarity_score <- similarity_score + 1/6
      }
      
      # Assign the accumulated similarity score to confusion matrix
      confusion_matrix[i, j] <- similarity_score
    }
  }
}

pages <- dominant_topics_df$page


# Set up a custom color palette
my_palette <- viridis(100)

# Create heatmap
heat <- heatmap.2(confusion_matrix,
                  trace = "none",    # No additional grid lines
                  col = my_palette,  # Use the custom color palette
                  margins = c(10, 10),  # Widen margins to fit row and column names
                  labRow = dominant_topics_df$page,  # Page names for rows
                  labCol = dominant_topics_df$page,  # Page names for columns
                  key.title = "Confusion Scores",   # Title for the color key
                  key.xlab = "Confusion Score",     # Label for the color key
                  key.ylab = "",                    # No label for the color key on y-axis
                  density.info = "none",            # No additional density plot
                  main = "Confusion Matrix Heatmap",
                  cexRow = 0.5,# Main title for the heatmap
                  cexCol = 0.5)

heatmap.2(confusion_matrix,
          trace = "none",    # No additional grid lines
          col = my_palette,  # Use the custom color palette
          labRow = NA,  # Page names for rows
          labCol = NA,  # Page names for columns
          key.title = "Confusion Scores",   # Title for the color key
          key.xlab = "Confusion Score",     # Label for the color key
          key.ylab = "",                    # No label for the color key on y-axis
          density.info = "none",            # No additional density plot
          main = "Confusion Matrix Heatmap",
          cexRow = 0.5,# Main title for the heatmap
          cexCol = 0.5# Main title for the heatmap
)


# Assuming confusion_matrix is a similarity matrix, we convert it to a distance matrix
distance_matrix <- as.dist(1 - confusion_matrix)

# Run Hclust
hc <- hclust(distance_matrix, method = "ward.D2")

#Visualize only dendogram
plot(hc, labels = rownames(confusion_matrix), main = "Dendrogram of Page Similarity", xlab = "", sub = "", cex = 0.7)

