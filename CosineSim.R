library(tm)
library(proxy)
library(gplots)
library(dplyr)
library(text2vec)
library(ggplot2)
library(Matrix)  # Load the Matrix package for Diagonal function
options(stringsAsFactors = FALSE)

# Load test data
posts <- read_csv("Copia de posts (1).csv")
posts <- subset(posts, is.na(account_handle)==FALSE)
posts$text <- ifelse(!is.na(posts$message), as.character(posts$message), "")  # If 'message' is not NA, use 'message'
posts$text <- paste(posts$text, ifelse(!is.na(posts$description), as.character(posts$description), ""), sep = " ")  # If 'description' is not NA, concatenate it
posts$p_text <- tolower(posts$text)
posts$p_text <- gsub("[[:punct:]]", "", posts$p_text)
posts$p_text <- gsub("\\d+", "", posts$p_text)
posts$p_text <- gsub("s$", "",  posts$p_text)

# Create a new data frame with cuisine and reviews
tests_df <- posts %>%
  group_by(account_handle) %>%
  summarize(text = paste(text, collapse = "\n"), Country = unique(account_page_admin_top_country))
View(tests_df)

# Text preprocessing
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

# Calculate IDF (Inverse Document Frequency)
num_docs <- nrow(dtm)
idf <- log(num_docs / colSums(as.matrix(dtm) > 0))

# Convert DTM to TF-IDF representation
tfidf <- as(dtm, "sparseMatrix") %*% Diagonal(x = idf)

# Convert TF-IDF matrix to a dense matrix for similarity calculation
tfidf_dense <- as.matrix(tfidf)

# Print dimensions and structure of tfidf_dense
print(dim(tfidf_dense))
print(head(tfidf_dense))

# Calculate Cosine Similarity
similarity_matrix <- proxy::simil(tfidf_dense, method = "cosine")
str(similarity_matrix)
print(dim(similarity_matrix))

# Get the names of pages for labeling
pages <- tests_df$account_handle

# Define the color palette with 100 colors
my_palette <- colorRampPalette(c("white", "blue"))(100)

# Plot a heatmap of the similarity matrix using heatmap.2
# Plot a heatmap of the similarity matrix using heatmap.2
heatmap.2(as.matrix(similarity_matrix),
          col = my_palette,  # Specify color palette using the generated palette
          trace = "none",  # Do not show trace lines
          xlab = "Pages", ylab = "Pages",  # Label axes
          margins = c(10, 10),  # Increase margins to fit longer labels
          cexRow = 0.5, cexCol = 0.5,  # Decrease text size for labels
          labRow = pages, labCol = pages,  # Add row and column names
          main = "Cosine Similarity Heatmap")  # Main title

# Extract the similarity values from the similarity matrix
similarity_values <- as.matrix(similarity_matrix)

# Perform hierarchical clustering on the similarity values
row_clusters <- hclust(dist(t(similarity_values)), method = "complete")

# Retrieve the order of rows based on clustering
ordered_rows <- similarity_values[row_clusters$order, ]

# Determine the cluster labels based on hierarchical clustering
num_clusters <- 4  # Define the number of clusters
cluster_labels <- cutree(row_clusters, k = num_clusters)

# Define a color palette with distinct colors for each cluster
cluster_colors <- rainbow(num_clusters)  # Generate distinct colors for each cluster


# Plot the ordered similarity matrix with cluster colors
heatmap(ordered_rows, 
        Rowv = TRUE, Colv = TRUE,  # Enable row and column clustering
        col = my_palette,  # Specify color palette based on cluster labels
        scale = "none",  # Do not scale rows or columns
        xlab = "Cuisine", ylab = "Cuisine",  # Label axes
        main = "Ordered Cosine Similarity Heatmap",
        labRow = cuisine_names[row_clusters$order], labCol = cuisine_names[row_clusters$order])  # Set main title and labels with ordered names

# Define the number of clusters
num_clusters <- 4

# Perform hierarchical clustering on the similarity values
row_clusters <- hclust(dist(t(similarity_values)), method = "complete")

# Retrieve the order of rows based on clustering
ordered_rows <- similarity_values[row_clusters$order, ]

# Determine the cluster labels based on hierarchical clustering
cluster_labels <- cutree(row_clusters, k = num_clusters)

# Define a color palette with distinct colors for each cluster
cluster_colors <- rainbow(num_clusters)  # Generate distinct colors for each cluster

# Convert cluster labels to factor for better visualization
cluster_labels_factor <- as.factor(cluster_labels)

# Plot the ordered similarity matrix with cluster colors
heatmap(ordered_rows, 
        Rowv = TRUE, Colv = TRUE,  # Enable row and column clustering
        col = my_palette,  # Specify color palette based on cluster labels
        scale = "none",  # Do not scale rows or columns
        xlab = "Cuisine", ylab = "Cuisine",  # Label axes
        main = "Ordered Cosine Similarity Heatmap with Clusters",
        labRow = cuisine_names[row_clusters$order], labCol = cuisine_names[row_clusters$order],  # Set main title and labels with ordered names
        RowSideColors = cluster_colors[cluster_labels_factor[row_clusters$order]],  # Assign cluster colors to rows
        ColSideColors = cluster_colors[cluster_labels_factor[row_clusters$order]])  # Assign cluster colors to columns



