
# Install the required libraries if not installed
if (!requireNamespace("textclean", quietly = TRUE)) {
  install.packages("textclean")
}

if (!requireNamespace("slam", quietly = TRUE)) {
  install.packages("slam")
}

# Load the required libraries
library(textclean)
library(slam)
library(sampling)
library(dbplyr)
library(readr)
library(NLP)
library(tm)
library(topicmodels)
library(SnowballC)
library(broom)
library(tokenizers)
library(textclean)
library(tidyverse)
library(stringr)
set.seed(1234)

posts <- read_csv("Data_Copia de posts (1).csv")
posts <- subset(posts, is.na(account_handle)==FALSE)
posts$text <- ifelse(!is.na(posts$message), as.character(posts$message), "")  # If 'message' is not NA, use 'message'
posts$text <- paste(posts$text, ifelse(!is.na(posts$description), as.character(posts$description), ""), sep = " ")  # If 'description' is not NA, concatenate it
posts$p_text <- tolower(posts$text)
posts$p_text <- gsub("[[:punct:]]", "", posts$p_text)
posts$p_text <- gsub("\\d+", "", posts$p_text)
posts$p_text <- gsub("s$", "",  posts$p_text)

# Additional preprocessing steps can be added as needed
# Tokenize words using 'tokenizers' package

# Example: Remove stopwords using tm package's removeWords
stopwordsSP <- readLines("spanish.txt", warn = FALSE, encoding = "UTF-8")
posts$p_text <- sapply(posts$p_text, function(x) paste(setdiff(unlist(strsplit(x, " ")), stopwordsSP), collapse = " "))

stopwordsEN <- readLines("English.txt", warn = FALSE, encoding = "UTF-8")
posts$p_text <- sapply(posts$p_text, function(x) paste(setdiff(unlist(strsplit(x, " ")), stopwordsEN), collapse = " "))

# Optional: Perform stemming or lemmatization appropriate for Spanish

posts$p_text <- wordStem(posts$p_text, language = "spanish")

# Filter out rows where both 'message' and 'description' are NA
posts <- subset(posts, text != " ")

posts$p_text <- sapply(posts$p_text, function(x) paste(tokenize_words(x), collapse = " "))


#example with one page

for (i in 1){
  page_data <- subset(posts, account_handle == "stopdesigualdad")
  
  # Create a corpus for the page's posts
  corpus <- Corpus(VectorSource(page_data$p_text))
  inspect(corpus)
  
  # Create a document-term matrix
  dtm <- DocumentTermMatrix(corpus)
  inspect(dtm)
  sparsity <- sum(dtm > 0) / (nrow(dtm) * ncol(dtm))
  cat("Sparsity of DTM:", sparsity, "\n")
  
  # Run LDA for the page
  lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))
  
  
  terms(lda_model, 10) #this does work
}

#calculate coherence in topics
# Function to calculate coherence
calculate_coherence <- function(lda_model, dtm, corpus) {
  # Extract the terms from the DTM
  terms <- colnames(as.matrix(dtm))
  
  # Get the topic-term matrix from the LDA model
  beta <- lda_model@beta
  
  # Calculate coherence for each topic
  topic_coherences <- numeric(lda_model@k)
  
  for (k in 1:lda_model@k) {
    # Extract the top terms for the current topic
    top_terms <- terms[order(beta[, k], decreasing = TRUE)[1:10]]  # Adjust the number of terms as needed
    
    # Extract the DTM from the Corpus object
    dtm <- DocumentTermMatrix(corpus)
    
    # Calculate pairwise co-occurrence
    co_occurrence <- findAssocs(dtm, terms = top_terms, corlimit = 0.25)  # Adjust the correlation limit as needed
    
    # Calculate coherence
    coherence <- sum(unlist(co_occurrence))/choose(length(top_terms), 2)
    
    topic_coherences[k] <- coherence
  }
  
  # Return the average coherence across topics
  return(mean(topic_coherences))
}

calculate_coherence(gen_lda_model, gen_dtm, gen_corpus)


#create a representative sample and run models for different k and alpha values
# Specify the values for k, alpha, and sample size
sample_size <- 5000 

# Initialize lists to store results

# Generate 5 unique samples
# Generate 5 unique samples
all_samples <- list()  # Initialize a list to store individual samples
all_samples_dtm <- list()
samples <- list()

for (sample_index in 1:5) {
  # Identify unique pages in the dataset
  unique_URL <- unique(posts$account_url)
  
  # Initialize an empty data frame to store the sampled data
  sampled_posts <- data.frame()
  
  # Initialize a variable to keep track of the total sampled size
  total_sampled_size <- 0
  
  # Iterate over each page and perform stratified sampling
  for (url in unique_URL) {
    # Subset data for the current page
    page_data <- subset(posts, account_url == url)
    
    # Determine the number of observations to sample from this page
    page_sample_size <- round(sample_size * nrow(page_data) / nrow(posts))
    
    # Randomly sample from the current page
    page_sample <- page_data[sample(nrow(page_data), size = page_sample_size, replace = FALSE), ]
    
    # Combine the sampled data from each page
    sampled_posts <- rbind(sampled_posts, page_sample)
    
    # Update the total sampled size
    total_sampled_size <- total_sampled_size + nrow(page_sample)
  }
  
  # Adjust the total sampled size to match the desired sample size
  if (total_sampled_size > sample_size) {
    sampled_posts <- sampled_posts[sample(1:nrow(sampled_posts), size = sample_size, replace = FALSE), ]
  }
  gen_corpus <- Corpus(VectorSource(sampled_posts$p_text))
  non_empty_corpus <- tm_filter(gen_corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)
  # Check if the document-term matrix is not empty
  if (length(non_empty_corpus) > 0) {
    # Create a document-term matrix
    dtm <- DocumentTermMatrix(non_empty_corpus, control = list(minDocFreq = 5))
    
    # Filter out documents with all-zero counts
    dtm <- dtm[rowSums(as.matrix(dtm) > 0) > 0, ]
    
    # Store the dtm data in the list
  
    all_samples_dtm[[paste("sample", sample_index, sep = "_")]] <- dtm
  }
  # Store the sampled data in the list
  samples[[paste("sample", sample_index, sep = "_")]] <- sampled_posts
  all_samples[[paste("sample", sample_index, sep = "_")]] <- non_empty_corpus
}

all_samples_dtm

#initialize to save stuff
# Initialize lists to store LDA models and their metadata
five_all_lda_models <- list()
five_metadata_list <- list()

k_values <- c(5, 10, 15)
alpha_values <- c(0.1, 0.5, 1/sample_size)

for (name in names(all_samples_dtm)) {
  if (nrow(all_samples_dtm[[name]]) > 0 && ncol(all_samples_dtm[[name]]) > 0 &&
      sum(all_samples_dtm[[name]]) > 0) {
    
    # Initialize lists to store LDA models and metadata for the current sample
    lda_models_sample <- list()
    metadata_sample <- list()
    
    # Iterate over hyperparameter values
    for (k in k_values) {
      for (alpha in alpha_values) {
        set.seed(1234)  # Adjust the seed as needed
        
        # Check if the document-term matrix is not empty
        if (nrow(all_samples_dtm[[name]]) > 0 && ncol(all_samples_dtm[[name]]) > 0 &&
            sum(all_samples_dtm[[name]]) > 0) {
          
          # Run LDA for the page
          lda_model <- LDA(all_samples_dtm[[name]], k = k, control = list(seed = 1234, alpha = alpha))
          
          # Store the LDA model in the list with a unique identifier
          lda_model_key <- paste("k", k, "alpha", alpha, "sample", name, sep = "_")
          lda_models_sample[[lda_model_key]] <- lda_model
          
          # Add metadata to the list
          metadata_sample[[lda_model_key]] <- data.frame(alpha = alpha, k = k, sample = name)
          cat("Proccessing")
        } else {
          cat("Empty or sparse document-term matrix for sample", name, "\n\n")
        }
      }
    }
    
    # Store results for this sample
    five_all_lda_models[[name]] <- lda_models_sample
    five_metadata_list[[name]] <- metadata_sample
  }
  cat("complete")
}

str(lda_model@beta)
# After running the above code, you can calculate coherence scores separately:
#remember to initiate the function coherence first
str(all_lda_models)

names(all_lda_models)


#save all in a single document

# Initialize the list to store all combined data
all_combined_data_list <- list()

# Generate 5 unique samples
for (sample_index in 1:5) {
  # Retrieve sample, dtm, and lda based on the index
  sample_key <- paste("sample", sample_index, sep = "_")
  
  sample <- samples[[sample_key]]
  dtm <- all_samples_dtm[[sample_key]]
  lda_models_sample <- all_lda_models[[sample_key]]
  metadata_sample <- metadata_list[[sample_key]]
  
  # Create a list to store data for all models in the sample
  all_combined_data <- list()
  
  # Iterate over keys in the sub-list
  for (lda_model_key in names(lda_models_sample)) {
    # Extract alpha and k values from the key
    # Extract alpha and k values from the key
    alpha_match <- regmatches(lda_model_key, regexpr("alpha_(0.[0-9]+)", lda_model_key))
    k_match <- regmatches(lda_model_key, regexpr("k_([0-9])", lda_model_key))
    
    # Check if there's a match for alpha and k
    if (length(alpha_match) > 0) {
      alpha_value <- as.numeric(sub("alpha_", "", alpha_match))
    } else {
      warning("No match found for alpha.")
    }
    
    if (length(k_match) > 0) {
      k_value <- as.numeric(gsub("[^0-9.]", "", k_match))
    } else {
      warning("No match found for k.")
    }
    
    lda_model <- lda_models_sample[[lda_model_key]]
    
    # Add alpha, k values, and sample data as metadata
    metadata <- list(alpha = alpha_value, k = k_value, sample_index = sample_index)
    
    # Store the combined data in the list
    all_combined_data <- c(all_combined_data, list(list(metadata = metadata, lda_model = lda_model)))
  }
  
  # Add the sample data to the list (since it's common for all models in the sample)
  all_combined_data <- c(all_combined_data, list(sample_data = sample, dtm_data = dtm))
  
  # Append the list for the current sample to the main list
  all_combined_data_list[[sample_key]] <- all_combined_data
}


# Print the first element of the list for sample_1
print(all_combined_data_list$sample_1)

# Save the list containing all combined data
saveRDS(all_combined_data_list, file = "Data_all_training_data.rds")



# Create models for each page
# Specify the values for k, alpha, and beta
k_values <- c(3, 5, 7)
alpha_values <- c(0.5, 0.1, 1/sample_size)

# Initialize lists to store DTMs and LDA models
pages_dtms <- list()
pages_lda_models <- list()

# Initialize list to store alpha and k values as metadata
pages_metadata <- list()

# Iterate over unique page names
unique_pages <- unique(posts$account_handle)


for (page in unique_pages) {
  # Skip if page is NA
  if (is.na(page)) {
    cat("Skipping NA page\n\n")
    next  # Skip to the next iteration
  }
  
  # Initialize lists for the current page
  lda_models <- list()
  dtms <- list()
  
  # Initialize list for metadata
  metadata_list <- list()
  
  # Iterate over hyperparameter values
  for (k in k_values) {
    for (alpha in alpha_values) {
      cat("Running LDA models for k =", k, ", alpha =", alpha, ", page =", page, "\n")
      
      # Subset data for the current page
      page_data <- subset(posts, account_handle == page)
      sample_size <- nrow(page_data)
      
      # Create a corpus for the page's posts
      corpus <- Corpus(VectorSource(page_data$p_text))
      
      # Remove empty documents
      non_empty_corpus <- tm_filter(corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)
      
      if (length(non_empty_corpus) > 0) {
        # Continue with the preprocessing steps
        # (Any additional preprocessing steps you may have)
        
        # Create a document-term matrix
        dtm <- DocumentTermMatrix(non_empty_corpus)
        
        # Filter out documents with all-zero counts
        non_empty_dtm <- dtm[rowSums(as.matrix(dtm) > 0) > 0, ]
        
        # Store the DTM in the list
        dtms[[paste("k", k, "_alpha", alpha)]] <- non_empty_dtm
        
        # Check if the document-term matrix is not empty
        if (nrow(non_empty_dtm) > 0 && ncol(non_empty_dtm) > 0 && sum(non_empty_dtm) > 0) {
          # Check if the sparsity is below a certain threshold
          sparsity_threshold <- 0.95
          if (sum(non_empty_dtm > 0) / (nrow(non_empty_dtm) * ncol(non_empty_dtm)) < sparsity_threshold) {
            # Run LDA for the page
            lda_model <- LDA(non_empty_dtm, k = k, control = list(seed = 1234, alpha = alpha))
            
            # Store the LDA model in the list
            lda_models[[paste("k", k, "_alpha", alpha)]] <- lda_model
            
            # Save alpha and k values as metadata
            metadata_list[[paste("k", k, "_alpha", alpha)]] <- data.frame(alpha = alpha, k = k)
          } else {
            cat("High sparsity. Skipping LDA for page", page, "\n\n")
          }
        } else {
          cat("Empty or sparse document-term matrix for page", page, "\n\n")
        }
      } else {
        cat("No non-empty documents for page", page, "\n\n")
      }
    }
  }
  
  # Save models and metadata for the current page
  page_key <- paste("page", page, sep = "_")
  pages_lda_models[[page_key]] <- lda_models
  pages_dtms[[page_key]] <- dtms
  pages_metadata[[page_key]] <- metadata_list
}



# Save models, metadata, and DTM for each page
# Create a list to store all combined data
pages_combined_data_list <- list()
page_keys_list <- names(pages_lda_models)


for (page_key in page_keys_list) {
  # Extract page name from the key
  page_name <- gsub("^page_", "", page_key)
  
  # Get the sub-lists for the current page
  page_lda_models <- pages_lda_models[[page_key]]
  page_metadata <- pages_metadata[[page_key]]
  page_dtm <- pages_dtms[[page_key]]
  
  # Extract DTM for the current page
  dtm <- page_dtm[[1]]  # Assuming the DTM is the same for all models on a page, adjust if needed
  
  # Create a list to store data for all models in the page
  all_combined_data <- list()
  
  # Iterate over keys in the sub-list
  for (lda_model_key in names(page_lda_models)) {
    # Extract data from the sub-lists
    lda_model <- page_lda_models[[lda_model_key]]
    
    # Extract alpha and k values from metadata
    metadata <- page_metadata[[lda_model_key]]
    alpha_value <- metadata$alpha
    k_value <- metadata$k
    
    # Add alpha, k, and page name values as metadata
    metadata <- data.frame(alpha = alpha_value, k = k_value, page_name = page_name)
    
    # Store the combined data in the list
    all_combined_data[[lda_model_key]] <- list(metadata = metadata, lda_model = lda_model)
  }
  
  # Append the list for the current page to the main list
  pages_combined_data_list[[page_key]] <- list(dtm = dtm, models = all_combined_data, page_key = page_key)
}

names(pages_combined_data_list)

# Save the list containing all combined data
saveRDS(pages_combined_data_list, file = "Data_all_pages_data.rds")


# Creation of a testing set
set.seed(1716)  # Set seed for reproducibility

# Initialize lists to store results
test_samples <- list()
test_samples_dtm <- list()
tests <- list()

# Specify the size of the new testing set
new_test_sample_size <- 2000

# Identify unique pages in the dataset
unique_URL <- unique(posts$account_url)

# Initialize an empty data frame to store the sampled data
test_posts <- data.frame()

# Initialize a variable to keep track of the total sampled size
total_test_sampled_size <- 0

# Iterate over each page and perform stratified sampling
for (url in unique_URL) {
  # Subset data for the current page
  page_data <- subset(posts, account_url == url)
  
  # Determine the number of observations to sample from this page
  page_sample_size <- round(new_test_sample_size * nrow(page_data) / nrow(posts))
  
  # Randomly sample from the current page
  page_sample <- page_data[sample(nrow(page_data), size = page_sample_size, replace = FALSE), ]
  
  # Combine the sampled data from each page
  test_posts <- rbind(test_posts, page_sample)
  
  # Update the total sampled size
  total_test_sampled_size <- total_test_sampled_size + nrow(page_sample)
}

# Adjust the total sampled size to match the desired sample size
if (total_test_sampled_size > new_test_sample_size) {
  test_posts <- test_posts[sample(1:nrow(test_posts), size = new_test_sample_size, replace = FALSE), ]
}
gen_corpus_test <- Corpus(VectorSource(test_posts$p_text))
non_empty_corpus_test <- tm_filter(gen_corpus_test, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)

# Check if the document-term matrix is not empty
if (length(non_empty_corpus_test) > 0) {
  # Create a document-term matrix
  dtm_test <- DocumentTermMatrix(non_empty_corpus_test, control = list(minDocFreq = 5))
  
  # Filter out documents with all-zero counts
  dtm_test <- dtm_test[rowSums(as.matrix(dtm_test) > 0) > 0, ]
  
  # Store the dtm data in the list
  test_samples_dtm[["sample_dtm"]] <- dtm_test
}

# Store the sampled data in the list
tests[["sample_data"]] <- test_posts
test_samples[["sample_corpus"]] <- non_empty_corpus_test

# Create a list containing DTM and corpus
combined_data <- list(dtm = test_samples_dtm, corpus = test_samples, posts = tests)

# Save the list
saveRDS(combined_data, file = "Data_test_data.rds")

# Initialize lists to store results for pages testing set
pages_test_samples_dtm <- list()
pages_tests <- list()

# Specify the size of the new testing set for pages
pages_test_size <- 2000

# Identify unique pages in the dataset
unique_pages <- unique(posts$account_handle)

# Initialize a variable to keep track of the page key for pages
page_key_list <- character(0)

# Initialize list to store sampled data
pages_test_data <- list()

# Iterate over unique page names
for (current_page in unique_pages) {
  # Exclude the current page from the list of unique pages for sampling
  other_pages <- setdiff(unique_pages, current_page)
  
  # Initialize a variable to keep track of the sampled data
  sampled_data <- data.frame()
  
  # Iterate over other pages and perform stratified sampling
  for (other_page in other_pages) {
    # Subset data for the other page
    other_page_data <- subset(posts, account_handle == other_page)
    
    # Check if other_page_data is not empty
    if (nrow(other_page_data) > 0) {
      # Determine the number of observations to sample from this page
      page_sample_size <- round(pages_test_size * nrow(other_page_data) / nrow(posts))
      
      # Randomly sample from the other page
      page_sample <- other_page_data[sample(nrow(other_page_data), size = page_sample_size, replace = FALSE), ]
      
      # Combine the sampled data from each page
      sampled_data <- rbind(sampled_data, page_sample)
    }
  }
  cat("Sample finish for", current_page, "\n")
  
  # Adjust the total sampled size to match the desired sample size for pages
  if (nrow(sampled_data) > pages_test_size) {
    sampled_data <- sampled_data[sample(1:nrow(sampled_data), size = pages_test_size, replace = FALSE), ]
  }
  
  # Create a corpus for the pages testing set
  gen_corpus_pages_test <- Corpus(VectorSource(sampled_data$p_text))
  non_empty_corpus_pages_test <- tm_filter(gen_corpus_pages_test, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)
  cat("Creating dtm for", current_page, "\n")
  
  # Check if the document-term matrix is not empty for pages
  if (length(non_empty_corpus_pages_test) > 0) {
    # Create a document-term matrix for pages
    dtm_pages_test <- DocumentTermMatrix(non_empty_corpus_pages_test, control = list(minDocFreq = 5))
    
    # Filter out documents with all-zero counts for pages
    dtm_pages_test <- dtm_pages_test[rowSums(as.matrix(dtm_pages_test) > 0) > 0, ]
    
    # Define the page key
    page_key <- paste("page", gsub("[^A-Za-z0-9]", "_", current_page), sep = "_")
    
    # Add the page key to the list
    page_key_list <- c(page_key_list, page_key)
    
    # Save the DTM in the list
    pages_test_samples_dtm[[page_key]] <- dtm_pages_test
    
    # Save sampled data in the list
    pages_tests[[page_key]] <- sampled_data
    
    # Store sampled data and DTM in the list
    pages_test_data[[page_key]] <- list(sampled_data = sampled_data, dtm = dtm_pages_test, metadata = metadata)
  }
  
  cat(current_page, "done\n")
}

# Save the list for pages testing set
saveRDS(pages_test_data, file = "Data_pages_test_data.rds")







