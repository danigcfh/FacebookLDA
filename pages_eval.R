# Load necessary libraries
library(tm)
library(topicmodels)
library(ldatuning)

# Load the training data
all_pages_data_list <- readRDS("all_pages_data.rds")

# Load the testing data for pages (replace with your actual testing data)
pages_test_data <- readRDS("pages_test_data.rds")

# Function to calculate coherence
calculate_coherence <- function(lda_model, dtm) {
  terms <- colnames(as.matrix(dtm))
  beta <- as.matrix(lda_model@beta)
  topic_coherences <- numeric(lda_model@k)
  
  if (inherits(dtm, "DocumentTermMatrix")) {
    for (k in 1:lda_model@k) {
      top_terms <- terms[order(beta[, k], decreasing = TRUE)[1:10]]
      co_occurrence <- findAssocs(dtm, terms = top_terms, corlimit = 0.25)
      coherence <- sum(unlist(co_occurrence))/choose(length(top_terms), 2)
      topic_coherences[k] <- coherence
    }
    return(mean(topic_coherences))
  } else {
    warning("Invalid DTM structure. Skipping coherence calculation.\n")
    return(NULL)
  }
}

# Function to calculate perplexity
calculate_perplexity <- function(lda_model, dtm) {
  log_likelihood <- logLik(lda_model, dtm)
  num_tokens <- sum(as.matrix(dtm))
  perplexity <- exp(-log_likelihood/num_tokens)
  return(perplexity)
}

# Function to calculate coherence between topics
calculate_inter_topic_coherence <- function(lda_model, dtm1, dtm2) {
  terms <- colnames(as.matrix(dtm1))
  beta1 <- as.matrix(lda_model@beta)
  beta2 <- as.matrix(lda_model@beta)
  
  inter_topic_coherences <- numeric(lda_model@k)
  
  for (k in 1:lda_model@k) {
    top_terms1 <- terms[order(beta1[, k], decreasing = TRUE)[1:10]]
    top_terms2 <- terms[order(beta2[, k], decreasing = TRUE)[1:10]]
    
    co_occurrence <- findAssocs(dtm1, terms = top_terms2, corlimit = 0.25)
    coherence <- sum(unlist(co_occurrence))/choose(length(top_terms1), 2)
    
    inter_topic_coherences[k] <- coherence
  }
  
  return(mean(inter_topic_coherences))
}

# Initialize a list to store evaluation results
pages_evaluation_results <- list()

# Iterate over each page
for (page_key in names(all_pages_data_list)) {
  
  # Extract data for the current page
  page_data <- all_pages_data_list[[page_key]]
  dtm <- page_data$dtm
  lda_models <- page_data$models
  
  cat("Processing page:", page_key, "\n")
  
  # Check if lda_models is not NULL
  if (!is.null(lda_models)) {
    cat("  LDA Models for", page_key, ": not NULL\n")
  } else {
    cat("  LDA Models for", page_key, ": NULL\n")
    next  # Skip to the next iteration
  }
  
  # Iterate over each LDA model for the page
  for (lda_model_index in seq_along(lda_models)) {
    lda_model_data <- lda_models[[lda_model_index]]
    lda_model <- lda_model_data$lda_model
    cat("    Processing LDA Model:", ifelse(is.null(lda_model), "NULL", "not NULL"), "\n")
    
    # Check if lda_model is present and not empty
    if (!is.null(lda_model) && length(lda_model) > 0) {
      
      # Check if lda_model is an S4 class and is either LDA or LDA_VEM
      if (inherits(lda_model, "LDA") || inherits(lda_model, "LDA_VEM")) {
        
        # Check if the lda_model has the necessary slots
        if ("beta" %in% slotNames(lda_model) && "k" %in% slotNames(lda_model)) {
          
          # Check if dtm is present and not empty
          if (!is.null(dtm) && length(dtm) > 0) {
            
            # Print some diagnostic information
            cat("      DTM Dimensions:", dim(dtm), "\n")
            
            # Calculate internal coherence score
            cat(" calculating internal coherence score and perplexity", "\n")
            internal_coherence_score <- calculate_coherence(lda_model, dtm)
            
            # Calculate perplexity on the training data
            training_perplexity <- calculate_perplexity(lda_model, dtm)
            
            cat(" calculating external coherence score and perplexity", "\n")
            
            # Assuming you have separate testing data for each page
            # Replace this with the actual testing data for the page
            test_data <- pages_test_data[[page_key]]
            test_dtm <- test_data$dtm
            
            # Check if test_dtm is present and not empty
            if (!is.null(test_dtm) && length(test_dtm) > 0) {
              # Calculate perplexity on the testing data
              test_perplexity <- calculate_perplexity(lda_model, test_dtm)
              
              # Calculate external coherence score
              external_coherence_score <- calculate_coherence(lda_model, test_dtm)
              
              # Calculate inter-topic coherence score
              inter_topic_coherence_score <- calculate_inter_topic_coherence(lda_model, dtm, test_dtm)
              
              # Save the evaluation results
              result_key <- paste(page_key, "k", lda_model_data$metadata$k, "alpha", lda_model_data$metadata$alpha, sep = "_")
              pages_evaluation_results[[result_key]] <- list(
                internal_coherence = internal_coherence_score,
                training_perplexity = training_perplexity,
                external_coherence = external_coherence_score,
                test_perplexity = test_perplexity,
                inter_topic_coherence = inter_topic_coherence_score,
                alpha = lda_model_data$metadata$alpha,
                k = lda_model_data$metadata$k,
                page = page_key
              )
              
              cat("      Internal Coherence:", internal_coherence_score, "| Training Perplexity:", training_perplexity, "| External Coherence:", external_coherence_score, "| Test Perplexity:", test_perplexity, "| Inter Topic Coherence:", inter_topic_coherence_score, "\n")
            } else {
              warning("      Test DTM is missing or empty. Skipping.\n")
            }
            
          } else {
            warning("      DTM is missing or empty. Skipping.\n")
          }
          
        } else {
          warning("      Invalid LDA model structure. Skipping.\n")
        }
        
      } else {
        warning("      Invalid LDA model class. Skipping.\n")
      }
    } else {
      warning("      LDA Model is missing or empty. Skipping.\n")
    }
  }       
}

# Save evaluation results for pages
saveRDS(pages_evaluation_results, "pages_evaluation_results.rds")


# ... (previous code)

# Function for min-max normalization
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Initialize a list to store normalized evaluation results
normalized_pages_results <- list()

# Iterate over each result in evaluation_results
for (result_key in names(pages_evaluation_results)) {
  result <- pages_evaluation_results[[result_key]]
  
  # Normalize each metric
  normalized_internal_coherence <- min_max_normalize(result$internal_coherence)
  normalized_inter_topic_coherence <- min_max_normalize(result$inter_topic_coherence)
  normalized_training_perplexity <- min_max_normalize(result$training_perplexity)
  normalized_external_coherence <- min_max_normalize(result$external_coherence)
  normalized_test_perplexity <- min_max_normalize(result$test_perplexity)
  
  # Save the normalized metrics
  normalized_pages_results[[result_key]] <- list(
    internal_coherence = normalized_internal_coherence,
    inter_topic_coherence = normalized_inter_topic_coherence,
    training_perplexity = normalized_training_perplexity,
    external_coherence = normalized_external_coherence,
    test_perplexity = normalized_test_perplexity,
    alpha = result$alpha,
    k = result$k,
    page = result$page
  )
}

# Save normalized evaluation results
saveRDS(normalized_pages_results, "normalized_pages_results.rds")

