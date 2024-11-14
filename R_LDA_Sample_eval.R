# Load necessary libraries
library(tm)
library(topicmodels)
library(ldatuning)

# Load the training data
all_combined_data_list <- readRDS("Data_all_training_data.rds")

# Load the testing data
test_data <- readRDS("Data_test_data.rds")

# Extract the testing set
test_samples_dtm <- test_data$dtm
test_samples <- test_data$corpus
tests <- test_data$posts

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
evaluation_results <- list()

# Iterate over each sample
for (sample_key in names(all_combined_data_list)) {
  
  if (!(sample_key %in% names(all_combined_data_list))) {
    cat("Sample key", sample_key, "not found. Skipping.\n")
    next
  }
  
  dtm <- all_combined_data_list[[sample_key]]$dtm_data
  lda_models <- all_combined_data_list[[sample_key]][1:9]
  
  cat("Processing sample:", sample_key, "\n")
  
  if (!is.null(lda_models)) {
    cat("  LDA Models for", sample_key, ": not NULL\n")
  } else {
    cat("  LDA Models for", sample_key, ": NULL\n")
    next
  }
  
  for (lda_model_index in seq_along(lda_models)) {
    lda_model_data <- lda_models[[lda_model_index]]
    lda_model <- lda_model_data$lda_model
    cat("    Processing LDA Model:", ifelse(is.null(lda_model), "NULL", "not NULL"), "\n")
    
    if (!is.null(lda_model) && length(lda_model) > 0) {
      
      if (inherits(lda_model, "LDA") || inherits(lda_model, "LDA_VEM")) {
        
        if ("beta" %in% slotNames(lda_model) && "k" %in% slotNames(lda_model)) {
          
          if (!is.null(dtm) && length(dtm) > 0) {
            cat("      DTM Dimensions:", dim(dtm), "\n")
            
            cat(" calculating internal coherence, inter topic coherence, and perplexity", "\n")
            internal_coherence_score <- calculate_coherence(lda_model, dtm)
            inter_topic_coherence_score <- calculate_inter_topic_coherence(lda_model, dtm, dtm)
            training_perplexity <- calculate_perplexity(lda_model, dtm)
            
            test_dtm <- test_samples_dtm$sample_dtm
            test_perplexity <- calculate_perplexity(lda_model, test_dtm)
            external_coherence_score <- calculate_coherence(lda_model, test_dtm)
            
            result_key <- paste(sample_key, "k", lda_model_data$metadata$k, "alpha", alpha = lda_model_data$metadata$alpha, sep = "_")
            
            evaluation_results[[result_key]] <- list(
              internal_coherence = internal_coherence_score,
              inter_topic_coherence = inter_topic_coherence_score,
              training_perplexity = training_perplexity,
              external_coherence = external_coherence_score,
              test_perplexity = test_perplexity,
              alpha = lda_model_data$metadata$alpha,
              k = lda_model_data$metadata$k,
              sample = sample_key
            )
            
            cat("      Internal Coherence:", internal_coherence_score, "| Inter Topic Coherence:", inter_topic_coherence_score, "| Training Perplexity:", training_perplexity, "| External Coherence:", external_coherence_score, "| Test Perplexity:", test_perplexity, "\n")
            
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

# Save evaluation results
saveRDS(evaluation_results, "Data_evaluation_results.rds")



# Function for min-max normalization
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Initialize a list to store normalized evaluation results
normalized_evaluation_results <- list()

# Iterate over each result in evaluation_results
for (result_key in names(evaluation_results)) {
  result <- evaluation_results[[result_key]]
  
  # Normalize each metric
  normalized_internal_coherence <- min_max_normalize(result$internal_coherence)
  normalized_inter_topic_coherence <- min_max_normalize(result$inter_topic_coherence)
  normalized_training_perplexity <- min_max_normalize(result$training_perplexity)
  normalized_external_coherence <- min_max_normalize(result$external_coherence)
  normalized_test_perplexity <- min_max_normalize(result$test_perplexity)
  
  # Save the normalized metrics
  normalized_evaluation_results[[result_key]] <- list(
    internal_coherence = normalized_internal_coherence,
    inter_topic_coherence = normalized_inter_topic_coherence,
    training_perplexity = normalized_training_perplexity,
    external_coherence = normalized_external_coherence,
    test_perplexity = normalized_test_perplexity,
    alpha = result$alpha,
    k = result$k,
    sample = result$sample
  )
}

# Save normalized evaluation results
saveRDS(normalized_evaluation_results, "Data_normalized_evaluation_results.rds")
