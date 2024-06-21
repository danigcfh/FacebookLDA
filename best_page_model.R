# Load the required libraries
library(topicmodels)
library(ggplot2)
library(cluster)
library(factoextra)

# Load evaluation results
pages_results <- readRDS("pages_evaluation_results.rds")
str(pages_results)

# Convert evaluation results to a data frame with overall scores
metrics_df <- do.call(rbind, lapply(names(pages_results), function(key) {
  data <- pages_results[[key]]
  
  # Replace NaN with NA
  internal_coherence <- ifelse(is.nan(data$internal_coherence), NA, data$internal_coherence)
  external_coherence <- ifelse(is.nan(data$external_coherence), NA, data$external_coherence)
  training_perplexity <- ifelse(is.nan(data$training_perplexity), NA, log(data$training_perplexity))
  test_perplexity <- ifelse(is.nan(data$test_perplexity), NA, log(data$test_perplexity))
  
  # Calculate overall coherence and overall perplexity
  overall_coherence <- mean(c(internal_coherence, external_coherence), na.rm = TRUE)
  overall_perplexity <- mean(c(training_perplexity, test_perplexity), na.rm = TRUE)
  
  # Avoid division by zero in combined_metric calculation
  if (overall_perplexity == 0) overall_perplexity <- NA
  
  # Calculate combined metric
  combined_metric <- 0.5 * overall_coherence + 0.5 / overall_perplexity
  
  data.frame(
    page = data$page,
    k = data$k,
    alpha = data$alpha,
    internal_coherence = internal_coherence,
    training_perplexity = training_perplexity,
    external_coherence = external_coherence,
    test_perplexity = test_perplexity,
    overall_coherence = overall_coherence,
    overall_perplexity = overall_perplexity,
    combined_metric = combined_metric,
    model_key = key
  )
}))

# Identify the best model based on combined score
best_model_key <- metrics_df$model_key[which.max(metrics_df$combined_metric)]
best_model_key

pages_data <- readRDS("all_pages_data.rds")
best_page_LDA <- pages_data$page_notevotoconabortoeideologiadegenero$models$`k 7 _alpha 1`$lda_model

