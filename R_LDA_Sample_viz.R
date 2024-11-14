# Load necessary libraries
library(ggplot2)

# Load evaluation results
evaluation_results <- readRDS("Data_evaluation_results.rds")

# Convert evaluation results to a data frame with overall scores
results_df <- do.call(rbind, lapply(names(evaluation_results), function(key) {
  data <- evaluation_results[[key]]
  overall_coherence <- mean(c(data$internal_coherence, data$external_coherence), na.rm = TRUE)
  overall_perplexity <- mean(c(data$training_perplexity, data$test_perplexity), na.rm = TRUE)
  combined_metric <- 0.5 * overall_coherence + 0.5 / overall_perplexity  # Adjust weights as needed
  
  data.frame(sample = data$sample,
             k = data$k,
             alpha = data$alpha,
             internal_coherence = data$internal_coherence,
             training_perplexity = log(data$training_perplexity),
             external_coherence = data$external_coherence,
             test_perplexity = log(data$test_perplexity),
             overall_coherence = overall_coherence,
             overall_perplexity = log(overall_perplexity),
             combined_metric = combined_metric)
}))

# Function to create a heatmap
create_heatmap <- function(data, title, value_column) {
  ggplot(data, aes(x = factor(k), y = sample, fill = log(.data[[value_column]]))) +
    geom_tile() +
    scale_fill_viridis_c(name = value_column) +
    labs(title = title,
         x = "Number of Topics (k)",
         y = "Sample",
         fill = value_column) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ factor(alpha), scales = "free_y", ncol = 2) 
  }

# Example usage:
# Plot overall coherence heatmap 
create_heatmap(results_df, "Overall Coherence Heatmap", "overall_coherence")

# Plot overall perplexity heatmap 
create_heatmap(results_df, "Overall Perplexity Heatmap", "overall_perplexity")

# Plot overall combined metric heatmap 
create_heatmap(results_df, "Overall Combined Metric Heatmap", "combined_metric")


top_models <- results_df[order(-results_df$combined_metric), ][1:10, ]
print(top_models)


