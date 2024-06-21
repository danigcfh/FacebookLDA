# Load necessary libraries
library(ggplot2)

# Load evaluation results for pages
pages_evaluation_results <- readRDS("pages_evaluation_results.rds")
View(pages_results_df)
# Convert evaluation results to a data frame with overall scores
# Convert evaluation results to a data frame with overall scores
pages_results_df <- do.call(rbind, lapply(names(pages_evaluation_results), function(key) {
  data <- pages_evaluation_results[[key]]
  overall_coherence <- mean(c(data$internal_coherence, data$external_coherence), na.rm = TRUE)
  overall_perplexity <- mean(c(data$training_perplexity, data$test_perplexity), na.rm = TRUE)
  combined_metric <- 0.5 * overall_coherence + 0.5 / overall_perplexity  # Adjust weights as needed
  
  data.frame(page = data$page,
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




# Function to create a heatmap for pages
# Function to create a heatmap for pages with selective y-axis labels
create_heatmap <- function(data, title, value_column, num_labels = 5) {
  # Convert value_column to numeric (if it's not already)
  if (!is.numeric(data[[value_column]])) {
    data[[value_column]] <- as.numeric(data[[value_column]])
  }
  
  top_data <- data[order(data[[value_column]], decreasing = TRUE), ]
  bottom_data <- data[order(data[[value_column]]), ]
  
  # Extract the y labels for the top or bottom results
  selected_pages <- c(bottom_data$page[1:num_labels], top_data$page[1:num_labels])
  
  ggplot(data, aes(x = factor(k), y = page, fill = log(.data[[value_column]]))) +
    geom_tile() +
    scale_fill_viridis_c(name = value_column) +
    labs(title = title,
         x = "Number of Topics (k)",
         y = NULL,  # Set y = NULL to remove y-axis labels
         fill = value_column) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust the number of columns as needed
    facet_wrap(~ factor(alpha), scales = "free_y", ncol = 2) +  
    scale_y_discrete(labels = function(x) ifelse(x %in% selected_pages, x, ""))
}

# Example usage:
# Plot overall coherence heatmap 
create_heatmap(pages_results_df, "Overall Coherence Heatmap for Pages", "overall_coherence", num_labels = 7)

# Plot overall perplexity heatmap 
create_heatmap(pages_results_df, "Overall Perplexity Heatmap for Pages", "overall_perplexity", num_labels = 7)

# Plot overall combined metric heatmap 
create_heatmap(pages_results_df, "Overall Combined Metric Heatmap for Pages", "combined_metric", num_labels = 7)
