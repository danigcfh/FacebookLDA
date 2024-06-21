calculate_coherence <- function(lda_model, dtm) {
  # Extract the terms from the DTM
  terms <- colnames(as.matrix(dtm))
  
  # Get the topic-term matrix from the LDA model
  beta <- lda_model@beta
  
  # Calculate coherence for each topic
  topic_coherences <- numeric(lda_model@k)
  
  for (k in 1:lda_model@k) {
    # Extract the top terms for the current topic
    top_terms <- terms[order(beta[, k], decreasing = TRUE)[1:10]]  # Adjust the number of terms as needed
    
    # Calculate pairwise co-occurrence within the DTM
    co_occurrence <- findAssocs(dtm, terms = top_terms, corlimit = 0.25)  # Adjust the correlation limit as needed
    
    # Calculate coherence
    coherence <- sum(unlist(co_occurrence))/choose(length(top_terms), 2)
    
    topic_coherences[k] <- coherence
  }
  
  # Return the average coherence across topics
  return(mean(topic_coherences))
}






