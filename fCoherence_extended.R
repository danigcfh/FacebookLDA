library(slam)
library(textclean)
library(tm)
library(topicmodels)
library(SnowballC)


# Function to calculate NPMI
calculate_npmi <- function(dtm, top_terms) {
  # Convert DTM to a term-document matrix
  tdm <- TermDocumentMatrix(dtm)
  
  # Convert the term-document matrix to a matrix
  tdm_matrix <- as.matrix(tdm)
  
  # Calculate the co-occurrence matrix
  co_occurrence_matrix <- tdm_matrix %*% t(tdm_matrix)
  
  # Normalize the co-occurrence matrix using the slam package
  npmi_matrix <- slam::normalizeMatrix(co_occurrence_matrix, method = "max")
  
  # Extract the upper triangle (excluding the diagonal) for NPMI values
  npmi_values <- npmi_matrix[upper.tri(npmi_matrix, diag = FALSE)]
  
  # Return the mean NPMI value
  return(mean(npmi_values, na.rm = TRUE))
}

# Function to calculate U Mass Coherence
calculate_umass_coherence <- function(dtm, top_terms) {
  # Calculate U Mass Coherence based on PMI
  pmi_matrix <- textclean::pmi(dtm)
  pmi_values <- pmi_matrix[upper.tri(pmi_matrix, diag = FALSE)]
  umass_coherence <- sum(pmi_values) / length(pmi_values)
  
  return(umass_coherence)
}

# Function to calculate Rocchio's Document Similarity
calculate_rocchio_similarity <- function(lda_model, corpus) {
  # Extract document-topic matrix from the LDA model
  gamma <- lda_model@gamma
  
  # Calculate cosine similarity between documents
  similarity_matrix <- sim2(as.matrix(gamma), method = "cosine", norm = "l2")
  
  # Calculate average similarity
  avg_similarity <- mean(similarity_matrix)
  
  return(avg_similarity)
}

# Function to calculate coherence
calculate_coherence <- function(lda_model, dtm, corpus = NULL, coherence_metric = "pairwise", top_terms_count = 10, corlimit = 0.25) {
  # Extract the terms from the DTM
  terms <- colnames(as.matrix(dtm))
  
  # Calculate coherence for each topic
  topic_coherences <- numeric(lda_model@k)
  
  for (k in 1:lda_model@k) {
    # Extract the top terms for the current topic
    top_terms <- terms[order(lda_model@beta[, k], decreasing = TRUE)[1:top_terms_count]]  # Adjust the number of terms as needed
    
    # Calculate coherence based on the chosen metric
    if (coherence_metric == "pairwise") {
      # Pairwise co-occurrence
      co_occurrence <- findAssocs(dtm, terms = top_terms, corlimit = corlimit)  # Adjust the correlation limit as needed
      coherence <- sum(unlist(co_occurrence)) / choose(length(top_terms), 2)
    } else if (coherence_metric == "npmi") {
      # NPMI (Normalized Pointwise Mutual Information)
      coherence <- calculate_npmi(dtm, top_terms)
    } else if (coherence_metric == "umass") {
      # U Mass Coherence
      umass_coherence <- calculate_umass_coherence(dtm, top_terms)
      coherence <- umass_coherence
    } else if (coherence_metric == "rocchio") {
      # Rocchio's Document Similarity
      rocchio_similarity <- calculate_rocchio_similarity(lda_model, corpus)
      coherence <- rocchio_similarity
    } else {
      stop("Invalid coherence metric. Supported metrics: 'pairwise', 'npmi', 'umass', 'rocchio'")
    }
    
    topic_coherences[k] <- coherence
  }
  
  # Return the average coherence across topics
  return(mean(topic_coherences))
}
