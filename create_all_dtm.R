
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

posts <- read_csv("Copia de posts (1).csv")
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

# Create a corpus for the page's posts
corpus <- Corpus(VectorSource(posts$p_text))

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

saveRDS(dtm, file = "all_dtm.rds")
