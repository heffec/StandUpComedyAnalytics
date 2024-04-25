library(topicmodels)
library(tm)
library(dendextend)
library(dplyr)
library(cluster)

explicitUTFCode <- c(91, 160, 95, 95, 160, 93) # [ __ ] expressed as explicit
folder <- "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject"

# Function to clean text
clean_text <- function(text) {
  # Remove profanity placeholders
  text <- gsub(paste0("\\", paste(explicitUTFCode, collapse = "\\s*")), NA, text, fixed = TRUE)
  
  # Remove audience behavior annotations
  text <- gsub("\\[laughter\\]", NA, text)
  text <- gsub("\\[applause\\]", NA, text)
  text <- gsub("\\[music\\]", NA, text)
  
  # Convert to lowercase
  text <- tolower(text)
  
  return(text)
}

perform_LDA <- function(text, num_topics = 5) {
  dtm <- DocumentTermMatrix(Corpus(VectorSource(text)))
  dtm_matrix <- as.matrix(dtm)
  dtmCleaned <- dtm_matrix[rowSums(dtm_matrix) > 0, ]
  
  if (nrow(dtmCleaned) == 0) {
    warning("Empty document-term matrix after cleaning. Skipping LDA.")
    return(NULL)
  }
  
  lda_model <- LDA(dtmCleaned, k = num_topics)
  return(lda_model)
}

extract_topics <- function(lda_model, threshold = 0.1) {
  topic_terms <- terms(lda_model, 10)
  terms_df <- as.data.frame(topic_terms)
  return(terms_df)
}

allTopics <- list()

# Iterate over each CSV file
for (file_path in csv_files) {
  print(file_path)
  
  data <- read.csv(file_path)
  data$text <- clean_text(data$text)
  data <- na.omit(data, cols = "text")
  data$text <- removeWords(data$text, stopwords("english"))
  data$text <- gsub("[[:punct:]]", "", data$text)
  data <- na.omit(data, cols = "text")
  
  individualLDA <- perform_LDA(data$text)
  
  # Check if LDA was successful
  if (!is.null(individualLDA)) {
    extractedTopics <- extract_topics(individualLDA)
    allTopics[[file_path]] <- individualLDA
    
    # Transpose the matrix so that rows represent topics
    topic_matrix_transposed <- t(as.data.frame(individualLDA@beta))
    
    # Calculate distances
    dist_matrix <- dist(topic_matrix_transposed)
    
    # Perform hierarchical clustering
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    print(summary(hclust_result))
    
    # Assign cluster labels to topics
    cluster_labels <- cutree(hclust_result, k = 10)
    
    # Set the output PNG file path
    file_name <- basename(file_path)
    png_file_path <- file.path(folder, paste0(file_name, "_dendrogram", ".png"))
    
    # Open PNG file for plotting
    png(png_file_path)
    
    # Plot the dendrogram for each document
    dendrogram_ind <- as.dendrogram(hclust_result)
    dendrogram_ind <- color_branches(dendrogram_ind, k = length(unique(cluster_labels)))
    plot(dendrogram_ind, main = "Clustering Dendrogram", horiz = TRUE)
    dev.off()
  }
}


# Combine topic matrices of all documents
all_topic_matrices <- lapply(allTopics, function(lda_model) as.data.frame(lda_model@beta))
combined_topic_matrix <- dplyr::bind_rows(all_topic_matrices)
combined_topic_matrix_transposed <- t(combined_topic_matrix)
dist_matrix <- dist(combined_topic_matrix_transposed)
hclust_result_all <- hclust(dist_matrix, method = "ward.D2")
cluster_labels_all <- cutree(hclust_result_all, k = 10)

# Plot the dendrogram for all documents
dendrogram_all <- as.dendrogram(hclust_result_all)
dendrogram_all <- color_branches(dendrogram_all, k = length(unique(cluster_labels_all)))

# Plot the combined dendrogram
png_file_path <- file.path(folder, paste0("all_dendrogram", ".png"))
png(png_file_path)
plot(dendrogram_all, main = "Aggregate Clustering Dendrogram", horiz = TRUE)
dev.off()



