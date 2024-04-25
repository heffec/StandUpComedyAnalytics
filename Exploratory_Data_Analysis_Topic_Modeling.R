library(quanteda)
library(topicmodels)
library(tm)

folder <- "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject"
# Get a list of CSV files in the directory
csv_files <- list.files(
  path = "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject",
  pattern = "\\.csv$",
  full.names = TRUE, 
)
csv_files <- setdiff(csv_files, "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject/ComicsList.csv")
allText <- c()
# Iterate over each CSV file
for (file_path in csv_files) {
  data <- read.csv(file_path)
  data$text <- tolower(data$text)
  data <- na.omit(data, cols = "text")
  
  allText <- c(allText, data$text)
}

# Create a Corpus from the text data
my_corpus <- Corpus(VectorSource(allText))

# Preprocess the corpus (e.g., convert to lowercase, remove punctuation)
my_corpus <- tm_map(my_corpus, content_transformer(tolower))
my_corpus <- tm_map(my_corpus, removePunctuation)

# Create a Document-Term Matrix (DTM)
my_dtm <- DocumentTermMatrix(my_corpus)

# Convert DTM to a data frame for better readability
my_dtm_df <- as.data.frame(as.matrix(my_dtm))

# Print the DTM data frame
print(my_dtm_df)



