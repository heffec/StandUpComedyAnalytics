library(ggplot2)
library(SentimentAnalysis)
library(tm)
library(NLP)
library(gutenbergr)
library(dplyr)
library(quanteda)
library(readr)
library(wordcloud)

fileNames <- list()
allData <- list()
wordDict <- c()
cuss <- "\\[ __ \\]"
soundEffectsAll <- c("[applause]"=0, "[music]"=0, "[laughter]"=0, "[cuss]"=0)

folder <- "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject"
# Get a list of CSV files in the directory
csv_files <- list.files(
  path = "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject",
  pattern = "\\.csv$",
  full.names = TRUE, 
)
csv_files <- setdiff(csv_files, "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject/ComicsList.csv")
# ----- Data Preprocessing ----- #

# Iterate over each CSV file
for (file_path in csv_files) {
  print(file_path)
  
  soundEffects <- c("[applause]"=0, "[music]"=0, "[laughter]"=0, "[cuss]"=0)
  
  
  # Read and clean CSV text
  data <- read.csv(file_path)
  data$text <- tolower(data$text)
  data <- na.omit(data, cols = "text")
  
  # Clean up file names to extract the specific dataset name
  split_fileName <- strsplit(file_path, '/')
  elements <- unlist(split_fileName)
  last = length(elements)
  modified_string <- substr(elements[last], 1, nchar(elements[last]) - 4)

  
  # --- Audience Behavior ---
  # This will record laughter, applause, and music from the set
  # Saved on an individual basis and in aggregate
  explicitUTFCode <- c(91,160,95,95,160,93) # [ __ ] expressed as explicit
  for (line in data$text) {
    for (words in line) {
      for (words in strsplit(words, " ")){
        for (word in words) {
          if (word == "[applause]") {
            soundEffects[word] = soundEffects[word] + 1
            soundEffectsAll[word] = soundEffectsAll[word] + 1
          }
          # music is only present in the beginning and end of a set
          # maybe once in between. Visual inspection of these sets proved that
          # music is often confused with laughter in YouTubes captioning system
          else if (word == "[music]") {
            if (!is.na(soundEffects["music"]) &&soundEffects["music"] > 3) {
              soundEffectsAll["[laughter]"] <- soundEffectsAll["[laughter]"] + 1
            }
          }
          else if (word == "[laughter]") {
            soundEffects[word] = soundEffects[word] + 1
            soundEffectsAll[word] = soundEffectsAll[word] + 1
            
          }
          else {
            utf_codes <- utf8ToInt(word)
            if (length(utf_codes) == 6 && all(utf_codes == explicitUTFCode)) {
              soundEffects["[cuss]"] <- soundEffects["[cuss]"] + 1
              soundEffectsAll["[cuss]"] <- soundEffectsAll["[cuss]"] + 1
            }
          }
        }
      }
    }
  }
  
  print(soundEffects["[applause]"])
  print(soundEffects["[laughter]"])
  print(soundEffects["[cuss]"])
  print("")
  
  data$text <- gsub("[[:punct:]]", "", data$text)
  data$text <- removeWords(data$text, stopwords("english"))
  wordDict <- c(wordDict, data$text)
  
  # --- Sentiment Analysis --- #
  hist_path <- paste0(folder, modified_string, "_hist.png")
  # Obtain SentimentGI scores
  sentiment <- analyzeSentiment(data$text)
  sentimentGI_scores <- sentiment$SentimentGI
  # Determine breaks for the histogram
  breaks <- "sturges"
  # Set the width and height of the plotting device to make the image smaller
  png(file = hist_path, width = 800, height = 600)
  # Create a histogram using the SentimentGI scores
  h <- hist(sentimentGI_scores, 
            breaks = breaks, 
            main = "SentimentGI Histogram", 
            xlab = "Sentiment Score",
            ylab = "Frequency")
  
  # Customize x-axis ticks and labels
  axis(side = 1, at = pretty(sentimentGI_scores, n = 10), labels = pretty(sentimentGI_scores, n = 10))
  # Adjust text labels to be smaller and inside the bars
  text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5), cex = 0.7)
  # Close the plotting device
  dev.off()
}

allDtm <- DocumentTermMatrix(Corpus(VectorSource(wordDict)))
# Convert the document-term matrix to a data frame
allDtm_df <- as.data.frame(as.matrix(allDtm))

# Visualize word frequencies
word_freq_all <- colSums(allDtm_df)
word_freq_all <- sort(word_freq_all, decreasing = TRUE)

barplot(word_freq_all[1:5], col = "skyblue", main = "Word Frequencies Without Stop Words")
all_wordFreq_path = paste0(folder, "/all_wordFreq.png")
ggsave(all_wordFreq_path, plot = last_plot(), units = "px", device = "png", width = 800, height = 600)


# --- Overall Sentiment Analysis --- #
hist_path <- paste0(folder, "/all_hist.png")
# Obtain SentimentGI scores
sentiment <- analyzeSentiment(wordDict)
sentimentGI_scores <- sentiment$SentimentGI
# Determine breaks for the histogram
breaks <- "sturges"
# Set the width and height of the plotting device to make the image smaller
png(file = hist_path, width = 800, height = 600)
# Create a histogram using the SentimentGI scores
h <- hist(sentimentGI_scores, 
          breaks = breaks, 
          main = "SentimentGI Histogram", 
          xlab = "Sentiment Score",
          ylab = "Frequency")

# Customize x-axis ticks and labels
axis(side = 1, at = pretty(sentimentGI_scores, n = 10), labels = pretty(sentimentGI_scores, n = 10))
# Adjust text labels to be smaller and inside the bars
text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5), cex = 0.7)
# Close the plotting device
dev.off()

# ---- Topic Modeling --- #
allDtm <- allDtm[rowSums(as.matrix(allDtm)) > 0, ]
lda <- LDA(allDtm, method = "Gibbs", k=10)
terms_df <- as.data.frame(terms(lda, 10))

# Save the list of data frames for further analysis if needed
save(allData, file = "allData.RData")

as.data.frame(wordDict)

