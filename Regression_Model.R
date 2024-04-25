library(SentimentAnalysis)
library(ggplot2)

sets_with_laughter <- list()
sets_with_applause <- list()

# Iterate over each CSV file
for (file_path in csv_files) {
  
  print(file_path)
  
  soundEffects <- c("[applause]"=0, "[music]"=0, "[laughter]"=0, "[cuss]"=0)
  
  # Read and clean CSV text
  data <- read.csv(file_path)
  data$text <- tolower(data$text)
  explicitUTFCode <- c(91, 160, 95, 95, 160, 93) # [ __ ] expressed as explicit
  data$text <- gsub(paste0("\\s*", paste(explicitUTFCode, collapse = "\\s*")), "", data$text, fixed = TRUE)
  data$text <- removeWords(data$text, stopwords("english"))
  data$text <- na.omit(data$text, cols = "text")

  # Find indices where text is equal to "\\[laughter\\]"
  laughter_indices <- grep("\\[laughter\\]", data$text)
  applause_indices <- grep("\\[applause\\]", data$text)

  data$text <- gsub("[[:punct:]]", "", data$text)
  
  # Check if there are more than 1 laughter indices
  if (length(laughter_indices) > 1) {
    prev <- 1  # Start from the first index
    avg_sentiments <- numeric()  # Vector to store average sentiments
    laugh_times <- numeric()  # Vector to store laugh times
    
    for (index in laughter_indices) {
      joke <- data$text[prev:(index - 1)]
      sentiment <- analyzeSentiment(joke)
      sentimentGI_scores <- sentiment$SentimentGI
      sentimentGI_scores <- na.omit(sentimentGI_scores)
      avg_sentiment <- mean(sentimentGI_scores)
      avg_sentiments <- c(avg_sentiments, avg_sentiment)
      
      laugh_time <- data$duration[index]
      laugh_times <- c(laugh_times, laugh_time)
      prev <- index + 1
    }
    
    # Create a data frame for each set with laughter
    set_data <- data.frame(LaughTime = laugh_times, AvgSentiment = avg_sentiments)
    sets_with_laughter[[file_path]] <- set_data
  }
  
  # Check if there are more than 1 applause indices
  if (length(applause_indices) > 1) {
    prev <- 1  # Start from the first index
    avg_sentiments <- numeric()  # Vector to store average sentiments
    applause_times <- numeric()  # Vector to store laugh times
    
    for (index in applause_indices) {
      joke <- data$text[prev:(index - 1)]
      sentiment <- analyzeSentiment(joke)
      sentimentGI_scores <- sentiment$SentimentGI
      sentimentGI_scores <- na.omit(sentimentGI_scores)
      avg_sentiment <- mean(sentimentGI_scores)
      avg_sentiments <- c(avg_sentiments, avg_sentiment)
      
      applause_time <- data$duration[index]
      applause_times <- c(applause_times, applause_time)
      # Check if sentiment analysis result is valid
      prev <- index + 1
    }
    
    # Create a data frame for each set with applause
    set_data <- data.frame(ApplauseTime = applause_times, AvgSentiment = avg_sentiments)
    sets_with_applause[[file_path]] <- set_data
  }
}

combined_data <- bind_rows(sets_with_laughter)
combined_data_applause <- bind_rows(sets_with_applause)

# Fit linear regression model - laugh
lm_model_laugh <- lm(AvgSentiment ~ LaughTime, data = combined_data)
print(summary(lm_model_laugh))
laugh_graph <- ggplot(combined_data, aes(x = LaughTime, y = AvgSentiment, color = "orange")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Laugh Time vs. Average Sentiment")

# Fit linear regression model - applause
lm_model_applause <- lm(AvgSentiment ~ ApplauseTime, data = combined_data_applause)
print(summary(lm_model_applause))
app_graph <- ggplot(combined_data_applause, aes(x = ApplauseTime, y = AvgSentiment, color = "magenta")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Applause Time vs. Average Sentiment")

path_applause <- paste0(folder, "/all_applauseRegression.png")
ggsave(path_applause, app_graph, width = 2400, height = 1200, units = "px")


