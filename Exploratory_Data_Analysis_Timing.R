library(ggplot2)

# List to store individual plots
individual_plots <- list()

csv_files <- list.files(
  path = "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject",
  pattern = "\\.csv$",
  full.names = TRUE, 
)
csv_files <- setdiff(csv_files, "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject/ComicsList.csv")

# List to store spline points and outlier points for each file
all_spline_points <- list()
outlier_indices_list <- list()
folder = "C:/Users/heffec/Desktop/Semester 8/Data Analytics/FinalProject"

for (file_path in csv_files) {
  print(file_path)
  
  # Read and clean CSV text
  df <- read.csv(file_path)
  df$text <- tolower(df$text)
  
  # Check for missing values
  if (any(is.na(df$start)) || any(is.na(df$speech_rate))) {
    next()  # Skip to the next iteration
  }
  
  # Calculate speech rate (words per second)
  df$word_count <- sapply(strsplit(df$text, "\\s+"), length)
  df$speech_rate <- df$word_count / df$duration
  
  # Cubic spline interpolation for the cleaned data
  spline_points <- smooth.spline(df$start, df$speech_rate)
  spline_df <- data.frame(x = spline_points$x, y = spline_points$y)
  all_spline_points[[file_path]] <- spline_df
  
  # Time series plot with cubic spline interpolation and LOESS smoothing
  plot <- ggplot(df, aes(x = start, y = speech_rate)) +
    geom_line() +
    geom_line(data = spline_df, aes(x, y), color = "green") +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    labs(x = "Start Time", y = "Speech Rate (words per second)", title = "Speech Rate Over Time with Cubic Spline Interpolation and LOESS Smoothing") +
    scale_color_manual(values = c("Actual Data", "Cubic Spline Interpolation", "Locally Estimated Scatterplot Smoothing"))

    # Save individual plot as PNG
    individual_plot_path <- paste0(file_path, "_spline_plot.png")
  ggsave(individual_plot_path, plot, width = 2400, height = 1200, units = "px")
  
  individual_plots[[file_path]] <- plot
}

# Combine all individual spline points into one big spline
all_spline_points_df_t <- data.table::rbindlist(all_spline_points)

# Get the first 80% of the data
subset_index <- 1:round(0.8 * nrow(all_spline_points_df_t))
subset_spline_points_df_t <- all_spline_points_df_t[subset_index, ]

# Fit a smooth spline to the subset data
spline_points_big_subset <- smooth.spline(subset_spline_points_df_t$x, subset_spline_points_df_t$y, spar = 0.8)
spline_df_big_subset <- data.frame(x = spline_points_big_subset$x, y = spline_points_big_subset$y)

# Create a big plot with the subset spline
subset_plot <- ggplot(data = spline_df_big_subset, aes(x = x, y = y)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Start Time", y = "Speech Rate (words per second)", title = "Subset Cubic Spline Interpolation")
subset_plot_path <- paste0(folder, "/subset_splinePlot.png")
ggsave(subset_plot_path, subset_plot, width = 2400, height = 1200, units = "px")

