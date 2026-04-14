library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)


# 1) Read in activity monitoring data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

zipfile <- file.path(getwd(), 
                     "activity_monitoring.zip")

if(!file.exists(zipfile)) {
  download.file(url,
                destfile = zipfile)
  unzip(zipfile)
}

activity_monitoring_file <- file.path(getwd(), 
                                      "activity.csv")

activity_data <- read_csv(activity_monitoring_file)


# 2) Plot data
# Total number of steps taken per day
total_steps <- sum(activity_data$steps, na.rm = TRUE)
print(total_steps)

# Histogram of total number of steps taken per day
ggplot(data = activity_data, aes(x = steps)) +
  geom_histogram(fill = "skyblue", 
                 color = "black") +
  labs(title = "Distribution of Total Steps Per Day", 
       x = "Steps Per Day", 
       y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Mean of steps taken per day
mean_steps <- mean(activity_data$steps, na.rm = TRUE)
print(mean_steps)

# Median of steps taken per day
median_steps <- median(activity_data$steps, na.rm = TRUE)
print(median_steps)
  
# Time series plot of average daily activity pattern
average_steps_5day <- activity_data %>%
  group_by(interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

ggplot(average_steps_5day, aes(x = interval,
                               y = average_steps)) +
         geom_line(color = "black") +
  labs(title = "Average Daily Activity Pattern (5-Minute Intervals)",
       x = "Interval",
       y = "Average Number of Steps Across All Days") +
  theme(plot.title = element_text(hjust = 0.5))
      


#   2c) Impute missing values
  #   2ci) Report total number of missing values in dataset
  #   2cii) Impute missing values and save as new dataset
    # Can use mean/median for that day, mean for that 5-min interval, etc.
  #   2ciii) Histogram of total steps each day
  #   2civ) Report mean and median total number of steps taken per day

#   2d) Create a factor variable with two levels ("weekday" and "weekend")
  #   Use the weekdays() function on imputed_data$date
  #   2di) Create a panel plot containing time series of activity pattern
    #   type = "1"
    #   x axis: 5-minute interval
    #   y axis: average number of steps taken across all weekday or weekend days