library(dplyr)
library(ggplot2)
library(data.table)

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


# 2) Analysis

# Total number of steps taken per day
total_steps <- sum(activity_data$steps, na.rm = TRUE)
print(total_steps) # 570,608 steps


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
print(mean_steps) # 37.3826 steps


# Median of steps taken per day
median_steps <- median(activity_data$steps, na.rm = TRUE)
print(median_steps) # 0 steps
  

# Time series plot of average daily activity pattern
average_steps_5day <- activity_data %>%
  group_by(interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

ggplot(average_steps_5day, aes(x = interval,
                               y = average_steps)) +
         geom_line(color = "black") +
  labs(title = "Average Steps Per Interval (5-Minutes)",
       x = "Interval",
       y = "Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))

# Which interval contains maximum number of steps?
max <- average_steps_5day %>%
  filter(average_steps == max(average_steps))
print(max) # Interval 835-840 minutes with 206 steps on average

# Report total number of missing values in data set
num_nulls <- sum(is.na(activity_data$steps))
print(num_nulls) # 2,304 records with null value of steps


# Impute missing values with the average steps for the given interval
setDT(activity_data)
activity_data[, mean_steps := mean(steps, na.rm = TRUE), by = interval]

# Overwrite data set with imputed values
activity_data[is.na(steps), steps := mean_steps]

# Re-calculate total, mean, and median steps taken per day
total_steps_imputed <- sum(activity_data$steps, na.rm = TRUE)
print(total_steps_imputed) # 656,737.5 steps

mean_steps_imputed <- mean(activity_data$steps)
print(mean_steps_imputed) # 37.3826 steps

median_steps_imputed <- median(activity_data$steps)
print(median_steps_imputed) # 0 steps

# Histogram of total steps each day, imputed
ggplot(data = activity_data, aes(x = steps)) +
  geom_histogram(fill = "skyblue", 
                 color = "black") +
  labs(title = "Distribution of Total Steps Per Day (Imputed)", 
       x = "Steps Per Day (Imputed)", 
       y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))


# Plot average daily activity pattern for weekdays and weekends
activity_data <- activity_data %>%
  mutate(weekday = weekdays(date)) %>%
  mutate(weekday_type = if_else(weekday %in% c("Saturday", "Sunday"), 
                                "Weekend", "Weekday"),
         weekday_type = factor(weekday_type, levels = c("Weekday","Weekend"))
  )

average_steps <- activity_data %>%
  group_by(weekday_type, interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

ggplot(average_steps, aes(x = interval,
                              y = average_steps)) +
  geom_line(color = "black") +
  facet_wrap(~ weekday_type, ncol = 1) +
  labs(title = "Average Steps Per Interval (5-Minutes) on Weekdays and Weekends",
       x = "Interval",
       y = "Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))