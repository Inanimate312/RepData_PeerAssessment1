library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)


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
#   2a) Histogram of total number of steps taken per day
  #   2ai) Calculate total number of steps taken per day
  #   2aii) Report the mean and median total number of steps taken per day
  
  
  
#   2b) Time series plot of average daily activity pattern
  #   type = "1"
  #   x axis: 5-minute interval
  #   y axis: average steps taken across all days


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