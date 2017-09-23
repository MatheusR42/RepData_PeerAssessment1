library(dplyr)

# Loading and preprocessing the data
activityData <- read.csv(unz("activity.zip", "activity.csv"), sep = ",")
activityData$date <- as.Date(as.character(activityData$date), "%Y-%m-%d")

# What is mean total number of steps taken per day?
totalByDay <- group_by(activityData, date) %>% summarize(steps = sum(steps, na.rm = TRUE))
meanSteps <- mean(totalByDay$steps, na.rm = TRUE)
medianSteps <- median(totalByDay$steps, na.rm = TRUE)
hist(totalByDay$steps, col = "turquoise3", xlab = "Total number of steps", main = "Total number of steps taken each day")
abline(v=meanSteps, col="red", lwd=2)
abline(v=medianSteps, col="black", lwd=2)
legend(
    "topright", # places a legend at the appropriate place
    c(paste0("Mean (", round(meanSteps,1)," steps)"), paste0("Median (", medianSteps," steps)")), # puts text in the legend
    pch = c("|","|"), # gives the legend appropriate symbols (lines)
    col=c("red", "black"),# gives the legend lines the correct color
    cex=0.8 # gives the legend box size
)

#Group by interval and summarize the average
groupInterval <- group_by(activityData, interval) %>%
            summarise(averageSteps = mean(steps, na.rm = TRUE))

#Get the interval with maximum number of steps
maxSteps <- groupInterval[groupInterval$averageSteps == max(groupInterval$averageSteps), ]

#Creating a great text to to plot
legendText <- paste0("\n \nMaximum number \nof steps (interval: ", maxSteps$interval,")")

#Plot interval x average number of steps
with(groupInterval, plot(interval, averageSteps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps"))
title(main="What is the average daily activity pattern?")

# Add a point and text to maximum number of steps
points(maxSteps$interval, maxSteps$averageSteps, pch = 16, col = "red")
text(maxSteps$interval, maxSteps$averageSteps, labels = legendText, pos = 4)

# Imputing missing values

# Create a new datset with original data
filledData <- activityData 

# Loop through the rows
for(i in 1:nrow(filledData)){
    # Verify if this row has missing value
    if(is.na(filledData[i, "steps"])){
        # Get row 5-minute interval
        rowInterval <- filledData[i, "interval"]
        
        # Filling in the missing value with the round of the mean value to this interval
        filledData[i, "steps"] <- round(groupInterval[groupInterval$interval == rowInterval, "averageSteps"], 0)
    }
}

# Original data
head(activityData,10)

# Filled in data
head(filledData,10)

# Set locale category to show week days in English
Sys.setlocale("LC_TIME", "C")
unique(weekdays(filledData$date))

# Creating a new factor variable to indicating whether a given date is a weekday or weekend day.
filledData$weekday <- "weekday"
filledData[weekdays(filledData$date) %in% c("Saturday", "Sunday"), "weekday"] <- "weekend"
filledData$weekday <- as.factor(filledData$weekday)

#Group by interval and weekday and summarize the average
groupIntervalWeekDay <- group_by(filledData, interval, weekday) %>%
    summarise(averageSteps = mean(steps, na.rm = TRUE))

library(ggplot2)
qplot(interval, averageSteps, data = groupIntervalWeekDay,
      facets = weekday ~ ., geom = "line",
      xlab = "5-minute interval", ylab = "Number of steps",
      main = "Differences in activity \npatterns between weekdays and weekends")
