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
interval <- group_by(activityData, interval) %>%
            summarise(averageSteps = mean(steps, na.rm = TRUE))

#Get the interval with maximum number of steps
maxSteps <- interval[interval$averageSteps == max(interval$averageSteps), ]

#Creating a great text to to plot
legendText <- paste0("\n \nMaximum number \nof steps (interval: ", maxSteps$interval,")")

#Plot interval x average number of steps
with(interval, plot(interval, averageSteps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps"))
title(main="What is the average daily activity pattern?")

# Add a point and text to maximum number of steps
points(maxSteps$interval, maxSteps$averageSteps, pch = 16, col = "red")
text(maxSteps$interval, maxSteps$averageSteps, labels = legendText, pos = 4)

