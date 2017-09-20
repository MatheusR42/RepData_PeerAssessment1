library(dplyr)
activityData <- read.csv(unz("activity.zip", "activity.csv"), sep = ",")
activityData$date <- as.Date(as.character(activityData$date), "%Y-%m-%d")

totalByDay <- group_by(activityData, date) %>% summarize(steps = sum(steps, na.rm = TRUE))
meanSteps <- mean(totalByDay$steps, na.rm = TRUE)
medianSteps <- median(totalByDay$steps, na.rm = TRUE)
hist(totalByDay$steps, col = "turquoise3", xlab = "Total number of steps", main = "Total number of steps taken each day")
abline(v=meanSteps, col="red")
abline(v=medianSteps, col="black")