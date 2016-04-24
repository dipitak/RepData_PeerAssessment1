
library(knitr)
opts_chunk$set(echo = TRUE)
library(ggplot2)

pathWd = "working directory path..."
setwd(pathWd)

##### 1. Code for reading in the dataset and/or processing the data
unzip(zipfile="activity.zip")
dtActivity <- read.csv("activity.csv")


##### 2. Histogram of the total number of steps taken each day
##### calculate total steps and plot into graph,
# calculate mean, median
dtActivity.total.steps <- tapply(dtActivity$steps, dtActivity$date, FUN=sum, na.rm=TRUE)
qplot(dtActivity.total.steps, binwidth=1000, xlab="total number of steps taken each day")

##### 3. Mean and median number of steps taken each day
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


##### 4. Time series plot of the average number of steps taken
########### plot the average steps and calculate mean and median
dtActivity.averages <- aggregate(x=list(steps=dtActivity$steps), by=list(interval=dtActivity$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=dtActivity.averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

#### 5. The 5-minute interval that, on average, contains the maximum number of steps
#### calculate max average steps
dtActivity.averages[which.max(dtActivity.averages$steps),]

#### 6. Code to describe and show a strategy for imputing missing data
## calculate total rows haveing missing value
dtActivity.missing <- is.na(dtActivity$steps)
table(dtActivity.missing)

# Replace each missing value with the mean value of its 5-minute interval
dtActivity.fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (dtActivity.averages[dtActivity.averages$interval==interval, "steps"])
        return(filled)
}
dtActivity.filled.data <- dtActivity
dtActivity.filled.data$steps <- mapply(dtActivity.fill.value, dtActivity.filled.data$steps, dtActivity.filled.data$interval)

#### 7. Histogram of the total number of steps taken each day after missing values are imputed
## calculate total steps after filling the missing value with mean value
dtActivity.total.steps <- tapply(dtActivity.filled.data$steps, dtActivity.filled.data$date, FUN=sum)
qplot(dtActivity.total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(dtActivity.total.steps)
median(dtActivity.total.steps)


## check weekend or weekday function
dtActivity.weekdayOrweekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
                return("weekday")
        }
        else if (day %in% c("Saturday", "Sunday")){
                return("weekend")
        }
        else{
                stop("invalid date")
        }
}

####### 7. Panel plot comparing the average number of steps taken per 
####### 5-minute interval across weekdays and weekends
dtActivity.filled.data$date <- as.Date(dtActivity.filled.data$date)
dtActivity.filled.data$day <- sapply(dtActivity.filled.data$date, FUN=dtActivity.weekdayOrweekend )

dtActivity.averages <- aggregate(steps ~ interval + day, data=dtActivity.filled.data, mean)
ggplot(dtActivity.averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")



