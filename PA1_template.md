# Reproducible Research: Peer Assessment 1

## Library Used:
library(knitr)
opts_chunk$set(echo = TRUE)
library(ggplot2)

## 1. Loading and preprocessing the data

```r
unzip(zipfile="activity.zip")
dtActivity <- read.csv("activity.csv")
```

## 2.  What is mean total number of steps taken per day?

```r
dtActivity.total.steps <- tapply(dtActivity$steps, dtActivity$date, FUN=sum, na.rm=TRUE)
qplot(dtActivity.total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

## 3. Mean and median number of steps taken each day

```r
mean(total.steps, na.rm=TRUE)
```

```
## Error in mean(total.steps, na.rm = TRUE): object 'total.steps' not found
```

```r
median(total.steps, na.rm=TRUE)
```

```
## Error in median(total.steps, na.rm = TRUE): object 'total.steps' not found
```

## 4. What is the average daily activity pattern?

```r
dtActivity.averages <- aggregate(x=list(steps=dtActivity$steps), by=list(interval=dtActivity$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=dtActivity.averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## 5. On average across all the days in the dataset, the 5-minute interval contains the maximum number of steps?

```r
dtActivity.averages[which.max(dtActivity.averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## 6. Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
dtActivity.missing <- is.na(dtActivity$steps)
table(dtActivity.missing)
```

```
## dtActivity.missing
## FALSE  TRUE 
## 15264  2304
```

All of the missing values are filled in with mean value for that 5-minute
interval.


```r
## 7. Replace each missing value with the mean value of its 5-minute interval
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
```
## 7. Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
dtActivity.total.steps <- tapply(dtActivity.filled.data$steps, dtActivity.filled.data$date, FUN=sum)
qplot(dtActivity.total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
mean(dtActivity.total.steps)
```

```
## [1] 10766.19
```

```r
median(dtActivity.total.steps)
```

```
## [1] 10766.19
```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
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
dtActivity.filled.data$date <- as.Date(dtActivity.filled.data$date)
dtActivity.filled.data$day <- sapply(dtActivity.filled.data$date, FUN=dtActivity.weekdayOrweekend )
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
dtActivity.averages <- aggregate(steps ~ interval + day, data=dtActivity.filled.data, mean)
ggplot(dtActivity.averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)
