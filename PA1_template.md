# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
if (!file.exists("projectdata")) {dir.create("projectdata")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./projectdata/Dataset.zip", mode = "wb")
unzip<- unzip( "./projectdata/Dataset.zip", exdir= "projectdata")
setwd("./projectdata")
ActivityData <- read.csv("activity.csv")
```

## What is the mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
StepsPerDay <- aggregate(steps ~ date, data=ActivityData, FUN=sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
barplot(StepsPerDay$steps, names.arg=StepsPerDay$date, xlab="Date", ylab="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(StepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(StepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepIntervals <- aggregate(steps ~ interval, data=ActivityData, FUN=mean)
plot(StepIntervals, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
StepIntervals$interval[which.max(StepIntervals$steps)]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(ActivityData))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I will use the mean of the 5-minute intervals as the new value for all missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
ActivityData <- merge(ActivityData, StepIntervals, by="interval", suffixes=c("",".y"))
NAs <- is.na(ActivityData$steps)
ActivityData$steps[NAs] <- ActivityData$steps.y[NAs]
ActivityData <- ActivityData[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
NewStepsPerDay <- aggregate(steps ~ date, data=ActivityData, FUN=sum)
barplot(NewStepsPerDay$steps, names.arg=NewStepsPerDay$date, xlab="Date", ylab="Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(NewStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(NewStepsPerDay$steps)
```

```
## [1] 10766.19
```

The missing values do not seem to have much impact on the mean or median.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
DayType <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "Weekend"
    } else {
        "Weekday"
    }
}
ActivityData$DayType <- as.factor(sapply(ActivityData$date, DayType))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow=c(2,1))
for (type in c("Weekend", "Weekday")) {
    StepsByType <- aggregate(steps ~ interval, data=ActivityData, subset=ActivityData$DayType==type, FUN=mean)
    plot(StepsByType, type="l", main=type)
}
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
