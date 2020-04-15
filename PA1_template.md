---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

# Reproducible Research: Peer Assessment 1




### Loading and preprocessing the data
##### 1. Load the data

```r
activity <- read.csv('activity.csv', na.strings = "NA")
```
##### 2. Clean up the date class

```r
activity$date <- ymd(activity$date)
```

### What is mean total number of steps taken per day?

```r
steps <- tapply(activity$steps, activity$date, sum, na.rm=T)
```
##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(steps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
meanSteps <- mean(steps)
medianSteps <- median(steps)
```
- Mean Steps 9354.2295082
- Median Steps 10395

### What is the average daily activity pattern?

```r
activity1 <- activity %>% group_by(interval) %>% summarize(mean.step=mean(steps, na.rm=T))
```

##### 1. Make a time series plot

```r
ggplot(activity1, aes(x=interval,y=mean.step)) + 
  geom_line(color="red") + 
  labs(y="Average Number of Steps", x="5-min Interval Times Series")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- which.max(activity1$mean.step)
maxInterval = activity1$interval[max]
```
- 5 min interval with max number of step on average is 835

### Imputing missing values

```r
numberOdMissingValues <- length(which(is.na(activity)))
```
- Number of missing values are 2304

##### 1. Devise a strategy for filling in all of the missing values in the dataset. Fill all the na values with mean

```r
impute <- activity
nas <- is.na(impute$steps)
intervalAverage <- tapply(impute$steps, impute$interval, mean, na.rm = TRUE, simplify = TRUE)
impute$steps[nas] <- intervalAverage[as.character(impute$interval[nas])]
```

##### 2. Make a histogram of the total number of steps taken each day 

```r
imputeSteps <- tapply(impute$steps, impute$date, sum)
qplot(imputeSteps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 

```r
impute$day <- ifelse(weekdays(impute$date) %in% c("Saturday","Sunday"), "weekend", "weekday")
averagedActivityImputed <- aggregate(steps ~ interval + day, data=impute, mean)
ggplot(averagedActivityImputed, aes(x=interval,y=steps, color=day)) + 
    geom_line() + 
    facet_grid(.~day) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
