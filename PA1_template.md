# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Here I suppose that the zipped data on acitivity (activity.zip from the initial github repository) is in the wroking directory. I first unzip the data and load it into R:

```r
activity <- read.csv(unz("activity.zip","activity.csv"))
```



## What is mean total number of steps taken per day?
1. Let's first break the data by date and calulate the total number of steps taken each day. I use the *plyr* package, so you have to make sure it's installed on your computer if you want to reproduce the code entirely

```r
library(plyr)
stepbyday <- ddply(activity,.(date),summarise, totalsteps = sum(steps, na.rm = TRUE) )
```

Now we can make a histogram of the variable *totalsteps* in *stepbyday* data frame:

```r
hist(stepbyday$totalsteps, xlab = "Number of Steps", main = "Histogram of the Total Number\n of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Again using the variable *totalsteps* in *stepbyday* data frame we can easily calculate the **mean** number of steps taken each day, which appears to be 

```r
meansteps <- mean(stepbyday$totalsteps)
meansteps
```

```
## [1] 9354
```

We can also calculate the median number of steps in the same manner:

```r
mediansteps <- median(stepbyday$totalsteps)
mediansteps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. I again use the *ddply* function from *plyr* package to break the data by 5-minute intervals and average:

```r
stepbyinterval <- ddply(activity,.(interval),summarise, averagesteps = mean(steps, na.rm = TRUE))
```

Now the variable *averagesteps* in the data frame *stepbyinterval* stores the average number of steps taken during a particular interval. I can now make the time series of daily activity patterns:

```r
plot(stepbyinterval$interval, stepbyinterval$averagesteps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

2. I can also find the five-minute interval with the highest average number of steps taken:

```r
highest <- which.max(stepbyinterval$averagesteps)
stepbyinterval$interval[highest]
```

```
## [1] 835
```

## Imputing missing values

1. Let's count how many rows in the data set contain NA values. We do this by calculating the number of rows in the original dataset and in the dataset with rows containing any NAs omitted:


```r
nrow(activity) - nrow(na.omit(activity))
```

```
## [1] 2304
```

2. Note that the only the variable steps contains missing values:

```r
steps_nas <- sum(unlist(sapply(activity$steps,is.na)))
steps_nas
```

```
## [1] 2304
```

```r
days_nas <- sum(unlist(sapply(activity$day,is.na)))
days_nas
```

```
## [1] 0
```

```r
interval_nas <- sum(unlist(sapply(activity$interval,is.na)))
interval_nas
```

```
## [1] 0
```

Let's make the strategy for missing values for *steps* variable simple: every NA will be substituted with the average number of steps in the corresponding interval. I first make a copy of the original dataset and then substitute the missing values using the previously computed *stepbyinterval* data frame:

```r
act <- activity
for (i in 1:nrow(act)){
        if (is.na(act$steps[i]) == TRUE){
                ## If the number of steps is missing, get the inteval number
                interval <- act$interval[i]
                ## Get the average number of steps for current interval in stepbyinterval data frame
                act$steps[i] <- stepbyinterval$averagesteps[stepbyinterval$interval == interval]
        }
}
```

I now repeat the steps from the first part of the assignment, build the histogram:


```r
stepbyday1 <- ddply(act,.(date),summarise, totalsteps = sum(steps) )
hist(stepbyday1$totalsteps, xlab = "Number of Steps", main = "Histogram of the Total Number\n of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

calculate the mean

```r
meansteps1 <- mean(stepbyday1$totalsteps)
meansteps1
```

```
## [1] 10766
```

and the median

```r
mediansteps1 <- median(stepbyday1$totalsteps)
mediansteps1
```

```
## [1] 10766
```

The estimates obviously **differ** from those computed from the original dataset. In this case imputing missing data resulted in **higher** estimates for both the mean and the median of the total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
1. Let's first create the new factor variable indicating whether each observation occured on "weekday" or "weekend":

```r
act$date <- as.Date(as.character(act$date))
act$dayofweek <- weekdays(act$date) %in% c('Saturday','Sunday')
act$dayofweek <- factor(act$dayofweek, levels = c(FALSE,TRUE), labels = c("weekday","weekend"))
```

2. We can now break the dataset into two separate ones on weekdays and weekends and build two plots similar to the one in section on daily activity pattern. 

Subset the data on weekdays and weekends, calculate the average number of steps for each interval (weekends and weekdays separately)

```r
weekdaysteps <- act[act$dayofweek == "weekday",]
byinterval_weekday <- ddply(weekdaysteps,.(interval),summarise, averagesteps = mean(steps))
weekendsteps <- act[act$dayofweek == "weekend",]
byinterval_weekend <- ddply(weekendsteps,.(interval),summarise, averagesteps = mean(steps))
```

Subset and plot the data on weekends 

```r
par(mar = c(3,3,5,3))
par(mfrow=c(2,1)) 
plot(byinterval_weekday$interval, byinterval_weekday$averagesteps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Weekday")
mtext("Average Daily Activity Pattern", side = 3, line = 3, cex = 2)
plot(byinterval_weekend$interval, byinterval_weekend$averagesteps, type = "l", xlab = "Interval", ylab = "Average Number of Steps", main = "Weekend")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 
