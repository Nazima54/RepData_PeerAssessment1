Reproducible Research Peer Assignment1

echo = TRUE

```r
load all packages used in this exploratory analysis
library(knitr)
library(datasets)
install.packages("devtools")  # so we can install from GitHub
devtools::install_github("ropensci/plotly")
```
Read the dataset "activity.csv"

```r
par(mar=c(1,1,1,1))
if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  file <- unzip(temp)
  unlink(temp)
}
activity <- read.csv("activity.csv")
```
Calculate the total number of steps taken per day(Calculate mean)

```r
stepsPerDay <- aggregate(steps ~ date,activity,sum)
stepsPerDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```
Show the Histogram of the Mean value of the steps taken per day

```r
hist(stepsPerDay$steps,main = "Number Of Steps Taken Per Day",col = "green", xlab = "steps")
```

```
## Warning in par("page"): "page" is not a graphical parameter
```

```
## Warning in plot_snapshot(incomplete_plots): Please upgrade R to at least
## version 3.0.2
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
Calculate and report the mean and median of the total number of steps taken per day

```r
stepsmean <- mean(stepsPerDay$steps)
stepsmedian <- median(stepsPerDay$steps)
stepsmean
```

```
## [1] 10766.19
```

```r
stepsmedian
```

```
## [1] 10765
```
The Mean of the total number of steps taken per day is 10766.1886792453
The Median number of steps taken per day is 10765

```r
## What is the average daily activity pattern?
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
totalstepsByInterval <- aggregate(steps ~ interval,data = activity,mean, na.rm = TRUE)
head(totalstepsByInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(steps ~ interval, data = totalstepsByInterval, type = "l",xlab="Interval",ylab ="Number of Steps",main="Average Number of Steps per Day by Interval")
```

```
## Warning in par("page"): "page" is not a graphical parameter
```

```
## Warning in plot_snapshot(incomplete_plots): Please upgrade R to at least
## version 3.0.2
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- totalstepsByInterval[which.max(totalstepsByInterval$steps), ]$interval
maxSteps
```

```
## [1] 835
```
## In every 5-minute interval, on average across all the days in the dataset,the maximum number of steps taken are 835")

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missingNAs <- activity[!complete.cases(activity),]
nrow(missingNAs)
```

```
## [1] 2304
```

```r
## "Total number of missing values in the dataset are 2304"
```

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc
## Impute data to replace NA's with mean value
## make fuction name Mean Intervalsteps

```r
interval2steps <- function(interval) {
  totalstepsByInterval[totalstepsByInterval$interval == interval, ]$steps
}
```
##Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
NAactivityFilled <- activity  # Make a new dataset with the original data
count = 0  # Count the number of data filled in
for (i in 1:nrow(NAactivityFilled)) {
  if (is.na(NAactivityFilled[i, ]$steps)) {
    NAactivityFilled[i, ]$steps <- interval2steps(NAactivityFilled[i, ]$interval)
    count = count + 1
  }
}
cat("Total ", count, "NA values were filled.\n\r")
```

```
## Total  2304 NA values were filled.
## 
```
## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing 

```r
totalNumsteps <- aggregate(steps ~ date, data = NAactivityFilled,sum) 
totalNumsteps
```

```
##          date    steps
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
hist(totalNumsteps$steps)
```

```
## Warning in par("page"): "page" is not a graphical parameter
```

```
## Warning in plot_snapshot(incomplete_plots): Please upgrade R to at least
## version 3.0.2
```

```r
mean(totalNumsteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalNumsteps$steps)
```

```
## [1] 10766.19
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
## The avergage mean of the steps taken per day before imputing the activity data and after replacing  NA's  of the same with the mean value, shows very less impact on its average mean.The differnece in mean value is negligible or very less.


```r
##Are there differences in activity patterns between weekdays and weekends?
##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# add a new column containing day of week
NAactivityFilled$date = as.Date(NAactivityFilled$date)
##create variables weekend and weekday
## add a new column indicating day of the week 
NAactivityFilled$day <- weekdays(NAactivityFilled$date)
# add a new column called day type and initialize to weekday
NAactivityFilled$day_type <- c("weekday")
## If day is Saturday or Sunday, make day_type as weekend
for (i in 1:nrow(NAactivityFilled)){
  if (NAactivityFilled$day[i] == "Saturday" || NAactivityFilled$day[i] == "Sunday"){
    NAactivityFilled$day_type[i] <- "weekend"
  }
}
```

```r
  ## convert day_time from character to factor
  NAactivityFilled$day_type <- as.factor(NAactivityFilled$day_type)
  ## aggregate steps as interval to get average number of steps in an interval across all days
  imputed_table <- aggregate(steps ~ interval+day_type,NAactivityFilled,mean)
```

```r
  ## make the panel plot for weekdays and weekends
  library(ggplot2)
   qplot(interval,steps,data=imputed_table,geom=c("line"),xlab="Interval", 
   ylab="Number of steps",main="") +facet_wrap(~ day_type, ncol=1)
```

```

```
