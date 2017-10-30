# Peer-graded Assignment: Course Project 1
Marlon Martinez  
October 29, 2017  



### Loading and preprocessing the data

Creating a data frame with the activity.csv

```r
activityDF<-read.table("activity.csv",sep = ",",header = TRUE)
head(activityDF)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

### What is mean total number of steps taken per day?

First, it's necessary to sum all steps for each day (missing values are ingnored)

```r
totalByDay<-tapply(activityDF$steps,activityDF$date,FUN = sum,na.rm=TRUE)
head(totalByDay)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

Histogram of the total number steps per day:

```r
hist(totalByDay,main = "Histogram of Total of Steps per Day",xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean of total steps per day is : 

```
## [1] 9354.23
```
and the median is: 

```
## [1] 10395
```

###What is the average daily activity pattern?

Average of steps for each interval:

```r
avgByInterval<-tapply(activityDF$steps,activityDF$interval,FUN = mean,na.rm=TRUE)
head(avgByInterval)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

Plotting results:

```r
plot(avgByInterval,type="l",xlab = "Interval position",ylab = "Average")
abline(v=which.max(avgByInterval),col="blue",lty=2)
legend("topright",legend=paste("Interval with max average: ",names(which.max(avgByInterval))),col ="blue",lty = 2,lwd=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgByInterval[which.max(avgByInterval)]
```

```
##      835 
## 206.1698
```

### Imputing missing values

The presence of missing days may introduce bias into some calculations or summaries of the data.
Total number of missing values in the dataset:

```r
sum(is.na(activityDF$steps))
```

```
## [1] 2304
```

The mean for the 5-minute interval calculated before was used to fill the missing data in the dataset.

```r
avgDF<-data.frame(interval=as.factor(names(avgByInterval)),avg=avgByInterval)
newActivityDF<-merge(activityDF,avgDF,by="interval")
head(newActivityDF)
```

```
##   interval steps       date      avg
## 1        0    NA 2012-10-01 1.716981
## 2        0     0 2012-11-23 1.716981
## 3        0     0 2012-10-28 1.716981
## 4        0     0 2012-11-06 1.716981
## 5        0     0 2012-11-24 1.716981
## 6        0     0 2012-11-15 1.716981
```

For all the missing values of the dataset of steps they will use the mean(avg) value of each interval. 

```r
newActivityDF$steps[is.na(newActivityDF$steps)]<-round(newActivityDF$avg[is.na(newActivityDF$steps)])
```

Sum all steps for each day (including missing values with the average of each interval)

```r
newTotalByDay<-tapply(newActivityDF$steps,newActivityDF$date,FUN = sum,na.rm=TRUE)
head(newTotalByDay)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10762        126      11352      12116      13294      15420
```

Histogram of the total number steps per day:

```r
hist(newTotalByDay,main = "Histogram of Total of Steps per Day",xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

**Now the mean and median are higher than before.**

```r
mean(newTotalByDay)
```

```
## [1] 10765.64
```

```r
median(newTotalByDay)
```

```
## [1] 10762
```

##Are there differences in activity patterns between weekdays and weekends?

Creating two level factor variable called weekday using the dataset with missing values fixed

```r
newActivityDF$date<-as.POSIXct(strptime(newActivityDF$date, "%Y-%m-%d"))
newActivityDF$weekday<-"weekday"
newActivityDF$weekday[weekdays(newActivityDF$date,abbreviate = TRUE)==c("Sat","Sun")]<-"weekend"
newActivityDF$weekday<-as.factor(newActivityDF$weekday)
head(newActivityDF)
```

```
##   interval steps       date      avg weekday
## 1        0     2 2012-10-01 1.716981 weekday
## 2        0     0 2012-11-23 1.716981 weekday
## 3        0     0 2012-10-28 1.716981 weekday
## 4        0     0 2012-11-06 1.716981 weekday
## 5        0     0 2012-11-24 1.716981 weekend
## 6        0     0 2012-11-15 1.716981 weekday
```

Creating 2 dataset with the mean of steps by interval, one for weekdays and the other for weekend

```r
subWeekDF<-aggregate(steps~interval,subset(newActivityDF,newActivityDF$weekday=="weekday"),FUN = mean) 
subWeekendDF<-aggregate(steps~interval,subset(newActivityDF,newActivityDF$weekday=="weekend"),FUN = mean)

subWeekDF$weekday<-"weekday"
subWeekendDF$weekday<-"weekend"
```

Merging the datasets with the average

```r
weekDayDF<-rbind(subWeekDF,subWeekendDF)
head(weekDayDF)
```

```
##   interval      steps weekday
## 1        0 1.98113208 weekday
## 2        5 0.32727273 weekday
## 3       10 0.13461538 weekday
## 4       15 0.14814815 weekday
## 5       20 0.07692308 weekday
## 6       25 1.39215686 weekday
```

Plotting the mean of intervals separated by weekday

```r
library(ggplot2)
gp<-ggplot(data = weekDayDF,aes(interval,steps))
gp+geom_line()+facet_grid(.~weekday)+ylim(0,250)+ggtitle("Average number of steps by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
