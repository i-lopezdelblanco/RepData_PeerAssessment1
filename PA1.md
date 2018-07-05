---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

1. Read the data into a table, and explore the first rows 

```r
activity<-read.csv("activity.csv")
head(activity)
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
2. Coerce the data to a data frame

```r
activitydf<-as.data.frame(activity)
```
## What is mean total number of steps taken per day?
1. Total number of steps each day in the variable StepsDay, to be displayed. Then explore a summary of the data. 

```r
StepsDay<-rowsum(activitydf$steps,activitydf$date)
StepsDay
```

```
##             [,1]
## 2012-10-01    NA
## 2012-10-02   126
## 2012-10-03 11352
## 2012-10-04 12116
## 2012-10-05 13294
## 2012-10-06 15420
## 2012-10-07 11015
## 2012-10-08    NA
## 2012-10-09 12811
## 2012-10-10  9900
## 2012-10-11 10304
## 2012-10-12 17382
## 2012-10-13 12426
## 2012-10-14 15098
## 2012-10-15 10139
## 2012-10-16 15084
## 2012-10-17 13452
## 2012-10-18 10056
## 2012-10-19 11829
## 2012-10-20 10395
## 2012-10-21  8821
## 2012-10-22 13460
## 2012-10-23  8918
## 2012-10-24  8355
## 2012-10-25  2492
## 2012-10-26  6778
## 2012-10-27 10119
## 2012-10-28 11458
## 2012-10-29  5018
## 2012-10-30  9819
## 2012-10-31 15414
## 2012-11-01    NA
## 2012-11-02 10600
## 2012-11-03 10571
## 2012-11-04    NA
## 2012-11-05 10439
## 2012-11-06  8334
## 2012-11-07 12883
## 2012-11-08  3219
## 2012-11-09    NA
## 2012-11-10    NA
## 2012-11-11 12608
## 2012-11-12 10765
## 2012-11-13  7336
## 2012-11-14    NA
## 2012-11-15    41
## 2012-11-16  5441
## 2012-11-17 14339
## 2012-11-18 15110
## 2012-11-19  8841
## 2012-11-20  4472
## 2012-11-21 12787
## 2012-11-22 20427
## 2012-11-23 21194
## 2012-11-24 14478
## 2012-11-25 11834
## 2012-11-26 11162
## 2012-11-27 13646
## 2012-11-28 10183
## 2012-11-29  7047
## 2012-11-30    NA
```

```r
summary(StepsDay)
```

```
##        V1       
##  Min.   :   41  
##  1st Qu.: 8841  
##  Median :10765  
##  Mean   :10766  
##  3rd Qu.:13294  
##  Max.   :21194  
##  NA's   :8
```
2. Plot an histogram of the data

```r
hist(StepsDay)
```

![](PA1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



3. Mean and median of the number of steps per day

```r
meanv<-round(mean(StepsDay, na.rm=TRUE),digits=0)
med<-median(StepsDay, na.rm=TRUE)
```
 The mean number of steps per day rounded to the unit step is 10766, and the median number of steps per day is 10765


## What is the average daily activity pattern?
1. Plot of the average number of steps across all days per daily 5-min interval


```r
pattern<-aggregate(activitydf$steps~activitydf$interval, FUN=mean)

plot(pattern, type="l", xlab="5-min interval", ylab="daily average number of steps")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. The interval that contains the maximum average number of steps:

```r
imax<-which(pattern$`activitydf$steps` == max(pattern$`activitydf$steps`), arr.ind = TRUE)
maxintv<-pattern$`activitydf$interval`[imax]
```
It is 835

## Imputing missing values
1. Missing values in the dataset:

```r
s<-sum(is.na(activitydf))
```
The total number of missing values in the dataset is 2304.


2. A strategy for filling in all missing values in the dataset could be assigning the mean value of the variable where data is missing.

3. Creating a new dataset that is equal to the original dataset but with the missing data filled in

Install the imputeTS package if not previouly installed
#```{r}
#install.packages("imputeTS")
#```



```r
library(imputeTS)
```

```
## Warning: package 'imputeTS' was built under R version 3.5.1
```

```r
activitydf2<-activitydf
activitydf2<-na.mean(activitydf)
```

3. Histogram of the total number of steps taken each day 


```r
StepsDay2<-rowsum(activitydf2$steps,activitydf2$date)
summary(StepsDay2)
```

```
##        V1       
##  Min.   :   41  
##  1st Qu.: 9819  
##  Median :10766  
##  Mean   :10766  
##  3rd Qu.:12811  
##  Max.   :21194
```

```r
hist(StepsDay)
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Mean and median total number of steps taken per day. 

```r
meanv2<-round(mean(StepsDay2, na.rm=TRUE),digits=0)
med2<-median(StepsDay2, na.rm=TRUE)
```
 The mean number of steps per day rounded to the unit step is 10766, and the median number of steps per day is 10766.19
 
These values do not differ from the estimates from the first part of the assignment. Imputing the variable mean where the variable is not registered is not altering the mean. It may alter other summary statistics. Median is affected by a fraction of a unit.


## Are there differences in activity patterns between weekdays and weekends?

1. To create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day, first add the day-of-week column in english language:

```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activitydf2$day <- weekdays(as.Date(activitydf2$date), abbreviate=FALSE)
```

and then convert day-of-week column vales to Weekday or Weekend

```r
for (i in 1:length(activitydf2$day)){
if (activitydf2$day[i] == "Saturday" || activitydf2$day[i] == "Sunday"){
        activitydf2$day[i] <- "Weekend"
} else {
        activitydf2$day[i] <- "Weekday"
}
}
activitydf2$day<-as.factor(activitydf2$day)
```



2. Panel plot comparing weekends and wwekdays:

First, separate data for each 


```r
X <- split(activitydf2, activitydf2$day)
d1<-as.data.frame(X[1])
d2<-as.data.frame(X[2])

names(d1)[1]<-paste("steps")
names(d1)[3]<-paste("interval")

names(d2)[1]<-paste("steps")
names(d2)[3]<-paste("interval")
```


Second, compute the relevant averages 


```r
pat1<-aggregate(d1$steps~d1$interval, FUN=mean)
pat2<-aggregate(d2$steps~d2$interval, FUN=mean)
```


Panel plot of both segments


```r
par(mfrow=c(2,1))
plot(pat2, type="l", xlab="5-min interval", ylab="avg steps", main = "weekend")
plot(pat1, type="l", xlab="5-min interval", ylab="avg steps", main = "weekday")
```

![](PA1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
