# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
The zip file containing the data for this analysis is available for download from [here]. Since this data set is realativly small it can be downloaded to a temp file and read in direcctly.


```r
temp<-tempfile()
activtyurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(activtyurl,temp)
activitydata <- read.table(unz(temp,"activity.csv"),sep=",",na.strings = "NA",header = TRUE)
activitydata$date<-as.Date(activitydata$date)
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

To get the totals, I summarized the data using the plyr package and ploted the histogram using the based plotting package.

```r
library(plyr)
stepsByDay<-ddply(activitydata,.(date),summarize,total=sum(steps))
with(stepsByDay,hist(total, breaks = 15))
```

![](PA1_template_files/figure-html/total steps per day-1.png)<!-- -->

The mean and the median steps per day are:

```r
with(stepsByDay,mean(total,na.rm=TRUE))
```

```
## [1] 10766.19
```

```r
with(stepsByDay,median(total,,na.rm=TRUE))
```

```
## [1] 10765
```

## What is the average daily activity pattern?

It should not surpring but activity is not spaced out equally throughout the day.

```r
stepsbyInt<-ddply(activitydata,.(interval),summarize,avgSteps=mean(steps,na.rm=TRUE))
with(stepsbyInt,plot(interval,avgSteps,type="l"))
abline(v=stepsbyInt[stepsbyInt$avgSteps==max(stepsbyInt$avgSteps),1],lwd=2,col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The most activity occurs at:

```r
stepsbyInt[stepsbyInt$avgSteps==max(stepsbyInt$avgSteps),1]
```

```
## [1] 835
```

## Imputing missing values
Browsing through the data it becomes clear that there are entire days and many intervals where values are missing.  Lets take a look at just how many there are


```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

We can see that roughly 13% of the observations are at missing, which is fairly significant. This raises the question, how would the data look if we imputed values to fill in the missing invtervals?

Since we have already calcuated the average for each interval, a simple approach is to use the average number of steps for for the interval that is missing. First I created a copied of the acivitydata data frame, into a new data frame "fullActivity".  Then set the rows equal to the avreages from stepsbyInt where the interval number for the day is equal.


```r
fullActivity<-activitydata
fullActivity[is.na(fullActivity$steps)==TRUE,]$steps = stepsbyInt$avgSteps[match(stepsbyInt$interval,fullActivity$interval)]
```

As we did before, we can get the total number of steps for our use fullActivity data frame.

```r
stepsByDay1<-ddply(fullActivity,.(date),summarize,total=sum(steps))
with(stepsByDay1,hist(total,breaks=15))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Then we can look at the summary stats

```r
with(stepsByDay1,mean(total,na.rm=TRUE))
```

```
## [1] 10766.19
```

```r
with(stepsByDay1,median(total,,na.rm=TRUE))
```

```
## [1] 10766.19
```

The values of the mean and the median for fullActvity are not very different than they were when we excluded the missing values. Replacing missing values with the average for each interval did not have a large impact on the distrbution or the mean or median.

## Are there differences in activity patterns between weekdays and weekends?


```r
fullActivity$weekend<-weekdays.Date(fullActivity$date)%in%c("Saturday","Sunday")
fullActivity$dayType<-as.factor(ifelse(fullActivity$weekend==TRUE,"Weekend","Weekdays"))

stepsByInt1<-ddply(fullActivity,.(interval,dayType),summarize,total=sum(steps))

library(lattice)
xyplot(total~interval|dayType,type="l", data=stepsByInt1, layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Yes there is much more activity on the weekdays.

[here]:<"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip">
