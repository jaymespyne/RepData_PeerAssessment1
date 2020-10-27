Reproducible Research: Week 2 Assignment (PA1\_Template)
========================================================

Setup Code
----------

    library(ggplot2)
    library(plyr)

    setwd('C:\\Users\\Jaymes Pyne\\Google Drive\\1_Stanford_Pyne\\Classes\\Coursera\\Data_Science_Specialization\\5 Reproducible Research')

      steps<-read.csv('./2_Data/activity.csv',header=T)
      stepsc<-steps[!is.na(steps),]
      steps$date<-as.Date(steps$date,"%Y-%m-%d")

Histogram of Number of Steps Taken Each Day (Missing Values Removed)
--------------------------------------------------------------------

      stepspd<-aggregate(x=steps[c("steps")],by=list(date=steps$date),
                         na.rm=T,FUN=sum,nfrequency=1)
      hist(stepspd$steps,xlab="Steps",main="Total Steps Per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

Summary Statistics of Steps Taken Per Day (MIssing Values Removed)
------------------------------------------------------------------

    summary(stepspd$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1    6779   10396    9355   12812   21195

Average Daily Activity Pattern (Missing Values Removed)
-------------------------------------------------------

    stepsap<-aggregate(x=steps[c("steps")],by=list(interval=steps$interval),na.rm=T,mean)
    ggplot(stepsap, aes(interval,steps))+
         geom_line()+
         labs(x="Interval",y="Average Steps Taken",title="Average Steps Per Interval Across All Days")+
         theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Interval 835 has the most number of steps on average across days.

With Imputed Missing Values
===========================

First, count all the NAs in the dataset
---------------------------------------

    sapply(steps,function(x) sum(is.na(x)))

    ##    steps     date interval 
    ##     2304        0        0

Next, impute missing values with the mean by interval
-----------------------------------------------------

    imputem<-function(x) replace(x,is.na(x),mean(x,na.rm=T))
    steps2<-ddply(steps,~interval,transform,steps=imputem(steps))

Histogram of total steps taken per day (Imputed Missing Values)
---------------------------------------------------------------

    stepspd2<-aggregate(x=steps2[c("steps")],by=list(date=steps2$date),na.rm=T,FUN=sum,nfrequency=1)
    hist(stepspd2$steps,xlab="Steps",main="Total Steps Per Day (Mean-Imputed NAs)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### Compared to listwise deletion of NAs

    hist(stepspd$steps,xlab="Steps",main="Total Steps Per Day (Mean-Imputed NAs)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Summary statistics of steps taken per day (mean-imputed NAs)
------------------------------------------------------------

    summary(stepspd2$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      42    9820   10767   10767   12812   21195

### Compared to listwise deletion of NAs

    summary(stepspd$steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1    6779   10396    9355   12812   21195

\#Weekdays vs. Weekends

\#\#Plot of Number of Steps Taken in Weekdays and Weekend

    steps2$dow<-weekdays(as.Date(steps2$date))
    steps2$wknd<-factor(steps2$dow,
                levels=c("Monday","Tuesday","Wednesday",
                       "Thursday","Friday","Saturday","Sunday"),
                labels=c("Weekday","Weekday","Weekday",
                       "Weekday","Weekday","Weekend","Weekend"))

    stepsap2<-aggregate(x=steps2[c("steps")],by=list(interval=steps$interval,wknd=steps2$wknd),na.rm=T,mean)
    ggplot(stepsap2, aes(interval,steps))+
         geom_line()+
         labs(x="Interval",y="Average Steps Taken",title="Average Steps Per Interval By Weekdays and Weekends")+
         facet_grid(.~wknd) + facet_grid(rows=vars(wknd))+
         theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)
