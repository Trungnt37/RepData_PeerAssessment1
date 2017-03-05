---
title: "Reproducible-Research - Project 1"
author: "Nguyen Thanh Trung"
date: "March 5, 2017"
output: html_document
variant: markdown_github
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.  

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day

## Download and import data  
```{r, echo = TRUE}
setwd("D:/Learning/DataScience/05ReproducibleResearch")
filename <- "repdata.zip"
if(!file.exists(filename)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, destfile = filename, method = "auto")    
}
if(!file.exists("activity.csv")){
    unzip(filename)
}

activity <- read.csv("activity.csv", head = TRUE, sep = ",", na.strings = NA)
activity$steps <- as.integer(activity$steps)
activity$date <- as.Date(activity$date)
activity$interval <- as.integer(activity$interval)
head(activity)
summary(activity)
```  
  
## Histogram of the total number of steps taken each day  
```{r, echo = TRUE}
activity_rmna <- na.omit(activity)
stepseachday <- aggregate(steps ~ date, activity_rmna, FUN = sum)
hist(stepseachday$steps, breaks = 20, col = 'green',
     main = 'Total number of steps taken each day',
     xlab = 'Steps')
```  
  
## Mean and median number of steps taken each day  
```{r, echo = TRUE}
meansteps <- mean(stepseachday$steps)
meansteps
mediansteps <- median(stepseachday$steps)
mediansteps
```  
So, the mean number of steps taken each day is `r meansteps` and the median is `r mediansteps`  
  
## Time series plot of the average number of steps taken  
```{r, echo = TRUE}
Avgsteps <- aggregate(steps~ interval, activity_rmna, FUN = mean)
names(Avgsteps) <- c('interval', 'avgsteps')
plot(Avgsteps$avgsteps ~ Avgsteps$interval, type = 'l',
     main = 'Average number of steps taken',
     xlab = 'Time interval',
     ylab = 'Average number of steps')
```  
  
## The 5-minute interval that, on average, contains the maximum number of steps  
```{r, echo = TRUE}
Avgsteps[Avgsteps$avgsteps == max(Avgsteps$avgsteps), ]
```  
The interval with the largest average number steps was at 835 and an average of 206 steps 
  
## Imputing missing values and show a strategy for imputing missing data  
#### 1. Calculate total number of missing values in the dataset  
```{r, echo = TRUE}
sum(is.na(activity$steps))
```  
#### 2. Imputing missing values using mean for each day  
```{r, echo = TRUE}
activity2 <- activity
na <- is.na(activity2$steps)
Avg <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify=T)
activity2$steps[na] <- Avg[as.character(activity2$interval[na])]
stepseachday2 <- aggregate(steps ~ date, activity2, FUN = sum)
hist(stepseachday2$steps, breaks = 20, col = 'green',
     main = 'Total number of steps taken each day',
     xlab = 'Steps')
```  
  
## Panel plot comparing the average number of steps taken per 5-minute interval across  weekdays and weekends  
```{r, echo = TRUE}
dayofweek <- weekdays(activity2$date)
day <- ifelse(dayofweek == 'Saturday'|dayofweek == 'Sunday', 'weekend', 'weekday')
activity3 <- cbind(activity2, dayofweek, day)
head(activity3)

Avgsteps2 <- aggregate(steps~ interval + day, activity3, FUN = mean)
names(Avgsteps2) <- c('interval', 'day', 'avgsteps')
par(mfrow = c(2,1))
with(Avgsteps2[Avgsteps2$day == 'weekday', ],
     plot(avgsteps ~ interval, type = 'l',
        main = 'Average number of steps taken - Weekday',
        xlab = 'Time interval',
        ylab = 'Average number of steps'))
with(Avgsteps2[Avgsteps2$day == 'weekend', ],
     plot(avgsteps ~ interval, type = 'l',
          main = 'Average number of steps taken - Weekend',
          xlab = 'Time interval',
          ylab = 'Average number of steps'))
``` 