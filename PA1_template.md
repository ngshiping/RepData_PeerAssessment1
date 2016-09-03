---
title: "Peer Graded Assignment: Course Project 1"
author: "ngshiping"
date: "September 3rd, 2016"
output: html_document
---
```{r setup, include=FALSE}
library(dplyr)
library(chron)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)
```

## Course Project Objetives

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip data [52K]

```{r}
myfile <- c("activity.csv")
setwd("/Volumes/MyMacData/R/Coursera Data Science Certification/Course V Reproducible Reserch/week 2")
activity.raw <- read.csv(myfile, header = TRUE, sep = ',', dec = '.')
```

## Assigment Parts
### 1. Loading and Preprocesing the data

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```{r}
str(activity.raw)
```

This file can be easily procesed by a read.csv command. The assigment indicastes that some parts of the assigment need the NA values to be ignored and some not. Also date variable is not in a required format for processing.

* Actions that we will take with the file:
    + Change date variable from String to Date format.
    + Two different strategies in different parts of the assigment with NAs depending on requirements:
        + Read file into a dataframe ignoring the NAs rows.
        + Read the file into a dataframe, process Interval variable and change NA values by 0.

```{r activity}

# Strategy I, remove rows with NAs
activity.no.na <- na.omit(activity.raw)
# Strategy II, change NAs to 0
activity.0.na <- activity.raw
activity.0.na$date <- as.Date(activity.0.na$date)
activity.0.na$steps[is.na(activity.0.na$steps)] <-0
```
### 2. Histogram of the total number of steps taken each day

As for this part of the assignment we can ignore the missing values in the dataset, let's use activity.no.na dataframe.

```{r histogram}
activity.by.date <- group_by(activity.no.na, date)
activity.date.sumsteps <- summarise(activity.by.date, sum(steps))
names(activity.date.sumsteps) <- c("Date", "SumSteps")
hist(activity.date.sumsteps$SumSteps, xlab="Total number of steps taken each day", ylab="Frequency", main="Histogram of total number of steps taken each day", col=2)
```

### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r mean}
the.mean <- mean(activity.date.sumsteps$SumSteps)
the.median <- median(activity.date.sumsteps$SumSteps)
```
```{r echo = FALSE}
cat("The mean of the total number of steps per day is:", the.mean, "and the median is:", the.median, ".")
```


### 4. Time series plot of the average number of steps taken

```{r}
activity.med <- group_by(activity.0.na, interval)
activity.med.tot <- summarise(activity.med, mean(steps))
names(activity.med.tot) <- c("Interval", "MeanSteps")
plot(activity.med.tot$Interval, activity.med.tot$MeanSteps, type="l", xlab="5 minute intervals", ylab="Average number of steps taken", main="Average number of steps taken in 5 minute intervals", col=2)
```

### 5. The 5-minute interval that, on average, contains the maximum number of steps

The 5-minute interval variable comes like the following,
some intervals ommited to avoid the long list of values...

```{r}
activity.med.tot$Interval[1:34]
activity.med.tot$Interval[100:133]
activity.med.tot$Interval[268:288]
```
These values are not in hexagesimal time system format. For this reason and for example, interval index number 13 correspond to value 100 instead of 60 (minutes) and so on. Having said this, the 5-minute interval that, on average, contains the maximum number of steps is calculated with the following lines.

```{r}
max.mean.value <- max(activity.med.tot$MeanSteps)
max.mean.value
which(activity.med.tot$MeanSteps == max.mean.value)
index.max.mean.value <- which(activity.med.tot$MeanSteps == max.mean.value)
cat("The 5-minute interval that, on average, contains the maximum number of steps is:", activity.med.tot$Interval[index.max.mean.value])
```
### 6. Code to describe and show a strategy for imputing missing data

The strategy is not sophisticated. It's just moving 0 to any NA value therein. First we have to make ourselves sure how many and where are the NAs. Funtions like mean, median or histogram arise different results depending on NA values omited or considered as 0. 

For all these actions, we have preserved the activity.raw dataframe as a "original copy" of the original csv file.

```{r}
sum(is.na(activity.raw$interval))
sum(is.na(activity.raw$date))
# These are the number of rows with NA in the step variable. 
sum(is.na(activity.raw$steps))
# So, finaly, we change date variable to Date format and change NA values on steps variable to 0.
activity.0.na <- activity.raw
activity.0.na$date <- as.Date(activity.0.na$date)
activity.0.na$steps[is.na(activity.0.na$steps)] <-0
sum(is.na(activity.0.na$steps))
```

### 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r}
activity.0.by.date <- group_by(activity.0.na, date)
activity.date.0.sumsteps <- summarise(activity.0.by.date, sum(steps))
names(activity.date.0.sumsteps) <- c("Date", "SumSteps")
hist(activity.date.0.sumsteps$SumSteps, xlab="Total number of steps taken", ylab="Frequency", main="Histogram of the total number of steps taken, NAs imputed", col=2)
```

```{r}
the.0.mean <- mean(activity.date.0.sumsteps$SumSteps)
the.0.median <- median(activity.date.0.sumsteps$SumSteps)
```
```{r echo = FALSE}
cat("The mean of the total number of steps per day is: ", the.0.mean, " and the median is: ", the.0.median)
```
The mean has became smaller that the mean with the NA rows ommited cause though the sum of the numerical values is the same, there are more (2304 more) observations to divide by to calculate the media. The median, for the same reason, is different.

### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
week.days <- is.weekend(activity.date.0.sumsteps$Date)
week.days
week.days[week.days %in% c("FALSE")] <- "weekday"
week.days[week.days %in% c("TRUE")] <- "weekend"
week.days
```
```{r}
weekdays.activity.med <- cbind(activity.0.na, week.days)
mean.by.weekdays <- weekdays.activity.med %>% group_by(week.days, interval) %>% summarize(MeanSteps=mean(steps))
qplot(interval, MeanSteps, data=mean.by.weekdays, geom="line", xlab="5 minute intervals", ylab="Average Number of Steps taken", main="Comparing the average number of steps taken per 5-minute \n interval on weekdays and weekends", color = "Red", facets =week.days ~ .)
```


