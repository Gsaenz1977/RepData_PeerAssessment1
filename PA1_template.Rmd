---
title: 'Reproducible Research: Peer Assessment 1'
author: "Gilberto Saenz"
date: "August 19, 2016"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Please note that the repository containing all info can be found here: <https://github.com/Gsaenz1977/RepData_PeerAssessment1>

####Steps to complete the Assignment

####Initial step in to load the plotting library (ggplot2)

```{r}
library(ggplot2)
```

####Loading and processing the data
This step will save the data set in the workign directory
```{r}
if(!file.exists('activity.csv')){
        unzip('activity.zip')
}
data <- read.csv("activity.csv")
```


####What is mean total number of steps taken per day?

First we calculate the total steps provided and fequency, then the plot is displayed
```{r}
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

```{r, echo=FALSE}
 qplot(total.steps,geom="histogram", xlab="Total steps per day", ylab = "Frequency of Steps", binwidth=500)
```

The mean and median are calculated with the code below

```{r, results='hide'}
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```

* Mean: `r mean(total.steps, na.rm=TRUE)`
* Median:  `r median(total.steps, na.rm=TRUE)`

####What is the average daily activity pattern?
First the averages are calculated
```{r}
 averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
```


then the plot is displayed

```{r, echo=FALSE}
ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps")
```

####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The interval and steps are:
```{r}
averages[which.max(averages$steps),]
```

####Imputing missing values

There are many steps info that do have missing values (NA). These missing info could lead into incorrect analysis/preductions.

```{r}
missing <- is.na(data$steps)
```

Total missing data is:`r sum(missing)`

All missing values are filled out with the mean value for the specific 5-minute interval.

```{r}
fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
                filled <- c(steps)
        else
                filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

Then total steps are recalculated and histogram is displayed:
```{r}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
```
```{r,echo=FALSE}
qplot(total.steps,geom="histogram", xlab="Total steps per day", ylab = "Frequency of Steps", binwidth=500)
```

In addition, the new mean and median are:

* Mean: `r mean(total.steps)`
* Median:  `r median(total.steps)`

####Are there differences in activity patterns between weekdays and weekends?

First, we need to segregate  day of the week for each measurement in the dataset. In this part, we use the dataset with the filled-in values.

```{r}
weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Then the average is recalculted and the plot is displayed:
```{r}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
```
```{r,echo=FALSE}
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Average number of steps")

```

        
End of document
