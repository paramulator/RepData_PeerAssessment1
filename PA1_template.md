---
title: 'Reproducible Research: Peer Assessment 1'
output:
  pdf_document:
    toc: yes
  html_document:
    keep_md: yes
    theme: cerulean
    toc: yes
---



## Assignment background

Devices marketed by vendors such as Nike and others are able to collect personal
activity data such as the number of steps taken each day.  The dataset for this assignment
is the record of walking activity collected from an anonymous user of one of these
devices.  It contains the number of steps taken during each 5 minute period in the 
months of October and November, 2012.

The assignment seeks to extract some basic features of the data such as the distribution
of daily step totals, the average number of steps taken during each 5-minute time period
within a day, the prevalence and effect of missing values on these features, the subsequent
effect that imputation strategies can have, and finally a comparison of weekday and
weekend step patterns.

---

## Load packages

These packages are required to repeat the results of this analysis:


```r
library(dplyr)
library(ggplot2)
library(knitr)  # For simple html tables.
```

---

## Define helper functions

The raw data describing the 5 minute time interval is actually a concatenation of hours
and minutes represented as an integer.  The functions defined here are used to transform
the raw `interval` variable into forms that are more approriate for this analysis. 


```r
# Helper function to convert raw integer time interval variable into a numeric 
# decimal hours representation.
to.hour <- function(interval){
    interval.char <- sprintf("%04d", interval)
    as.numeric(substr(interval.char, 1, 2)) + as.integer(substr(interval.char, 3, 4)) / 60.0
}

# Helper function to convert raw integer time interval variable into a 
# character HH:MM display.
to.hhmm <- function(interval){
    interval.char <- sprintf("%04d", interval)
    paste0(substr(interval.char, 1, 2), ":", substr(interval.char, 3, 4))
}
```

---

## Load raw data

The data for this assignment can be found [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).
It was downloaded and unzipped in to the current working directory for this project.
The structure of the raw dataset data appears below.  In particular notice the jump in values for the
`interval` variable bewteen values `55` and `100`.


```r
rawData <- read.csv("activity/activity.csv", header = TRUE, sep = ",", 
                    col.names = c("steps", "date", "interval"),
                    colClasses = c("integer", "Date", "integer")) %>% tbl_df %>% 
            mutate(weekday = weekdays(date), hour = to.hour(interval), hhmm = to.hhmm(interval))
glimpse(rawData)
```

```
## Observations: 17,568
## Variables: 6
## $ steps    (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ date     (date) 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012...
## $ interval (int) 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 10...
## $ weekday  (chr) "Monday", "Monday", "Monday", "Monday", "Monday", "Mo...
## $ hour     (dbl) 0.00000000, 0.08333333, 0.16666667, 0.25000000, 0.333...
## $ hhmm     (chr) "00:00", "00:05", "00:10", "00:15", "00:20", "00:25",...
```

---

## Mean steps per day

What is the mean total number of steps taken per day?

First, we'll group the raw data by day and sum the number of steps taken across all 5 minute 
time periods within each day, ignoring for now any periods where the data are missing.  From there 
we can generate a histogram of the daily step totals so we can see how variable these are.


```r
stepsPerDay <- rawData %>% 
    group_by(date) %>% summarise(totalSteps = sum(steps, na.rm = TRUE))

ggplot(stepsPerDay) +
    geom_histogram(aes(x = totalSteps), 
                   binwidth = 1000, colour = "black", fill = "gray") +
    xlab("Steps per Day") + ylab("Number of Days") +
    scale_y_continuous(breaks = seq(0, 10, 1)) +
    ggtitle("Histogram of Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Notive the tendency for daily step counts to be concetrated between 10,000 and
15,000 steps per day.  Notice also that there is a tall bar on the far left of the histogram.  This happens 
because there are 10 days with a step total
less than 1,000.  Of those days, 8 have a 
step total of exactly zero.  As we'll see later, there are many 5-minute time periods
where the step count is missing, and evidently some days where nearly all of the counts
are missing.

Given the daily step totals we can compute the mean and median values.

```r
statsPerDay <- data.frame("Statistic" = c("Mean", "Median"), 
                          "Value" = c(mean(stepsPerDay$totalSteps), 
                                      quantile(stepsPerDay$totalSteps, prob = 0.5)),
                          row.names = NULL)

# A knitr function for printing html tables.
kable(statsPerDay, digits = 1, align = c("l", "l"), 
      caption = "Total Steps per Day", format.args = list(decimal.mark = ".", big.mark = ","))
```



|Statistic |Value    |
|:---------|:--------|
|Mean      |9,354.2  |
|Median    |10,395.0 |

With $Mean < Median$ and given the histogram we can see that the daily step totals 
are slightly skwewed to the left.  Again, missing values are probably influencing the
shape of the data.

---

## Average daily activity

What is the average daily activity pattern?

First lets take a look at the average pattern of steps taken throughout a day.
Starting with the raw data, we'll re-group everything by 5-minute time periods and 
then compute the average (across all of the days) of the number of steps taken in each 
period.  For now we'll ignore all of the missing values.

**Note:** I have chosen to transform the original `interval` variable in the raw data 
into an  equivalent `hour` variable.  The `interval` variable in raw form is really
the concatenation of hours and minutes stored in integer form.  Its not appropriate
for an accurate representation of time so I created a function called `to.hour` that translates the
raw `interval` variable into a decimal representation of hours.  I use this variable for plotting
and as you can see it facilitates an easy interpretation of typical intra-day activity.


```r
stepsPerTimePeriod <- rawData %>%
    group_by(hour, hhmm) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

ggplot(stepsPerTimePeriod) +
    geom_line(aes(x = hour, y = meanSteps)) + scale_x_continuous(breaks = seq(0, 24, 1)) +
    xlab("Hour") + ylab("Mean Number of Steps") + ggtitle("Mean Number of Steps Taken Every 5 Minutes") 
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The plot shows the average number of steps taken every 5 minutes, so there are going to 
be 12 data points every hour.  In the plot the individual points are over-layed by the line that connects 
them together.  With data at this level we can find the beginning of the 5-minute time period 
with the maximum average number of steps taken.


```r
maxTimePeriod <- stepsPerTimePeriod %>% ungroup %>% 
    arrange(desc(meanSteps)) %>% slice(1) %>% select(hhmm, meanSteps)
colnames(maxTimePeriod) <- c("Time of Day", "Mean Number of Steps")
kable(maxTimePeriod, digits = c(0, 1), align = c("c", "c"),
      caption = "Most Active Time Period", format.args = list(decimal.mark = ".", big.mark = ","))
```



| Time of Day | Mean Number of Steps |
|:-----------:|:--------------------:|
|    08:35    |        206.2         |

---

## Imputing missing values

The number of 5-minute time periods with missing step counts is shown below.  The missing values
can affect the shape of the raw data as well as any computed statistics.


```r
numberMissing <- sum(is.na(rawData$steps))
names(numberMissing) <- "Periods with missing step counts"
print(numberMissing)
```

```
## Periods with missing step counts 
##                             2304
```

My imputation scheme is to replace each missing value with the average number of steps
taken for each day of week and time of day combination.


```r
meanDayTime <- rawData %>% group_by(weekday, hour) %>%
                summarize(meanSteps = mean(steps, na.rm = TRUE)) 
impData <- rawData %>% inner_join(meanDayTime, by = c("weekday", "hour")) 
impData$steps[is.na(impData$steps)] <- impData$meanSteps[is.na(impData$steps)] 
```

After replacing NAs with imputed values, we can compare the distributions of total steps per day
between the raw and imputation-adjusted datasets.


```r
impStepsPerDay <- impData %>% 
    group_by(date) %>% summarise(totalSteps = sum(steps))

ggplot(impStepsPerDay) +
    geom_histogram(aes(x = totalSteps), 
                   binwidth = 1000, colour = "black", fill = "gray") +
    xlab("Steps per Day") + ylab("Number of Days") +
    scale_y_continuous(breaks = seq(0, 10, 1)) +
    ggtitle("Histogram of Steps per Day Using Imputed Values")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

A comparison of this histogram with the previous one suggests that the overall distibution of steps
per day has been shifted to the right **as a result of the imputation**, with an
increased concentration in days with step counts above 10,000.  In 
particular, its clear that the number of zero-step days has been  
reduced from 8 to 
0, resulting in the number of days with 
less than 1,000 steps dropping from 10 
to 2.

Here are the mean and median steps per day based on the imputation-adjusted data.


```r
impStatsPerDay <- data.frame("Statistic" = c("Mean", "Median"), 
                          "Value" = c(mean(impStepsPerDay$totalSteps), 
                                      quantile(impStepsPerDay$totalSteps, prob = 0.5)),
                          row.names = NULL)

kable(impStatsPerDay, digits = 1, align = c("l", "l"), 
      caption = "Total Steps per Day", format.args = list(decimal.mark = ".", big.mark = ","))
```



|Statistic |Value    |
|:---------|:--------|
|Mean      |10,821.2 |
|Median    |11,015.0 |

Comparing these statistics with those for the raw data indicates that the 
distribution of the number of steps per day has indeed been shifted to the right, 
with an increase in the mean by 1466.98 steps per day, and an increase in the median by 620
steps per day.

---

## Weekday vs. weekend activity patterns

Are there differences in activity patterns between weekdays and weekends?

First we'll add a factor indicating whether or not a given date in the data falls on a 
weekday or weekend.  This will be based on the `weekday` variable that was added when 
the raw data was first read in.


```r
impData2 <- impData %>% 
    mutate(weekendInd = factor(weekday %in% c("Saturday", "Sunday"), 
                               labels = c("Weekday", "Weekend")))
```

A plot of the average number of steps taken per time period, faceted by this new factor will 
contrast the step patterns for weekdays vs. weekends.  


```r
stepsPerTimePeriod2 <- impData2 %>%
    group_by(weekendInd, hour, hhmm) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

ggplot(stepsPerTimePeriod2) +
    geom_line(aes(x = hour, y = meanSteps)) + scale_x_continuous(breaks = seq(0, 24, 1)) +
    facet_wrap(~weekendInd, ncol = 1) +
    xlab("Hour") + ylab("Mean Number of Steps") +
    ggtitle("Mean Number of Steps Taken Every 5 Minutes")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

From the plots we can see that walking intensity is more uniformly spread throughout
the day on weekends and more concentrated in the early hours during weekdays. It also appears 
that more late-evening walking takes place on weekends compared to weekdays.

---

## Summary

Weekday and weekend walking patterns vary with more uniform patterns throughout the day on weekend days
and more early walking taking place in the early hours of weekdays.  Walking intensity is concentrated
between 10K and 15K steps per day but is affected by the quality of the raw data.  An imputation
strategy to replace NAs in the raw data with average values results in a slight skew to the right of the
original dataset.
