---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

#### **Download and unzip the zip file**

```r
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip',destfile = 'data.zip')
unzip('data.zip') # unzips the file activity.csv
```

#### **Read the csv**

```r
raw_data = read.csv('activity.csv')
num_rows = dim(raw_data)[1]
num_cols = dim(raw_data)[2]
```
The data is now loaded! Loaded in 17568 rows and 3 columns of data.

## What is mean total number of steps taken per day?

#### **Group the data by interval. Create the histogram**

```r
library(dplyr)
grouped_data = raw_data %>% group_by(date)
daily_steps = grouped_data %>% summarise(daily_steps = sum(steps,na.rm = TRUE))
hist(daily_steps$daily_steps,breaks = 10,labels = TRUE,xlab = 'Daily Step Count', ylab = '# of Days (Frequency)',main = 'Daily Step Count Histogram')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### **Calculate the mean and median**

```r
mean = mean(daily_steps$daily_steps,na.rm=TRUE)
median = median(daily_steps$daily_steps,na.rm=TRUE)
```
The mean daily step count is 9354.2295082. The median daily step count is 10395.


## What is the average daily activity pattern?

#### **Plot the average steps per interval**

```r
# use dplyr for some calculations
grouped_data_interval = raw_data %>% group_by(interval)
interval_avg_steps_df = grouped_data_interval %>% summarise(interval_avg_steps = mean(steps,na.rm = TRUE))
# create the plot
plot(interval_avg_steps_df$interval,interval_avg_steps_df$interval_avg_steps,type = 'l',xlab = 'Interval',ylab = 'Avg. Step Count (across all days)', main = 'Avg. Step Count over Intervals')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### **Find the interval with the max avg steps**

```r
max_index = which.max(interval_avg_steps_df$interval_avg_steps)
interval_max_avg_steps = data.frame(interval_avg_steps_df)[max_index,1]
step_count_max_avg_steps = data.frame(interval_avg_steps_df)[max_index,2]
```
The interval with the maximum average step count is 835 with 206.1698113 steps.


## Imputing missing values

#### **Find the number of null rows/values**

```r
total_nulls = sum(is.na(raw_data))
summary(raw_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
The summary aligns with the total_nulls calculation (2304 rows are null).

#### **Fix the null values by using the interval average step counts**

```r
revised_df = merge(data.frame(raw_data),interval_avg_steps_df,by.x = 'interval', by.y = 'interval')
# create a new_steps column that uses the interval average in the case that steps is na
revised_df = mutate(revised_df, steps_new = ifelse (is.na(steps), interval_avg_steps, steps))
grouped_data_2 = revised_df %>% group_by(date)
daily_steps_2 = grouped_data_2 %>% summarise(daily_steps = sum(steps_new))
# create the histogram
hist(daily_steps_2$daily_steps,breaks = 10,labels = TRUE,xlab = 'Daily Step Count', ylab = '# of Days (Frequency)',main = 'Daily Step Count Histogram')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### **Calculate the mean and median**

```r
new_mean = mean(daily_steps_2$daily_steps)
new_median = median(daily_steps_2$daily_steps)
```
The new mean is 1.0766189\times 10^{4}. The new median is 1.0766189\times 10^{4}.

## Are there differences in activity patterns between weekdays and weekends?

#### **Create the factor fields for weekend vs. weekday. Plot the results**

```r
# add the columns to denote weekday vs. weekend
revised_df = mutate(revised_df, day_of_week = weekdays(as.Date(revised_df$date)))
revised_df$week_day_end_flag = ifelse(revised_df$day_of_week %in% c('Saturday','Sunday'),'Weekend','Weekday')

# create a df that is grouped by interval, and the week day vs. end flag, and the avg steps for each
grouped_data_interval_weekday = revised_df %>% group_by(interval,week_day_end_flag)
interval_weekday_avg_steps_df = grouped_data_interval_weekday %>% summarise(interval_avg_steps = mean(steps_new))

# weekday df and weekend df
weekend_df = interval_weekday_avg_steps_df[interval_weekday_avg_steps_df$week_day_end_flag == 'Weekend',]
weekday_df = interval_weekday_avg_steps_df[interval_weekday_avg_steps_df$week_day_end_flag == 'Weekday',]

par(mfrow=c(2,1))
plot(weekday_df$interval,weekday_df$interval_avg_steps, main="Weekday",type = 'l',xlab = 'Interval', ylab = 'Avg. Step Count', ylim = c(0,200))
plot(weekend_df$interval,weekend_df$interval_avg_steps, main="Weekend",type = 'l',xlab = 'Interval', ylab = 'Avg. Step Count', ylim = c(0,200))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
