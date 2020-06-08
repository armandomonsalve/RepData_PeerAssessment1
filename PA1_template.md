---
author: "Armando Monsalve"
date: "6/7/2020"
output: 
  html_document: 
    keep_md: yes
---

Peer-graded Assignment: Course Project 1
========================================




### Loading and preprocessing the data


```r
# Loading data
setwd("C:/Users/Armando Monsalve/Documents/Armando Monsalve/..DATA SCIENCE/..COURSES/Johns Hopkins University Data Science Course/5 Reproducible Research/Week 2/Course Project 1")
data_p1 = read.csv("activity.csv")
```

Checking data dimensions


```r
data_p1 %>%
        dim()
```

```
## [1] 17568     3
```

Checking first records of data:


```r
data_p1 %>%
        head()
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

Validating if there are any duplicates within each day for each interval:


```r
data_p1 %>%
        group_by(date, interval) %>%
        summarize(Count = n()) %>%
        arrange(desc(Count))
```

```
## # A tibble: 17,568 x 3
## # Groups:   date [61]
##    date       interval Count
##    <fct>         <int> <int>
##  1 2012-10-01        0     1
##  2 2012-10-01        5     1
##  3 2012-10-01       10     1
##  4 2012-10-01       15     1
##  5 2012-10-01       20     1
##  6 2012-10-01       25     1
##  7 2012-10-01       30     1
##  8 2012-10-01       35     1
##  9 2012-10-01       40     1
## 10 2012-10-01       45     1
## # ... with 17,558 more rows
```

*Intervals within days are unique (OK)*


Transforming date columns to date format using lubridate::ymd function


```r
data_p1$date = data_p1$date %>%
        ymd()

# Checking transformation:
data_p1$date %>% 
        class()
```

```
## [1] "Date"
```


### What is the mean of total number of steps taken per day?


```r
# Histogram of total steps per interval across all days
data_p1 %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps, na.rm = T)) %>%
        ggplot(aes(SumSteps)) + 
        geom_histogram(bins = 50)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Mean and Median of total of steps per day:


```r
# Mean
(data_p1 %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps, na.rm = T)))$SumSteps %>%
        mean()
```

```
## [1] 9354.23
```


```r
# Median
(data_p1 %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps, na.rm = T)))$SumSteps %>%
        median()
```

```
## [1] 10395
```


```r
# Or we can see the mean and median by applying summary to the calculated column "SumSteps". It should return same results as prior step/chunk
data_p1 %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps, na.rm = T)) %>%
        select(SumSteps) %>%
        summary()
```

```
##     SumSteps    
##  Min.   :    0  
##  1st Qu.: 6778  
##  Median :10395  
##  Mean   : 9354  
##  3rd Qu.:12811  
##  Max.   :21194
```


### What is the average daily activity pattern?


```r
data_p1 %>%
        group_by(interval) %>%
        summarize(AvgSteps = mean(steps, na.rm = T)) %>%
        ggplot(aes(interval, AvgSteps)) +
        geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Calculating max avg steps value
max_avg_steps = data_p1 %>%
        group_by(interval) %>%
        summarize(AvgSteps = mean(steps, na.rm = T)) %>%
        select(AvgSteps) %>%
        max()

# Indexing this max steps value in summarized data
data_p1 %>%
        group_by(interval) %>%
        summarize(AvgSteps = mean(steps, na.rm = T)) %>%
        filter(AvgSteps == max_avg_steps)
```

```
## # A tibble: 1 x 2
##   interval AvgSteps
##      <int>    <dbl>
## 1      835     206.
```

The 835 5-min interval across all days has the highest number of steps on average.


### Imputing missing values

1. Calculating how many NA values we have:


```r
sapply(lapply(data_p1, is.na),table)
```

```
## $steps
## 
## FALSE  TRUE 
## 15264  2304 
## 
## $date
## 
## FALSE 
## 17568 
## 
## $interval
## 
## FALSE 
## 17568
```

There are 2,304 NA values and are all present in the "steps" column.

2. Devising a strategy for imputing these NA values:


```r
data_p1 %>%
        group_by(interval) %>%
        summarize(AvgSteps = mean(steps, na.rm = T)) %>%
        ggplot(aes(interval, AvgSteps)) + 
        geom_line() + 
        labs(title = "Mean per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
data_p1 %>%
        group_by(interval) %>%
        summarize(MedSteps = median(steps, na.rm = T)) %>%
        ggplot(aes(interval, MedSteps)) + 
        geom_line() + 
        labs(title = "Median per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

The difference between the mean and the median plot shows there are many outlier values within multiple intervals. We could plot boxplots for some of these intervals to prove this hipothesis:


```r
data_p1 %>%
        filter(interval >= 750 & interval <= 1750) %>%
        ggplot(aes(as.factor(interval),steps)) + 
        geom_boxplot(na.rm = T) + 
        labs(title = "Outliers per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

As we can see, there are numerous values outside each interval boxplot so, in my opinion, it'd be better to use median values compared to using mean values for each interval se we don't introduce bias when imputing missing values.


3. Imputing missing values with median for each interval across all days:


```r
for (i in 1:nrow(data_p1)) {
        if (is.na(data_p1[i,]$steps)) {
                data_p1$steps_imp[i] = 
                        (data_p1 %>%
                        group_by(interval) %>%
                        summarize(MedSteps = median(steps, na.rm = T)) %>%
                         filter(interval == data_p1[i,]$interval))$MedSteps
        } else {
                data_p1$steps_imp[i] =
                        data_p1[i,]$steps
        }
}

# Creating a new data set with imputed values according to asignment:
data_p1_imp = data_p1[,2:4]

# Eliminating new variable on original data
data_p1$steps_imp = NULL
```


```r
# Checking if new data set has any NA values
any(is.na(data_p1_imp))
```

```
## [1] FALSE
```


4. Making a histogram of the total number of steps taken each day and calculating the mean and median total number of steps taken per day.


```r
# Histogram of total number of steps taken each day:
data_p1_imp %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps_imp)) %>%
        ggplot(aes(SumSteps)) + 
        geom_histogram(bins = 50) +
        labs(title = "New Data with Imputation")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

Comparing to histogram of data without imputation:

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Mean and median of the sum of steps per day for original data
data_p1 %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps, na.rm = T)) %>%
        select(SumSteps) %>%
        summary()
```

```
##     SumSteps    
##  Min.   :    0  
##  1st Qu.: 6778  
##  Median :10395  
##  Mean   : 9354  
##  3rd Qu.:12811  
##  Max.   :21194
```

```r
# Mean and median of the sum of steps per day for imputed data
data_p1_imp %>%
        group_by(date) %>%
        summarize(SumSteps = sum(steps_imp)) %>%
        select(SumSteps) %>%
        summary()
```

```
##     SumSteps    
##  Min.   :   41  
##  1st Qu.: 6778  
##  Median :10395  
##  Mean   : 9504  
##  3rd Qu.:12811  
##  Max.   :21194
```

The differences between these two data sets for mean and median are essentially on the mean value since it went from 9354 to 9504. Moreover, on the median value, it stayed the same (10395). Going further, the minimum value changed from 0 to 40, but since the most important quartiles stayed the same I consider that the imputation didn't add bias to our data.


### Are there differences in activity patterns between weekdays and weekends?


```r
# Creating a factor variable to categorize each day as "weekday" or "weekend"
data_p1_imp$day_of_week = ifelse(weekdays(data_p1_imp$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),
                                 "Weekday", 
                                 "Weekend") %>%
        as.factor()

# Checking how many of these categories are in our set:
data_p1_imp$day_of_week %>%
        table()
```

```
## .
## Weekday Weekend 
##   12960    4608
```

Creating a panel plot for the average steps per 5-min interval for weekdays and weekends:


```r
data_p1_imp %>%
        group_by(day_of_week,interval) %>%
        summarize(AvgSteps = mean(steps_imp)) %>%
        ggplot(aes(interval,AvgSteps)) +
        geom_line() +
        facet_grid(day_of_week ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

What we see between these two categories of days is that they present the highest spikes in average steps at the 870-880 5-minute intervals. Moreover, for weekend days, spikes in average steps can get very close to the maximum average number of steps in comparison to weekdays where we can see a big spike and small spikes for the rest of the day.

Also, we can see that for weekends there are more sharp spikes (average steps > 80) compared to weekdays where we can see far less spikes that go above ~80 steps in average. This could be explained by assuming that on weekdays people walk more on certain time slots due to jobs; and, on the other hand, on weekends people would rather walk/exercise at different times of the day. 



