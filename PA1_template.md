---
title: "Course Project 1"
author: "Rick Hogoboom"
date: "5/30/2020"
output: 
  html_document: 
    keep_md: yes
---


## Reading and Editing Data
The only requirement for editing our initial dataset is to convert the date column from a character class to a date class.


```r
activitydata <- read.csv("./activity.csv")
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")
```

## What is the mean total number of steps taken per day?
The below code chunk calculates the number of steps taken per day and present it in a histogram format.


```r
sums <- activitydata %>% group_by(date) %>% summarize(steps = sum(steps, na.rm = TRUE)) 
sums_hist <- ggplot(sums, aes(steps)) +
     geom_histogram(fill = "purple", color = "black", binwidth = 1000) +
     theme_bw() +
     labs( x = "Steps per Day", y = "Frequency", title = "Histogram of Steps Taken per Day")

print(sums_hist)
```

![](PA1_template_files/figure-html/sumtabs-1.png)<!-- -->


We also are interested in the mean and median of this initial data set.


```r
sum_mean <- round(mean(sums$steps), 2)
sum_median <- median(sums$steps)
```

So our mean is 9354.23 and our median is 10395.

## Average Daily Activity Pattern

The below code takes the average for each given time interval throughout a day, and presents in an average time series format. 


```r
interval_means <- activitydata %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
max_inter <- interval_means[which.max(interval_means$steps),1]
max_steps <- round(interval_means[which.max(interval_means$steps),2], 2)

interval_means <- activitydata %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
line_interval_means <- ggplot(interval_means,  aes(interval,steps)) +
     geom_line(size = 1) +
     theme_bw() +
     labs(x = "Time Interval", y = "Average steps per interval", title= "Average Steps in One Day")

print(line_interval_means)
```

![](PA1_template_files/figure-html/intervalmeans-1.png)<!-- -->

As the graph above shows, the 5-minute interval with the highest average steps is interval 835 with 206.17 steps.

## Imputing Missing Values

First we need to calculate the number of missing values (they are all in the steps column).


```r
missingvalues <- sum(is.na(activitydata$steps))
```
There are 2304 missing values. 

I chose to replace the missing values with the average interval value of the dataset. The below code will create a new dataset imputing these values.


```r
imputed_data <- interval_means$steps[match(activitydata$interval, interval_means$interval)]
replaced_data <- transform(activitydata, steps = ifelse(is.na(activitydata$steps), yes = imputed_data, no = activitydata$steps))
```

Now we need to create a new histogram with our imputed values, and compare our summary statistics to their previous values. 

```r
sums2 <- replaced_data %>% group_by(date) %>% summarize(steps = sum(steps)) 

sum_mean2 <- format(round(mean(sums2$steps),2), scientific = FALSE)
sum_median2 <- format(round(median(sums2$steps),2), scientific = FALSE)

sums_hist <- ggplot(sums2, aes(steps)) +
     geom_histogram(fill = "blue", color = "black", binwidth = 1000) +
     theme_bw() +
     labs( x = "Steps per Day", y = "Frequency", title = "Histogram of Steps Taken per Day", subtitle = "With NA values replaced with interval mean")

print(sums_hist)
```

![](PA1_template_files/figure-html/secondsums-1.png)<!-- -->

We can tell that the means and medians have changed. The mean has shifted from 9354.23 to 10766.19 and the median has shifted from 10395 to 10766.19.

## Differences between Weekdays and Weekends?

To decide if there is a difference between weekdays and weekends in our data, first we need to create a new factor variable that will describe each observation correctly.


```r
replaced_data <- mutate(replaced_data, if_else(weekdays(replaced_data$date) == "Saturday" | weekdays(replaced_data$date) == "Sunday", 
                                               "Weekend",  "Weekday"))
names(replaced_data)[4] <- "dateclass"
replaced_data$dateclass <- as.factor(replaced_data$dateclass)
```
All done. For the last pair of graphs we are going to see the average interval per time interval, and plotted one on top of the other. 


```r
date_means <- replaced_data %>% group_by(interval, dateclass) %>% summarize(steps = mean(steps, na.rm = TRUE))

line_date_means <- ggplot(date_means,  aes(interval,steps, color = dateclass)) +
     geom_line(size = 1) +
     facet_grid(rows = vars(dateclass)) +
     labs(x = "Time Interval", y = "Average steps per interval", title= "Average Steps in One Day", subtitle = "Comparsion Between Weekdays and Weekends")

print(line_date_means)
```

![](PA1_template_files/figure-html/datemeans-1.png)<!-- -->

There does seem to be a difference in the two.  

It probably would have made sense to use these new values for our inputted values! 

# Thus concludes Rick's first attempt at RMarkdown. 

