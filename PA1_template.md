# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1.Load the data (i.e.  read.csv() )

```r
UserPath <- "./"
if(!dir.exists(UserPath)) UserPath <- "./"
data <- read.csv(paste(UserPath,"activity.csv",sep=""), header=TRUE)
```

2. transform the data (if necessary) into a format suitable for your analysis

```r
steps_per_day <- aggregate(formula=steps~date, data = data, FUN=sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, main = "Total number of steps taken each day", xlab = "Number of steps",  col = "blue" )
```

![](PA1_template_files/figure-html/histogram_steps_per_day-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken per day

```r
Mean_total_number_of_steps_per_day <- mean(steps_per_day$steps)
Mean_total_number_of_steps_per_day
```

```
## [1] 10766.19
```

```r
Median_total_number_of_steps_per_day <- median(steps_per_day$steps)
Median_total_number_of_steps_per_day
```

```
## [1] 10765
```
## What is the average daily activity pattern?

0. Assesment of the steps average

```r
steps_average <- aggregate(formula = steps~interval, data = data, FUN = mean)
```

1. Make a time series plot (i.e.  type = "l" ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(steps_average$interval, steps_average$steps, type="l", 
     main="Time series of the plot of the 5 minute interval and the average number of steps", col = "red") 
```

![](PA1_template_files/figure-html/Time_series_plot-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps_average<-(steps_average$steps)
steps_average_subset <- subset(steps_average, steps == max_steps_average)
steps_average_subset$interval
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105
##  [15]  110  115  120  125  130  135  140  145  150  155  200  205  210  215
##  [29]  220  225  230  235  240  245  250  255  300  305  310  315  320  325
##  [43]  330  335  340  345  350  355  400  405  410  415  420  425  430  435
##  [57]  440  445  450  455  500  505  510  515  520  525  530  535  540  545
##  [71]  550  555  600  605  610  615  620  625  630  635  640  645  650  655
##  [85]  700  705  710  715  720  725  730  735  740  745  750  755  800  805
##  [99]  810  815  820  825  830  835  840  845  850  855  900  905  910  915
## [113]  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
is_na_data<-is.na(data)
is_na_data_res <- colSums(is_na_data)
is_na_data_res[[1]]
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#data$newSteps <- averageSteps$steps
steps_median <- aggregate(formula = steps~date, data = data, FUN = median)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_dataset<- data
new_dataset$steps[is_na_data[,1]] <- steps_average$steps
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
new_steps_sum <- aggregate(formula = steps~date, data = new_dataset, FUN = sum)
```


```r
hist(new_steps_sum$steps,main = "Total number of steps taken each day", xlab = "Number of steps with filling values",  col = "green" )
```

![](PA1_template_files/figure-html/histogram_imputed_values-1.png)<!-- -->


```r
new_Mean_total_number_of_steps_per_day <- mean(new_steps_sum$steps)
new_Mean_total_number_of_steps_per_day
```

```
## [1] 10766.19
```

```r
new_Median_total_number_of_steps_per_day <- median(new_steps_sum$steps)
new_Median_total_number_of_steps_per_day
```

```
## [1] 10766.19
```


```r
new_Mean_total_number_of_steps_per_day - Mean_total_number_of_steps_per_day
```

```
## [1] 0
```

```r
new_Median_total_number_of_steps_per_day - Median_total_number_of_steps_per_day
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data_days <- weekdays(as.Date(data$date))
data_days_number <- as.POSIXlt(data$date)$wday
new_dataset$Week_day_end <- "weekday"
new_dataset$Week_day_end[data_days_number > 5] <- "weekend"
new_dataset$Week_day_end <- factor(new_dataset$Week_day_end)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
weekdays <- subset(new_dataset, new_dataset$Week_day_end == "weekday")
weekdays_average <- aggregate(formula = steps ~ interval, data = weekdays, FUN = mean)

weekends <- subset(new_dataset, new_dataset$Week_day_end == "weekend")
weekends_average <- aggregate(formula = steps ~ interval, data = weekends, FUN = mean)
```



```r
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(weekdays_average$interval, weekdays_average$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekdays", col ="red", type="l") 

plot(weekends_average$interval, weekends_average$steps
     , xlab = "interval", ylab = "Number of steps"
     , main ="Weekends", col ="green", type="l")
```

![](PA1_template_files/figure-html/panel-1.png)<!-- -->
