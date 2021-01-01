---
output:
  pdf_document: default
  html_document: default
  word_document: default
---
Coursera Reproducible research Assignment 1
==============================================

```{r echo = FALSE}
setwd('C:/Users/huguel/Documents/Training/R/Coursera/Reproducible Research/Reproducible_Research_P1')
```

### Import data from local drive as CSV format and do a quick discovery
```{r}
df <- read.csv('activity.csv', header = TRUE)

#data discovery 
str(df)
min(df$date)
max(df$date)
max(df$interval)
head (df)

# number of NA rows out of the 17568 obs.
sum( as.numeric(as.logical(is.na(df$steps))))
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

Make a histogram of the total number of steps taken each day

we will use library(dplyr)

```{r}
library(dplyr)
# create new dataset by ignoring null values and adding the daily sum of steps
df2 <- df %>% subset(steps != 'NA') %>% group_by(date) %>% mutate (steps_p_day = sum(steps))

# generate related histogram
hist (df2$steps_p_day
        , main = "Histogram of total number of steps per day"
        , xlab = 'Nb of steps per day')
```

Calculate and report the mean and median of the total number of steps taken per day
```{r}
(df2$mean_steps_p_day_tot <- mean(df2$steps_p_day))
(df2$med_steps_p_day_tot  <- median(df2$steps_p_day))

# generate the histogram of the mean and median of the total steps per day
df2 <- df2 %>% group_by(date) %>% mutate (mean_steps_p_day = mean(steps), med_steps_p_day = median(steps))

hist (df2$mean_steps_p_day
        , main = "Histogram of the MEAN number of steps per day"
        , xlab = 'Nb of steps per day')

hist (df2$med_steps_p_day
        , main = "Histogram of the MEDIAN number of steps per day"
        , xlab = 'Nb of steps per day')
```
we can see from above charts that the mode of the average number of steps per day is between 35 and 40
when the median is at 0

### What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)

we will use library ggplot2
```{r}
library(ggplot2)

# change date field into date type and identify x axis range
df2$date <-  as.Date(df2$date, "%Y-%m-%d")
mx_dt <- max(df2$date)
mn_dt <- min(df2$date)

# graph steps per days
ggplot(df2, aes(x = date, y = mean_steps_p_day)) +
      xlim  (mn_dt, mx_dt) +
      geom_line () +
      geom_smooth ()
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps 
```{r}
df2 <- df2 %>% group_by(interval) %>% mutate (steps_p_interv_mean = mean(steps))
unique(df2[df2$steps_p_interv_mean == max(df2$steps_p_interv_mean), 3])

```
therefore the interval with the maximum steps is 835

### Imputing missing Values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
# number of missing values
sum (as.numeric(as.logical(is.na(df$steps))))

# Percentage of steps with value = 0 over non null steps values
sum (as.numeric(as.logical(!is.na(df$steps) & df$steps == 0)))/sum (as.numeric(as.logical(!is.na(df$steps))))

```
Devise a strategy for filling in all of the missing values in the dataset
 - we will use the mean steps per interval to fill in the gaps (NA)

Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r}
# start by simplifying the df2 with the mean data 
df_mean <- df2 %>% select (interval,steps_p_interv_mean) %>% distinct (interval,steps_p_interv_mean)

# create joined (imputed) dataset by joining df with df_mean on interval to include the mean per interval on missing data
df_joined <- inner_join(df,df_mean, by = "interval")

# fill in the gaps with 'mean steps per interval' in  df_joined 
df_joined$steps [which (is.na(df_joined$steps))] <- df_joined$steps_p_interv_mean [which (is.na(df_joined$steps))]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
```{r}
# create new columns with sum, mean, med steps for the imputed dataset
df_joined <- df_joined %>% 
              group_by(date) %>% 
              mutate (steps_p_day_sum = sum(steps), 
                      steps_p_day_mean = mean(steps),
                      steps_p_day_med = median(steps))

# Percentage of steps with value = 0 over non null steps values
sum (as.numeric(as.logical(!is.na(df$steps) & df_joined$steps == 0)))/sum (as.numeric(as.logical(!is.na(df_joined$steps))))

df_joined %>% 
          select (date,steps_p_day_sum,steps_p_day_mean,steps_p_day_med) %>% 
           distinct (date,steps_p_day_sum,steps_p_day_mean,steps_p_day_med)

hist (df_joined$steps_p_day_mean
        , main = "Histogram of MEAN steps per day"
        , xlab = 'mean steps per day')
hist (df_joined$steps_p_day_med
        , main = "Histogram of MEDIAN steps per day"
        , xlab = 'median steps per day')
```

Do these values differ from the estimates from the first part of the assignment? 
```{r}
# sum steps per day prior to imputation
prior <- sum(df2$steps_p_day)
# sum steps per day after imputation
after <- sum(df_joined$steps_p_day_sum)
# increase in steps per day after vs prior imputation (in pct)
(after/prior-1) * 100

# median steps per day after imputation
(df_joined$med_steps_p_day_tot  <- median(df_joined$steps_p_day_sum))
```
In order to impute the missing data, we have used the mean steps per interval
 - regarding the mean histogram, the overall shape is very similar to the one without the imputation, except that the mode is way higher.
 - regarding the median data, the first histogram is only 0 as more than 50% of the data which is not null have 0 values
 - regarding the sum of daily steps, we can see an increase of 15.09% due to the imputation

What is the impact of imputing missing data on the estimates of the total daily number of steps?
sum of steps per day has drastically increased has null values have been replaced by the interval mean usually higher than 0


## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels 
“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` {r}
# change date field into date data type 
df_joined$date <-  as.Date(df_joined$date, "%Y-%m-%d")
# add day name field
df_joined$wd_nm <- weekdays(df_joined$date)
# add week day flag with 'wd' for week day as default
df_joined$wd_flag <- 'wd'
# correct flag default with proper value 'we' for week end days
df_joined$wd_flag[df_joined$wd_nm == 'Saturday' | df_joined$wd_nm == 'Sunday'] <- 'we'

```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r}
df_joined <- df_joined %>% group_by (wd_flag) %>% mutate (mean_wd = mean(steps))

mx_int <- max(df_joined$interval)
lab_char_vect <- c (wd = 'weekday', we = 'weekend')

ggplot(df_joined, aes(x = interval, y = steps)) +
      xlim  (0, mx_int) +
      geom_line (color = 'steelblue') +
      theme(strip.background = element_rect(fill="antiquewhite"),
            panel.background = element_rect(fill= 'white')) +
      facet_grid (wd_flag ~.,  labeller = labeller (wd_flag = lab_char_vect)) +
      labs (title = 'average number of steps taken, across weekday or weekend days', x = 'Interval', y = 'Number of steps')
```