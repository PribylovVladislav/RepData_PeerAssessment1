## R version 3.6.0 (2019-04-26)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1


library(dplyr) # version 0.8.5
library(ggplot2) # version 2_3.3.0
library(chron) # version 2.3-55

Sys.setlocale("LC_ALL","English")

### Loading and preprocessing data ###

if(!file.exists('./data/Activity_monitoring_data.zip')) {
    if(!dir.exists('./data')) {dir.create('./data')}
    fileURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
    download.file(fileURL, './data/Activity_monitoring_data.zip')
    dir.create('./data/Activity_monitoring_data')
    unzip('./data/Activity_monitoring_data.zip', exdir = './data/Activity_monitoring_data')
}

Activity <- read.csv('./data/Activity_monitoring_data/activity.csv')

### Data processing ###

# What is mean total number of steps taken per day?

Activity_SPD <- Activity %>% group_by(date) %>% summarize(SPD = sum(steps))
histogram <- ggplot(Activity_SPD, aes(x = SPD)) + geom_histogram(fill = 'yellowgreen', color = 'black') +
    labs(title = 'Total number of steps taken each day', x = 'Steps per day', y = 'Number of days') + theme_bw()  
print(histogram)
SPD_mean <- mean(Activity_SPD$SPD, na.rm = TRUE)
SPD_median <- median(Activity_SPD$SPD, na.rm = TRUE)
rm(Activity_SPD)

# What is the average daily activity pattern?

Activity_ADA <- Activity %>% group_by(interval) %>% summarize(step_mean = mean(steps, na.rm = TRUE))
average_plot <- ggplot(Activity_ADA, aes(x = interval, y = step_mean)) + geom_line(colour = 'turquoise') + 
    labs(title = 'Average Daily Steps', x = 'Interval', y = 'Number of steps') + theme_bw()
print(average_plot)
max_interval <- as.numeric(Activity_ADA[Activity_ADA$step_mean == max(Activity_ADA$step_mean) ,1])

# Imputing missing values

NA_count <- sum(is.na(Activity$steps))
Activity_merge <- merge(Activity, Activity_ADA, by = 'interval', all.x = TRUE, all.y = TRUE)
Activity_NA <- Activity_merge %>% subset(is.na(steps)) %>% select(-steps) %>% rename(steps = step_mean)
Activity_merge <- Activity_merge %>% select(-step_mean) %>% subset(!is.na(steps))
Activity_imputed <- rbind(Activity_NA, Activity_merge)
Activity_imputed$steps <- round(Activity_imputed$steps)
rm(Activity_NA, Activity_merge, Activity_ADA)

SPD_imputed <- Activity_imputed %>% group_by(date) %>% summarize(SPD = sum(steps))
histogram_imputed <- ggplot(SPD_imputed, aes(x = SPD)) + geom_histogram(fill = 'yellowgreen', color = 'black') +
    labs(title = 'Total number of steps taken each day', x = 'Steps per day', y = 'Number of days') + theme_bw()
print(histogram_imputed)
mean_imputed <- mean(SPD_imputed$SPD)
median_imputed <- median(SPD_imputed$SPD)
rm(SPD_imputed)

# Are there differences in activity patterns between weekdays and weekends?

Activity_imputed$weekdays <- !is.weekend(as.Date(Activity$date))
Activity_imputed <- Activity_imputed %>% group_by(interval, weekdays) %>% summarize(step_mean = mean(steps))
Activity_imputed$weekdays[Activity_imputed$weekdays == 'TRUE'] <- 'weekday'
Activity_imputed$weekdays[Activity_imputed$weekdays == 'FALSE'] <- 'weekend'
panel <- ggplot(Activity_imputed, aes(x = interval, y = step_mean, color = weekdays)) + geom_line() + 
    labs(title = 'Average Daily Steps by Weektype', x = 'Interval', y = 'Number of steps') + 
    facet_grid(weekdays ~ ., ) + theme_bw()
print(panel)