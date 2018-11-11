library(tidyverse)
library(lubridate)

#####
#
#   Cleaning & Loading Data
#
#####

original_heart_data <- read_csv("../data/brian_heart.csv")
original_sleep_data <- read_csv("../data/brian_sleep.csv")

heart_data <- original_heart_data %>% 
  mutate_at(1:3, funs(as_datetime(., tz = "America/New_York"))) %>%
  mutate(start_time = strftime(start_date, format="%H:%M:%S"),
         end_time = strftime(end_date, format="%H:%M:%S"),
         entry_date = date(end_date),
         half_hour = ((2 * hour(end_date)) + (1 * (minute(end_date) > 29))))

sleep_data <- original_sleep_data %>% 
  mutate(flag = TRUE, start_time = strftime(start_date, format="%H:%M:%S"), end_time = strftime(end_date, format="%H:%M:%S")) %>%
  spread(key = sleep_type, value = flag, fill = FALSE) %>%
  filter(HKCategoryValueSleepAnalysisAsleep) %>%
  mutate_at(1:3, funs(as_datetime(., tz = "America/New_York"))) %>%
  mutate(interval = interval(start_date, end_date), flag = TRUE,
         start_time = strftime(start_date, format="%H:%M:%S"),
         end_time = strftime(end_date, format="%H:%M:%S"),
         seconds_length = int_length(interval))

heart_data$asleep <- lubridate::`%within%`(heart_data$start_date, as.list(sleep_data$interval))

daily_sleep_data <- sleep_data %>%
  mutate(entry_date = date(end_date)) %>% 
  group_by(entry_date) %>%
  summarize(latest_wakeup = max(end_time), seconds_of_sleep = sum(seconds_length)) %>%
  ungroup() %>%
  mutate(day_of_week = wday(entry_date, label = TRUE), weekend = day_of_week == "Sat" | day_of_week == "Sun")

sleep_and_hr <- heart_data %>% 
  filter(asleep == FALSE) %>%
  inner_join(daily_sleep_data) %>%
  filter(as.numeric(hms(end_time)) > as.numeric(hms(latest_wakeup))) %>%
  group_by(entry_date) %>%
  summarize(avg_heart_rate = mean(heart_rate),
            stdev_heart_rate = sd(heart_rate)) %>% 
  inner_join(daily_sleep_data) %>%
  ungroup() %>%
  mutate(hours_of_sleep = seconds_of_sleep/3600)

# top five supposed
top_five_supposed <- heart_data %>%
  filter(asleep == FALSE) %>%
  mutate(supposed_date = date(end_date)) %>%
  filter(supposed_date %in% as.Date(c("2018-08-02", "2018-07-26", "2018-08-30", "2018-08-24", "2018-07-25"))) %>%
  group_by(supposed_date) %>%
  summarize(highest_rate_supposed = max(heart_rate))

# top five actual
top_five_actual <- heart_data %>% 
  filter(asleep == FALSE) %>%
  mutate(actual_date = date(end_date)) %>%
  group_by(actual_date) %>%
  summarize(highest_rate_actual = max(heart_rate)) %>%
  arrange(desc(highest_rate_actual)) %>%
  head(5)

supposed_dates <- as.Date(c("2018-08-02", "2018-07-26", "2018-08-30", "2018-08-24", "2018-07-25"))
actual_dates <- top_five_actual$actual_date

#####
#
#   Creating & Saving Figures
#
#####

#All Heart Rates Over Time
heart_data %>%
  ggplot() +
  geom_line(aes(x = end_date, y = heart_rate))

#Average Heart Rates Over Time
heart_data %>%
  filter(asleep == FALSE) %>%
  group_by(entry_date) %>%
  summarize(avg = mean(heart_rate)) %>%
  ggplot() +
  geom_point(aes(x = entry_date, y = avg))

#Amount of sleep every day
daily_sleep_data %>%
  filter(!weekend) %>%
  ggplot() + 
  geom_point(aes(x = entry_date, y = seconds_of_sleep))

#Amount of Sleep Over Heart Rate
sleep_vs_hr %>%
  ggplot() +
  geom_point(aes(x = hours_of_sleep, y = avg_heart_rate)) +
  geom_smooth(aes(x = hours_of_sleep, y = avg_heart_rate), color = "red", method = lm, se = FALSE)

#Top Five Moments
for (i in 1:5){
  plot_data <- heart_data %>% 
    filter(entry_date %in% actual_dates[i]) %>%
    mutate(entry_date_char = as.character(entry_date), source = ifelse(entry_date == supposed_dates[i], "supposed", "actual"))
  
  print(ggplot(plot_data)
        + geom_point(aes(x = as.numeric(hms(end_time))/3600, y = heart_rate, color = asleep))
        + ggtitle(i))
}

#Top Five Moments vs Supposed Top Five
for (i in 1:5){
  plot_data <- heart_data %>% 
    filter(entry_date %in% c(supposed_dates[i], actual_dates[i]), asleep == FALSE) %>%
    mutate(entry_date_char = as.character(entry_date), source = ifelse(entry_date == supposed_dates[i], "supposed", "actual"))
  
  print(ggplot(plot_data) 
        + geom_density(aes(x = heart_rate, fill = source), alpha = 0.5)
        + ggtitle(i))
}

#Distribution of Heart Rate Broken Down Half-Hourly
heart_data %>%
  group_by(half_hour) %>%
  summarize(avg_hr = median(heart_rate), twenty_fifth = quantile(heart_rate, .25), seventy_fifth = quantile(heart_rate, .75)) %>%
  ggplot() +
  geom_line(aes(x = half_hour, y = avg_hr), size = 1) +
  geom_ribbon(aes(x = half_hour, ymin = twenty_fifth, ymax = seventy_fifth), alpha = 0.5)