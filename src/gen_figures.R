library(tidyverse)
library(lubridate)

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
  summarize(latest_wakeup = max(end_time), seconds_of_sleep = sum(seconds_length))

#All Heart Rates Over Time
heart_data %>%
  ggplot() +
  geom_line(aes(x = end_date, y = heart_rate))

#Average Heart Rates Over Time

#Amount of sleep every day

#Amount of Sleep Over Heart Rate

#Top Five Moments

#Distribution of Heart Rate Broken Down Half-Hourly