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
         half_hour = ((2 * hour(end_date)) + (1 * (minute(end_date) > 29))),
         day_of_week = wday(entry_date, label = TRUE),
         weekend = day_of_week == "Sat" | day_of_week == "Sun")

sleep_data <- original_sleep_data %>% 
  mutate(flag = TRUE, start_time = strftime(start_date, format="%H:%M:%S"), 
         end_time = strftime(end_date, format="%H:%M:%S"),
         entry_date = date(end_date)) %>%
  spread(key = sleep_type, value = flag, fill = FALSE) %>%
  filter(HKCategoryValueSleepAnalysisAsleep) %>%
  mutate_at(1:3, funs(as_datetime(., tz = "America/New_York"))) %>%
  mutate(interval = interval(start_date, end_date), flag = TRUE,
         start_time = strftime(start_date, format="%H:%M:%S"),
         end_time = strftime(end_date, format="%H:%M:%S"),
         seconds_length = int_length(interval),
         day_of_week = wday(entry_date, label = TRUE),
         weekend = day_of_week == "Sat" | day_of_week == "Sun")

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
#   Creating Figures
#
#####

#All Heart Rates Over Time
HR_over_time <- heart_data %>%
  ggplot() +
  ggtitle("All Heart Rates") +
  geom_point(aes(x = end_date, y = heart_rate)) +
  xlab("Time") +
  ylab("HR (bpm)")


#Average Heart Rates Over Time
avg_HR_over_time <- heart_data %>%
  filter(asleep == FALSE) %>%
  group_by(entry_date) %>%
  summarize(avg = mean(heart_rate)) %>%
  ggplot() +
  ggtitle("Daily Average Heart Rate, Jul-Oct 2018") +
  geom_point(aes(x = entry_date, y = avg)) +
  geom_smooth(aes(x = entry_date, y = avg), color = "red", method = lm, se = FALSE) +
  xlab("Date") +
  ylab("AVG HR (bpm)")

#Amount of sleep every day
daily_sleep <- daily_sleep_data %>%
  filter(!weekend) %>%
  ggplot() + 
  ggtitle("Daily Hours of Sleep, Jul-Oct 2018") +
  geom_point(aes(x = entry_date, y = seconds_of_sleep/3600)) +
  geom_smooth(aes(x = entry_date, y = seconds_of_sleep/3600), color = "red", method = lm, se = FALSE) +
  xlab("Date") +
  ylab("Hours of Sleep")

#Amount of Sleep Over Heart Rate
sleep_on_HR <- sleep_and_hr %>%
  ggplot() +
  ggtitle("Sleep vs Average Heart Rate") +
  geom_point(aes(x = hours_of_sleep, y = avg_heart_rate)) +
  geom_smooth(aes(x = hours_of_sleep, y = avg_heart_rate), color = "red", method = lm, se = FALSE) +
  xlab("Hours of Sleep") +
  ylab("AVG HR (bpm)")

#Top Five Moments

#Vector to hold top five moment plots
top_five_plots <- list()

for (i in 1:5) local({
  plot_data <- heart_data %>% 
    filter(entry_date %in% actual_dates[i])
  
  top_five_plots[[i]] <<- ggplot(plot_data) +
    ggtitle(sprintf("Top HR Date #%i",i)) +
    geom_point(aes(x = as.numeric(hms(end_time))/3600, y = heart_rate, color = asleep)) +
    xlab("Hour") +
    ylab("HR (bpm)") +
    guides(fill=guide_legend(title="Asleep"))
})

#Top Five Moments vs Supposed Top Five

#Vector to hold top five comparison plots
top_five_comparison_plots <- list()

for (i in 1:5) local({
  plot_data <- heart_data %>% 
    filter(entry_date %in% c(supposed_dates[i], actual_dates[i]), asleep == FALSE) %>%
    mutate(entry_date_char = as.character(entry_date), source = ifelse(entry_date == supposed_dates[i], "Hypothesized", "Actual"))
  
  top_five_comparison_plots[[i]] <<- ggplot(plot_data) +
    ggtitle(sprintf("Top HR Date #%i : Actual vs Hypothesis", i)) + 
    geom_density(aes(x = heart_rate, fill = source), alpha = 0.5) + 
    xlim(40, 200) +
    xlab("HR (bpm)") +
    ylab("Density") + 
    theme(legend.title = element_blank())
})

#Distribution of Heart Rate Broken Down Half-Hourly
half_hourly_HR_distributions <- heart_data %>%
  filter(!weekend) %>%
  group_by(half_hour) %>%
  summarize(avg_hr = median(heart_rate), twenty_fifth = quantile(heart_rate, .25), seventy_fifth = quantile(heart_rate, .75)) %>%
  ggplot() +
  ggtitle("Expected HR by Hour") +
  geom_line(aes(x = half_hour/2, y = avg_hr), size = 0.5) +
  geom_ribbon(aes(x = half_hour/2, ymin = twenty_fifth, ymax = seventy_fifth), alpha = 0.5) +
  xlab("Hour") + 
  ylab("AVG HR (bpm)")

#####
#
#   Saving Figures
#
#####

ggsave(HR_over_time, file = "../figures/HR_over_time.png", height = 4, width = 8)

ggsave(avg_HR_over_time, file = "../figures/avg_HR_over_time.png", height = 4, width = 8)

ggsave(daily_sleep, file = "../figures/daily_sleep.png", height = 4, width = 4)

ggsave(sleep_on_HR, file = "../figures/sleep_on_HR.png", height = 4, width = 4)

for(i in 1:5){
  ggsave(top_five_plots[[i]], file = paste("../figures/top_five_", i, ".png", sep = ""), height = 4, width = 4)
  ggsave(top_five_comparison_plots[[i]], file = paste("../figures/top_five_comparison_", i, ".png", sep = ""), height = 4, width = 4)
}

ggsave(half_hourly_HR_distributions, file = "../figures/half_hourly_HR_distributions.png", height = 4, width = 4)
