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

heart_data$asleep_flag <- lubridate::`%within%`(heart_data$start_date, as.list(sleep_data$interval))
heart_data <- heart_data %>%
  mutate(asleep = ifelse(asleep_flag, "Asleep", "Awake"))

#####
#
#   Creating Figures
#
#####

#Average Heart Rates Over Time
avg_HR_over_time <- heart_data %>%
  filter(asleep_flag == FALSE) %>%
  group_by(entry_date) %>%
  summarize(avg = mean(heart_rate)) %>%
  ggplot() +
  ggtitle("Daily Average Heart Rate, Jul-Oct 2018") +
  geom_point(aes(x = entry_date, y = avg)) +
  geom_smooth(aes(x = entry_date, y = avg), color = "red", method = lm, se = FALSE) +
  xlab("Date") +
  ylab("Average Heart Rate (bpm)") +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = '#F4F4F4'), 
        panel.background = element_rect(fill = '#F4F4F4'),
        legend.key = element_rect(fill = "#F4F4F4"),
        legend.background = element_rect(fill = "#F7F7F7", color="#E6E6E6"),
        panel.grid.major = element_line(color="#E6E6E6"), 
        panel.grid.minor = element_line(color="#E6E6E6"))

#Dates of interest for the investigation
interesting_dates <- as.Date(c("2018-07-26", "2018-08-02", "2018-08-04", "2018-08-30"))

#Vector to hold the scatter plots
interesting_date_scatter_plots <- list()

for (i in 1:4){
  scatter_plot_data <- heart_data %>% 
    filter(entry_date %in% interesting_dates[i])

  interesting_date_scatter_plots[[i]] <- ggplot(scatter_plot_data) + 
          geom_point(aes(x = as.numeric(hms(end_time))/3600, y = heart_rate, color = asleep)) +
          scale_color_manual(values = c("Awake" = "#FAAA6D", "Asleep" = "#105388")) +
          ggtitle(sprintf("Heart Rate Points from %s", interesting_dates[i])) +
          scale_x_continuous(limits = c(0, 24), breaks=seq(0, 24, 8)) +
          ylim(40, 200) +
          xlab("Hour of Day") +
          ylab("Heart Rate (bpm)") + 
          theme(legend.title = element_blank(),
                plot.background = element_rect(fill = '#F4F4F4'), 
                panel.background = element_rect(fill = '#F4F4F4'),
                legend.key = element_rect(fill = "#F4F4F4"),
                legend.background = element_rect(fill = "#F7F7F7", color="#E6E6E6"),
                panel.grid.major = element_line(color="#E6E6E6"), 
                panel.grid.minor = element_line(color="#E6E6E6"))
}

#Vector to hold the density plots
interesting_date_density_plots <- list()

for (i in 1:4){
  density_plot_data <- heart_data %>% 
    filter(entry_date %in% c(interesting_dates[i]), asleep_flag == FALSE)

  interesting_date_density_plots[[i]] <- ggplot(density_plot_data) +
    ggtitle(sprintf("Heart Rate Density Plot from %s", interesting_dates[i])) + 
    geom_density(aes(x = heart_rate), alpha = 0.40, color='red', fill="red") + 
    xlim(40, 200) +
    ylim(0, 0.03) +
    xlab("Heart Rate (bpm)") +
    ylab("Density") + 
    theme(legend.title = element_blank(),
          plot.background = element_rect(fill = '#F4F4F4'), 
          panel.background = element_rect(fill = '#F4F4F4'),
          legend.key = element_rect(fill = "#F4F4F4"),
          legend.background = element_rect(fill = "#F7F7F7", color="#E6E6E6"),
          panel.grid.major = element_line(color="#E6E6E6"), 
          panel.grid.minor = element_line(color="#E6E6E6"))
}

#####
#
#   Saving Figures
#
#####

ggsave(avg_HR_over_time, file = "../figures/avg_HR_over_time.png", height = 4, width = 8)

for(i in 1:4){
  ggsave(interesting_date_scatter_plots[[i]], file = paste("../figures/interesting_date_scatter_plot_", i, ".png", sep = ""), height = 4, width = 4)
  ggsave(interesting_date_density_plots[[i]], file = paste("../figures/interesting_date_density_plot_", i, ".png", sep = ""), height = 4, width = 4)
}
