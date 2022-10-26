library(tidyverse)
library(cjmr)
library(lubridate)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")

# read in lockdown dates
bg_dates <- readxl::read_xlsx("data_in/lockdown_dates.xlsx") %>%
  mutate(start_date = lubridate::date(start_date),
         end_date = lubridate::date(end_date)) %>%

  # add in row for seasonal effects
  bind_rows(list(lockdown = "seasonal",
              start_date = ymd("2020-09-01"),
              end_date = ymd("2021-01-01"),
              q_type = "positive"))

# *****************************************************************************
# Prepare data
# *****************************************************************************
covid_trends_df <- pan_df %>%
  select(wave, weight_percent,
         spending_time_outside = cv_q2a_2,
         noticing_nature = cv_q2a_6,
         fear_covid = m2b_q4a_14,
         stay_home_covid = m2b_q2_13
         ) %>%
  wave_to_date() %>%
  pivot_longer(cols = c(spending_time_outside:stay_home_covid),
               names_to = "question", values_to = "response") %>%
  group_by(date, question,response) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%

  filter(!is.na(response)) %>%

  group_by(date, question) %>%
  mutate(total_resp = sum(n)) %>%
  ungroup() %>%

  mutate(prop = n / total_resp) %>%
  ungroup() %>%

  mutate(q_type = if_else(question == "fear_covid" | question == "stay_home_covid",
         true = "negative", false = "positive"))

# *****************************************************************************
# Two plots
# *****************************************************************************
ggplot() +

  geom_rect(data = filter(bg_dates, q_type == "negative"),
            mapping = aes(xmin = start_date, xmax = end_date),
            ymin = 0, ymax = 1, fill = "grey50", alpha = 0.2) +

  geom_line(data = filter(covid_trends_df, response == "Yes", q_type == "negative"),
            mapping = aes(date, prop, colour = question)) +

  scale_x_date(expand = c(0,10), date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0), limits = c(0,0.8)) +

  labs(x = NULL) +
  theme_cjmr_explanatory() +
  theme(legend.position = "none")

ggsave("viz_out/figure_5.svg", units = "mm", width = 161, height = 80)
ggsave("viz_out/figure_5_large.svg", units = "mm", width = 200, height = 85)

ggplot() +

  geom_rect(data = filter(bg_dates, q_type == "negative"),
            mapping = aes(xmin = start_date, xmax = end_date),
            ymin = 0, ymax = 1, fill = "grey50", alpha = 0.2) +

  geom_line(data = filter(covid_trends_df, response == "Yes", q_type == "positive"),
            mapping = aes(date, prop, colour = question)) +

  scale_x_date(expand = c(0,10), date_breaks = "1 month",
               date_labels = "%b",
               limits = c(ymd("2020-03-26"), ymd("2021-04-12"))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0), limits = c(0,0.8)) +

  labs(x = NULL) +
  theme_cjmr_explanatory() +
  theme(legend.position = "none")

ggsave("viz_out/figure_6.svg", units = "mm", width = 161, height = 80)


# *****************************************************************************
# Get data points for annotations
# *****************************************************************************
covid_trends_df %>%
  filter(response == "Yes") %>%
  group_by(question) %>%
  summarise(max = max(prop),
            min = min(prop))

