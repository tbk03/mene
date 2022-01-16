library(tidyverse)
library(cjmr)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")


# *****************************************************************************
# Prepare data
# *****************************************************************************


a_y_14_fct_levels <- c("Other", "No visits","Any visits")

any_visits_by_month <- pan_df %>%

  mutate(any_visits_14 = if_else(any_visits_14 == "Dont know" |
                                   any_visits_14 == "Prefer not to say",
                                 true = "No visits",
                                 false = any_visits_14)) %>%

  select(wave, weight_percent, any_visits_14) %>%
  filter(!is.na(any_visits_14)) %>%
  separate(wave, into = c("wave", "date"), sep = " - ") %>%
  mutate(date = lubridate::my(date)) %>%
  select(-wave) %>%
  group_by(date, any_visits_14) %>%
  summarise(n = sum(weight_percent)) %>%
  mutate(month = format(date,"%B"), freq = n / sum(n),
         any_visits_14_fct = factor(any_visits_14,
                                    levels = a_y_14_fct_levels)) %>%
  ungroup()


any_visits_by_month %>%
  distinct(any_visits_14_fct)

# *****************************************************************************
# Produce the plot
# *****************************************************************************

ggplot(filter(any_visits_by_month,
              any_visits_14_fct == "Any visits"),
       aes(date, freq, fill = any_visits_14_fct)) +
  geom_area(alpha = 0.75) +

  scale_x_date(expand = c(0,10), date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous(expand = c(0,0),
                     labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("grey85", "#0EAB4E")) +

  labs(x = NULL) +
  theme_cjmr_explanatory() +
  theme(legend.position = "none")
#facet_wrap(~any_visits_14_fct)

ggsave("viz_out/any_visits_twitter.svg", units = "mm", height = 93, width = 155)
