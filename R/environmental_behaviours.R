library(tidyverse)
library(cjmr)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")

# *****************************************************************************
# Prepare data
# *****************************************************************************

behaviour_df <- pan_df %>%
  select(meat = m4_q13a_a,
         car_commute =m4_q13a_b,
         fly = m4_q13b,
         weight_percent) %>%
  pivot_longer(cols = c(meat:fly),
               names_to = "behaviour",
               values_to = "frequency") %>%
  filter(!is.na(frequency)) %>%
  group_by(behaviour,frequency) %>%
  summarise(n = n()) %>%
  ungroup() %>%

  group_by(behaviour) %>%
  mutate(tot_resp = sum(n)) %>%
  ungroup() %>%

  mutate(prop = n / tot_resp)

unique(behaviour_df$frequency)

freq_levels <- c("Every day", "More than twice a week,\n but not every day",
                "Twice a week", "At least once a week", "Once or twice a month",
                "Once every 2-3 months", "Less often",
                "Never", "Don’t know",
                "Prefer not to say")

plotting_df <- behaviour_df %>%


  # combine frequencies
  mutate(frequency = case_when(
    frequency == "Once a week" ~ "At least once a week",
    frequency == "More than once a year, but less than every 2 – 3 months" ~ "Less often",
    frequency == "Once every year" ~ "Less often",
    frequency == "More than twice a week, but not every day" ~ "More than twice a week,\n but not every day",
    TRUE ~ frequency)) %>%

  # for ordering bars
  mutate(frequency = factor(frequency,
                            levels = rev(freq_levels))) %>%
  filter(frequency != "Don’t know",
         frequency != "Prefer not to say") %>%

  group_by(behaviour, frequency) %>%
  summarise(prop = sum(prop))


ggplot(plotting_df) +
  geom_col(aes(prop, frequency, fill = behaviour), width = 0.5) +

  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +

  labs(x = "Percentage of people") +

  facet_wrap(~behaviour) +
  theme_cjmr_explanatory() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line())

ggsave("viz_out/figure_9.svg", units = "mm", width = 247.4, height = 130)

View(plotting_df)
