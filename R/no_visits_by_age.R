library(tidyverse)
library(cjmr)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")


# *****************************************************************************
# Prepare data
# *****************************************************************************

no_visits_by_age_df <- pan_df %>%

  # focus on columns of interest
  select(respondent_id, weight_percent, weight_grossed_no_of_visits, age_band, no_of_visits) %>%

  # ignore survey respondents who didn't provide a number of visits
  filter(!is.na(no_of_visits)) %>%

  # catergorise number_of_visits
  mutate(no_of_visits_groups = case_when(
    no_of_visits == 0 ~ "none",
    no_of_visits >= 14 ~ "daily",
    TRUE ~ "other"
  )) %>%

  # calculate number of people in each group (weighted)
  group_by(age_band, no_of_visits_groups) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%

  # calculate proportions by age_band
  group_by(age_band) %>%
  mutate(sample_size = sum(n),
         prop = n / sample_size) %>%
  ungroup()


no_visits__df <- no_visits_by_age_df %>%

  # calculate proportions
  group_by(no_of_visits_groups) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop_pop = n / sum(n),
         people_per_hundred = round(prop_pop * 100),
         no_of_visits_fac = factor(no_of_visits_groups,
                                   levels = c("none", "daily", "other")))


# *****************************************************************************
# Two plots
# *****************************************************************************
no_visits_by_age_df %>%
  filter(no_of_visits_groups == "none") %>%
  ggplot() +
  geom_col(aes(age_band, prop), fill = "#737373", width = 0.35) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0), limits = c(0,0.45)) +
  labs(x = "Age") +
  theme_cjmr_explanatory() +
  theme(axis.ticks.x = element_blank())

ggsave("viz_out/figure_3.svg", units = "mm", width = 120, height = 70)


no_visits_by_age_df %>%
  filter(no_of_visits_groups == "daily") %>%
  mutate(prop = round(prop, 2)) %>%
  ggplot() +
  geom_col(aes(age_band, prop), fill = "#6EC287", width = 0.35) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0),limits = c(0,0.45)) +
  labs(x = "Age") +
  theme_cjmr_explanatory() +
  theme(axis.ticks.x = element_blank())

ggsave("viz_out/figure_4.svg", units = "mm", width = 120, height = 70)






