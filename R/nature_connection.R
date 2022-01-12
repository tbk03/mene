library(tidyverse)
library(cjmr)
library(lubridate)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")

# *****************************************************************************
# Prepare data for first plot (leafs)
# *****************************************************************************
pan_df %>%
  select(feel_part_nature = m1_q6_a,
         nature_happy = m1_q6_b,
         weight_percent) %>%
  na.omit() %>%
  #distinct(feel_part_nature) %>%
  group_by(feel_part_nature) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%
  mutate(total_resp = sum(n),
         prop = round(n / total_resp, 2)) %>%
  summarise(sum(prop))

# *****************************************************************************
# Prepare data for second plot
# *****************************************************************************

plotting_df <- pan_df %>%
  select(feel_part_nature = m1_q6_a,
         nature_happy = m1_q6_b,
         find_beauty = m4_q8_a,
         spending_time = m4_q8_c,
         nature_amazing = m4_q8_d,
         weight_percent) %>%
  pivot_longer(cols = c(feel_part_nature:nature_amazing),
               names_to = "question", values_to = "response") %>%
  filter(!is.na(response)) %>%
  mutate(response = str_to_lower(response)) %>%
  group_by(question, response) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(total_resp = sum(n),
         prop = n / total_resp, 2) %>%
  ungroup() %>%

  filter(str_detect(response, c("^agree|y agree"))) %>%
  group_by(question) %>%
  summarise(total_agree = sum(prop))


# *****************************************************************************
# Create second plot
# *****************************************************************************

ggplot(plotting_df) +
  geom_point(mapping = aes(total_agree, 0), size = 10, alpha = 0.3) +
  scale_y_continuous(limits = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,0.9),
                     breaks = seq(0,0.9,0.1)) +
  theme_cjmr_explanatory() +
  theme(axis.line.y = element_blank())

ggsave("viz_out/figure_8.svg", units = "mm", width = 161, height = 80)


