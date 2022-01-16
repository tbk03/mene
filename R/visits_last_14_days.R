library(tidyverse)
library(cjmr)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")


# *****************************************************************************
# Prepare data
# *****************************************************************************
pan_df %>%
  select(any_visits_14, weight_percent) %>%
  filter(!is.na(any_visits_14)) %>%
  group_by(any_visits_14) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%
  mutate(tot_resp = sum(n),
         prop = n / tot_resp)
