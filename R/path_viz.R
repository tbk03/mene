library(tidyverse)
library(cjmr)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")

# *****************************************************************************
# Prepare data - visits
# *****************************************************************************


a_y_14_fct_levels <- c("Other", "No visits","Any visits")

any_visits_by_month <- pan_df %>%

  mutate(any_visits_14 = if_else(any_visits_14 == "Dont know" |
                                   any_visits_14 == "Prefer not to say",
                                 true = "No visits",
                                 false = any_visits_14)) %>%

  select(wave, weight_percent, any_visits_14) %>%
  filter(!is.na(any_visits_14)) %>%
  wave_to_date() %>%
  group_by(date, any_visits_14) %>%
  summarise(n = sum(weight_percent)) %>%
  mutate(month = format(date,"%B"), freq = n / sum(n),
         any_visits_14_fct = factor(any_visits_14,
                                    levels = a_y_14_fct_levels)) %>%
  ungroup()


any_visits_by_month %>%
  distinct(any_visits_14_fct)

# *****************************************************************************
# Prepare data - covid questions
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
  group_by(date, question, response) %>%
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
# Combine data - ahead of plotting
# *****************************************************************************

any_visits_simp <- any_visits_by_month %>%
  filter(any_visits_14 == "Any visits") %>%
  select(date,
         prop_any_visits = freq)

covid_trends_simp <- covid_trends_df %>%
  filter(question == "fear_covid",
         response == "Yes") %>%
  select(date,
         prop_fear = prop)

plotting_df <- any_visits_simp %>%
  left_join(covid_trends_simp) %>%
  mutate(month = lubridate::month(date, label = TRUE))

# *****************************************************************************
# Create plot
# *****************************************************************************

ggplot(plotting_df, aes(prop_fear, prop_any_visits)) +
  geom_path(size = 0.5, colour = "grey50") +
  geom_point(colour = "#126E96", size = 2.5) +
  ggrepel::geom_text_repel(mapping = aes(prop_fear, prop_any_visits,
                          label = month)) +
  scale_y_continuous(labels = scales::percent_format(scale = 100,
                                                     accuracy = 5),
                     limits = c(0.45,0.7),
                     breaks = seq(0.45, 0.7, 0.05)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100,
                                                     accuracy = 5),
                     limits = c(0.25, 0.6)) +
  scale_color_gradient(low = "#0EAB4E", high =  "grey25") +
  labs(x = "Percentage of people who reported fear of Covid-19 as a reason
       for not spending time outside in the last 14 days") +
  coord_equal() +
  theme_cjmr_explanatory() +
  theme(panel.grid.major.x = element_line(size = 0.5, colour = "grey80"),
        legend.position = "none")

ggsave("viz_out/path.svg", units = "mm", width = 161, height = 161)
