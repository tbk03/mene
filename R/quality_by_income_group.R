library(tidyverse)
library(cjmr)
library(lubridate)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")


# *****************************************************************************
# Prepare data
# *****************************************************************************

quality_by_inc_group <- pan_df %>%

  # focus on variables need for the plot
  select(weight_percent, income,
         perceived_change = m1_q3) %>%

  # merge "not changed" and "dont know" responses
  mutate(perceived_change = if_else(perceived_change == "not changed"  | perceived_change == "Don’t know",
                                    "neutral",
                                    perceived_change),
         perceived_change = str_replace(perceived_change, " in the last 5 years", "")) %>%

  # calculate proportions
  group_by(income, perceived_change) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%

  group_by(income) %>%
  mutate(sample_size = sum(n)) %>%
  ungroup() %>%

  mutate(prop = n / sample_size)


# transform data for plotting
plot_df <- quality_by_inc_group %>%
  mutate(prop = if_else(str_detect(perceived_change, "reduced"),
                        true = -prop, false = prop),
         p_c_fac = factor(perceived_change,
                          levels = c("reduced a lot", "reduced a little",
                                     "improved a lot", "improved a little",
                                     "neutral")),
         facet_var = if_else(p_c_fac == "neutral",
                             true = 1, false = 0)) %>%
  filter(income != "Don't know", income != "Prefer not to say")

plot_df$p_c_fac

# *****************************************************************************
# Prepare data
# *****************************************************************************
ggplot(plot_df, aes(prop, income, fill = p_c_fac)) +
  geom_col(width = 0.5) +
  geom_vline(xintercept = 0) +

  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +

  facet_wrap(~facet_var) +
  theme_cjmr_explanatory() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none")

ggsave("viz_out/figure_7.svg", units = "mm", width = 161, height = 80)
