library(tidyverse)
library(cjmr)
library(sf)

source("R/pan_functions.R")

# read in people and nature survey data
pan_df <- read_pan("../datasets/people_and_nature_survey/People_and_Nature_Q1_Q4.xlsx")


# MSOA shapefile
shp_folder <- "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3-shp/"
shp_name <-  "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp"

msoa_bound <- read_sf(str_c("../datasets/people_and_nature_survey/",
                            shp_folder, shp_name)) %>%
  janitor::clean_names() %>%
  filter(str_detect(msoa11cd,"^E")) # focus on England only

# look up table
lookup_name <- "Local_Authority_District_to_Region_(April_2021)_Lookup_in_England.csv"
la_to_region_lookup <- read_csv(str_c("../datasets/people_and_nature_survey/",
                                     lookup_name)) %>%
  janitor::clean_names() %>%
  select(lad21cd, rgn21nm) %>%
  filter(str_detect(lad21cd,"^E")) # focus on England only

# MSOA population data
msoa_pop_name <- "sape23dt4mid2020msoasyoaestimatesunformatted.xlsx"
msoa_pop <- readxl::read_xlsx(str_c("../datasets/people_and_nature_survey/",
                        msoa_pop_name), sheet = "Mid-2020 Persons",
                  skip = 4) %>%
  janitor::clean_names() %>%
  select(msoa_code, all_ages,
         lad21cd = la_code_2021_boundaries) %>%
  filter(str_detect(msoa_code,"^E")) # focus on England only

# *****************************************************************************
# Prepare data
# *****************************************************************************

# lsoa populations with spatial information
msoa_pop_region <- msoa_pop %>%
  left_join(la_to_region_lookup) %>%

  # West and North Norhamptonshire LAs not allocated a region
  # so replace NAs in region with East Midlands
  mutate(rgn21nm = replace_na(rgn21nm, "East Midlands"))

# lsoa_pop_region %>%
#   filter(is.na(rgn21nm)) %>%
#   count(lad21cd)

msoa_pop_sf <- msoa_bound %>%
  left_join(msoa_pop_region, by = c("msoa11cd" = "msoa_code")) %>%
  select(all_ages, rgn21nm)

# lsoa_pop_sf %>%
#   filter(is.na(all_ages))

# proportion of population with no visits (last 14 days) for each region
no_visits_by_region <- pan_df %>%

  select(region, weight_percent, any_visits_14) %>%
  filter(!is.na(any_visits_14)) %>%
  group_by(region, any_visits_14) %>%
  summarise(n = sum(weight_percent)) %>%
  ungroup() %>%

  group_by(region) %>%
  mutate(region_n = sum(n)) %>%
  ungroup() %>%

  filter(any_visits_14 == "No visits") %>%
  mutate(prop_no_visits = n / region_n) %>%

  select(rgn21nm = region, prop_no_visits) %>%

  # correct region names ahead of joining
  mutate(rgn21nm = case_when(
    rgn21nm == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
    rgn21nm == "East" ~ "East of England",
    TRUE ~ rgn21nm
  ))

# lsoa_pop_sf %>%
#   st_drop_geometry() %>%
#   distinct(rgn21nm)

no_visits_sf <- msoa_pop_sf %>%
  left_join(no_visits_by_region) %>%
  mutate(num_no_visits = prop_no_visits * all_ages) %>%
  select(num_no_visits) %>%
  mutate(num_no_visits_points = round(num_no_visits / 1000))


# dummy_sf <- no_visits_sf %>%
#   slice(1:10)
#
# dummy_num_points <- no_visits_sf$num_no_visits_points[1:10]

# dot_density_points <- st_sample(dummy_sf, dummy_num_points)
# sum(dummy_num_points)


# no_visits_sf %>%
#   filter(is.na(num_no_visits)) %>%
#   st_drop_geometry() %>%
#   count(rgn21nm)
#
# no_visits_sf %>%
#   filter(is.na(rgn21nm))

dot_density_points <- st_sample(no_visits_sf %>%
                                  slice(1:10),
                                no_visits_sf$num_no_visits_points[1:10])

dot_density_points <- st_sample(no_visits_sf,
                                no_visits_sf$num_no_visits_points)

st_write(dot_density_points,
         "data_out/dot_density.shp",
         delete_layer = TRUE)

ddps <- st_read("data_out/dot_density.shp")

ggplot(dot_density_points) +
  geom_sf(alpha = 0.3, size = 0.05) +
  ggthemes::theme_map()

ggplot(ddps) +
  geom_sf(alpha = 0.3, size = 0.05) +
  ggthemes::theme_map()
