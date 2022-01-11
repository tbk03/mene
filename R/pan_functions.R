library(tidyverse)
library(lubridate)

read_pan <- function(location){

  # read in excel file
  pan_raw <- readxl::read_xlsx(location, sheet = "Data", guess_max = 26000)

  # clean data
  pan_clean <- pan_raw %>%
    janitor::clean_names()

  return(pan_clean)

}


wave_to_date <- function(df){

  res <- df %>%
    separate(wave, into = c("wave", "date"), sep = " - ") %>%
    mutate(date = lubridate::my(date),
           date = date %m+% days(15)) %>%
    select(-wave)

  return(res)

}
