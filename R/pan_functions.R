library(tidyverse)

read_pan <- function(location){

  # read in excel file
  pan_raw <- readxl::read_xlsx(location, sheet = "Data", guess_max = 26000)

  # clean data
  pan_clean <- pan_raw %>%
    janitor::clean_names()

  return(pan_clean)

}

