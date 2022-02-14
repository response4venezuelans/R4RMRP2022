# Consolidated report RMRP 2022

# function writing

# Packages

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)

# Filter by the needed country

if (is.null(countryname) || (countryname=="All")) {
  df5Wconsolidated <- df5W   
} else {
  df5Wconsolidated <- df5W %>% filter(Country == countryname)    
}

# Get consolidated template file