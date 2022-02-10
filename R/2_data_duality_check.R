### Data Quality Check RMRP 2022 ####  

### Function start

r4v_error_report <- function(data,countryname = NULL)
{ 

### Get packages

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)

### This script must be executed only after doing the read data script

### Filter the country if needed

if (is.null(countryname) || (countryname=="All")) {
  df5W <- df5W   
} else {
  df5W <- df5W %>% filter(Country == countryname)    
}

# Data wrangling of reference table for quality check
# Vectors for verification
  
  AOlist <- as.vector(dfAO["Name"])
  IPlist <- as.vector(dfIP["Name"])
  
# Data Quality Check

  df5Werror <- df5W %>%
    mutate(
      
    )

} 
## remove objects end of script##
