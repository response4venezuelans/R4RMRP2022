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
  df5Werror <- df5W   
} else {
  df5Werror <- df5W %>% filter(Country == countryname)    
}

# Script will check if cascading values are matching, concatenate relevent columns
  
  df5Werror <- df5Werror %>%
    mutate(countryadmin1 = paste(Country, Admin1),
           Admin1and2 = paste(Admin1, Admin2),
           sectorindicator = paste(Subsector, Indicator))%>%
    left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
    select(-Code, -sectindic)
    
  
# Data wrangling of reference table for quality check
# Vectors for verification
  
  AOlist <- as.vector(dfAO["Name"])
  IPlist <- as.vector(dfIP["Name"])
  countrylist <- as.vector(dfadmin2["countryadmin1"])
  admin2list <- as.vector(dfadmin2["admin1and2"])
# Data Quality Check

  df5Werror <- df5Werror %>%
    rowwise()%>%
    # Where: check missing mandatory fields, Country-Admin1 pairs and Admin1-Admin2 pairs
    mutate(missing = ifelse(is.na(Country) | is.na(Admin1), "ERROR", ""),
           countryadmincheck = ifelse(!any(countryadmin1 == countrylist), "ERROR", ""),
           admin1and2check = ifelse(!is.na(Admin2) & !any(Admin1and2 == admin2list), "ERROR", ""),
    # Who: Missing values and Org names that are not part of the list
      miss_appeal_org = ifelse(!is.na(Appealing_org) & any(Appealing_org == AOlist), "", "ERROR"),
      miss_setup = ifelse(is.na(Implementation), "ERROR", ""),
      miss_implementing_org = ifelse(Implementation == "Yes" & is.na(Implementing_partner) | !any(Implementing_partner == IPlist), "", "ERROR"),
    # When: Missing month
    miss_month = ifelse(is.na(Month), "ERROR", ""),
    # What: missing values and inconsistencies in CVA
    missing_what = ifesle(is.na(Subsector)|is.na(Indicator)|is.na(Activity_Name)| is.na(RMRPActivity)|is.na(CVA), "ERROR", ""),
    wrongsectindicator = ifelse(!any(sectorindicator == sectindic)),
    # CVA mistakes
    zeroCVA = ifelse(CVA == "Yes" & (is.na(Value)| Value = 0), "ERROR", ""),
    missingmechanism = ifelse(CVA == "Yes" & is.na(Delivery_mechanism), "ERROR", ""),
    CVANotoYes = ifelse((!is.na(Delivery_mechanism) | (!is.na(Value) & Value > 0)) & CVA == "No", "ERROR", ""),
    MultipurposeSector = ifelse(Subsector == "Multipurpose Cash Assistance (MPC)" & CVA == "No", "ERROR", ""),
    # Output and Breakdown related mistakes. Errors will be divided according to indicator types
    # PNiN indicator related mistakes
    PiNNoBenef = ifelse(Indicatortype == 'PiN' & (is.na(New_beneficiaries) | New_beneficiaries = 0 | is.na(Total_monthly) | Total_monthly = 0), "ERROR", ""),
    NewBenefvstotal = ifelse(Indicatortype == 'PiN' & New_beneficiaries > Total_monthly, "ERROR", ""),
    PopTypeBreakdown = ifelse(Indicatortype == 'PiN' & New_beneficiaries != sum(IN_DESTINATION,
                                                                                 IN_TRANSIT,
                                                                                 Host_Communities,
                                                                                 PENDULARS,
                                                                                 Returnees, na.rm = TRUE), "ERROR", ""),
    AGDBreakdown = ifelse(Indicatortype == 'PiN' & New_beneficiaries != sum(Girls,
                                                                              Boys,
                                                                              Women,
                                                                              Men,
                                                                            Oher_under,
                                                                              Other_above, na.rm = TRUE), "ERROR", ""),
    # Capacity Building indicators
    CBuildingNoBenef = ifelse(Indicatortype == 'Capacity Building' & (Total_monthly = 0 | is.na(Total_monthly)), "ERROR", ""),
    # Todos los otros indicadores
    NoOutput = ifelse ((Indicatortype != 'Capacity Building' & Indicatortype == 'PiN') & (Quantity_output = 0 | is.na(Quantity_output)), "ERROR", "")
    )
  # Count errors and classify
  
  # Remove empty errors column for easier reading
  
  # print error file if needed
} 
## remove objects end of script##
