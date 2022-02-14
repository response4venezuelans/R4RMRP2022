### Data Quality Check RMRP 2022 ####  

### Function start

r4v_error_report <- function(data,countryname = NULL, print = NULL)
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
  sectindiclist <-  as.vector(dfindicator["sectindic"])
  
# Data Quality Check

  df5Werror <- df5Werror %>%
    rowwise()%>%
    # Where: check missing mandatory fields, Country-Admin1 pairs and Admin1-Admin2 pairs
    mutate(missingcountry = ifelse(is.na(Country) | is.na(Admin1), "Review", ""),
           countryadmincheck = ifelse(!any(countryadmin1 == countrylist), "Review", ""),
           admin1and2check = ifelse(!is.na(Admin2) & !any(Admin1and2 == admin2list), "Review", ""),
    # Who: Missing values and Org names that are not part of the list
      miss_appeal_org = ifelse(!is.na(Appealing_org) & any(Appealing_org == AOlist), "", "Review"),
      miss_setup = ifelse(is.na(Implementation), "Review", ""),
      miss_implementing_org = ifelse((Implementation == "Yes" & (is.na(Implementing_partner) | !any(Implementing_partner == IPlist))) | 
                                       (Implementation == "No" & !is.na(Implementing_partner)), "Review", ""),
    # When: Missing month
    miss_month = ifelse(is.na(Month), "Review", ""),
    # What: missing values and inconsistencies in CVA
    missing_what = ifelse(is.na(Subsector)|is.na(Indicator)|is.na(Activity_Name)| is.na(RMRPActivity)|is.na(CVA), "Review", ""),
    wrongsectindicator = ifelse(!any(sectorindicator == sectindiclist), "Review", ""),
    # CVA mistakes
    zeroCVA = ifelse(CVA == "Yes" & (is.na(Value)|  Value == 0), "Review", ""),
    missingmechanism = ifelse(CVA == "Yes" & is.na(Delivery_mechanism), "Review", ""),
    CVANotoYes = ifelse((!is.na(Delivery_mechanism) | (!is.na(Value) & Value > 0)) & CVA == "No", "Review", ""),
    MultipurposeSector = ifelse(Subsector == "Multipurpose Cash Assistance (MPC)" & CVA == "No", "Review", ""),
    # Output and Breakdown related mistakes. Reviews will be divided according to indicator types
    # PNiN indicator related mistakes
    PiNNoBenef = ifelse(Indicatortype == 'PiN' & (is.na(New_beneficiaries) | New_beneficiaries == 0 | is.na(Total_monthly) | Total_monthly == 0), "Review", ""),
    NewBenefvstotal = ifelse(Indicatortype == 'PiN' & New_beneficiaries > Total_monthly, "Review", ""),
    PopTypeBreakdown = ifelse(Indicatortype == 'PiN' & New_beneficiaries != sum(IN_DESTINATION,
                                                                                 IN_TRANSIT,
                                                                                 Host_Communities,
                                                                                 PENDULARS,
                                                                                 Returnees, na.rm = TRUE), "Review", ""),
    AGDBreakdown = ifelse(Indicatortype == 'PiN' & New_beneficiaries != sum(Girls,
                                                                              Boys,
                                                                              Women,
                                                                              Men,
                                                                            Other_under,
                                                                              Other_above, na.rm = TRUE), "Review", ""),
    # Capacity Building indicators
    CBuildingNoBenef = ifelse(Indicatortype == 'Capacity Building' & (Total_monthly == 0 | is.na(Total_monthly)), "Review", ""),
    # Todos los otros indicadores
    NoOutput = ifelse ((Indicatortype != 'Capacity Building' & Indicatortype != 'PiN') & (Quantity_output == 0 | is.na(Quantity_output)), "Review", "")
    )%>%
    select(-countryadmin1, -Admin1and2, -sectorindicator, -Indicatortype)
  # Count errors and classify
  
  df5Werror$Review[apply(df5Werror, 1, function(r) any(r %in% c("Review"))) == TRUE] <- "Please review activity"
  
  # Remove empty errors column for easier reading
  
  df5Werror <- df5Werror %>%
    discard(across(34:54,(is.na(.) | . =="")))
  
  # print error file
  if(write == "yes"){
  writexl::write_xlsx(df5Werror, './out/5WErrorReport.xlsx')
  } 
  
  ## remove objects end of script##
  rm(AOlist, IPlist, countrylist, admin2list, df5Werror, sectindiclist)
  
} 

