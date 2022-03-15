### Data Quality Check RMRP 2022 ####  

### Function start

r4v_error_reportSP <- function(data,countryname = NULL, 
                             write = "yes")
{ 

### Get packages

  # SHINY Creating a list which will return all the dataframes
  return_data <- list()
  
  
library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)

### This script must be executed only after doing the read data script

### Double check colnames
  
  colnames(df5WSP) <- c("Country",
                      "Admin1",
                      "Admin2",
                      "Appealing_org",
                      "Implementation",
                      "Implementing_partner",
                      "Month",
                      "Subsector",
                      "Indicator",
                      "Activity_Name",
                      "Activity_Description",
                      "COVID19",
                      "RMRPActivity",
                      "CVA",
                      "Value",
                      "Delivery_mechanism",
                      "Quantity_output",
                      "Total_monthly",
                      "New_beneficiaries",
                      "IN_DESTINATION",
                      "IN_TRANSIT",
                      "Host_Communities",
                      "PENDULARS",
                      "Returnees",
                      "Girls",
                      "Boys",
                      "Women",
                      "Men",
                      "Other_under",
                      "Other_above")

### Filter the country if needed

if (is.null(countryname) || (countryname=="All")) {
  df5WerrorSP <- df5WSP   
} else {
  df5WerrorSP <- df5WSP %>% filter(Country == countryname)    
}

# Script will check if cascading values are matching, concatenate relevent columns
  
  df5WerrorSP <- df5WerrorSP %>%
    mutate(countryadmin1 = paste(Country, Admin1),
           Admin1and2 = paste(Admin1, Admin2),
           sectorindicator = paste(Subsector, Indicator))%>%
    left_join(dfindSP, by = c("Subsector" =  "SectorSP", "Indicator" = "Indicador"))%>%
    select(-Codigo, -sectindic)
    
  
# Data wrangling of reference table for quality check
# Vectors for verification
  
  AOSPlist <- unique(as.vector(dfAOSP["Nombre"]))
  IPSPlist <- unique(as.vector(dfIPSP["Nombre"]))
  countrylist <- unique(as.vector(dfadmin1["countryadmin1"]))
  admin2list <- unique(as.vector(dfadmin2["admin1and2"]))
  sectindiclistSP <-  as.vector(dfindSP["sectindic"])
  
# Data Quality Check

  df5WerrorSP <- df5WerrorSP %>%
    rowwise()%>%
    # Where: check missing mandatory fields, Country-Admin1 pairs and Admin1-Admin2 pairs
    mutate(missingcountry = ifelse(is.na(Country) | is.na(Admin1), "Review", ""),
           countryadmincheck = ifelse(!any(countryadmin1 == countrylist), "Review", ""),
           admin1and2check = ifelse(!is.na(Admin2) & !any(Admin1and2 == admin2list), "Review", ""),
    # Who: Missing values and Org names that are not part of the list
      miss_appeal_org = ifelse(!is.na(Appealing_org) & any(Appealing_org == AOSPlist), "", "Review"),
      miss_setup = ifelse(is.na(Implementation), "Review", ""),
      miss_implementing_org = ifelse((Implementation == "Si" & (is.na(Implementing_partner) | !any(Implementing_partner == IPSPlist))) | 
                                       (Implementation == "No" & !is.na(Implementing_partner)), "Review", ""),
    # When: Missing month
    miss_month = ifelse(is.na(Month), "Review", ""),
    # What: missing values and inconsistencies in CVA
    missing_what = ifelse(is.na(Subsector)|is.na(Indicator)|is.na(Activity_Name)| is.na(RMRPActivity)|is.na(CVA), "Review", ""),
    wrongsectindicator = ifelse(!any(sectorindicator == sectindiclistSP), "Review", ""),
    # CVA mistakes
    zeroCVA = ifelse(CVA == "Si" & (is.na(Value)|  Value == 0), "Review", ""),
    missingmechanism = ifelse(CVA == "Si" & is.na(Delivery_mechanism), "Review", ""),
    CVANotoYes = ifelse((!is.na(Delivery_mechanism) | (!is.na(Value) & Value > 0)) & CVA == "No", "Review", ""),
    MultipurposeSector = ifelse(Subsector == "Multipurpose Cash Assistance (MPC)" & CVA == "No", "Review", ""),
    # Output and Breakdown related mistakes. Reviews will be divided according to indicator types
    # PNiN indicator related mistakes
    PiNNoBenef = ifelse(Indicatortype == 'PiN' & ((is.na(New_beneficiaries) | New_beneficiaries == 0) & (is.na(Total_monthly) | Total_monthly == 0)), "Review", ""),
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
    CBuildingNoBenef = ifelse(Indicatortype == 'Capacitaciones' & (Total_monthly == 0 | is.na(Total_monthly)), "Review", ""),
    # Todos los otros indicadores
    NoOutput = ifelse ((Indicatortype != 'Capacitaciones' & Indicatortype != 'PiN') & (Quantity_output == 0 | is.na(Quantity_output)), "Review", ""),
    Review = NA)%>%
    ungroup()%>%
    select(-countryadmin1, -Admin1and2, -sectorindicator, -Indicatortype)
  # Count errors and classify
  
  df5WerrorSP$Review[apply(df5WerrorSP, 1, function(r) any(r %in% c("Review"))) == TRUE] <- "Please review activity"
  
  # Remove empty errors column for easier reading
  # Create a row number column 
  
  df5WerrorSP <- df5WerrorSP%>%
    mutate(id = row_number())

  # split the dataframe in 2
  df5Werror1SP <- df5WerrorSP%>%
    select(Country,
           Admin1,
           Admin2,
           Appealing_org,
           Implementation,
           Implementing_partner,
           Month,
           Subsector,
           Indicator,
           Activity_Name,
           Activity_Description,
           COVID19,
           RMRPActivity,
           CVA,
           Value,
           Delivery_mechanism,
           Quantity_output,
           Total_monthly, 
           New_beneficiaries,
           IN_DESTINATION,
           IN_TRANSIT,
           Host_Communities,
           PENDULARS,
           Returnees,  
           Girls,
           Boys,
           Women,
           Men,
           Other_under,
           Other_above,
           id)

 df5Werror2SP <- df5WerrorSP%>%
   select(missingcountry,
          countryadmincheck,
          admin1and2check,
          miss_appeal_org,
          miss_setup,
          miss_implementing_org,
          miss_month,
          missing_what,
          wrongsectindicator,
          zeroCVA,
          missingmechanism,
          CVANotoYes,           
           MultipurposeSector,
           PiNNoBenef,
           NewBenefvstotal,
           PopTypeBreakdown,
           AGDBreakdown,
           CBuildingNoBenef,     
           NoOutput,
           Review,
           id)
  
  # remove empty columns
 df5Werror2SP <-  df5Werror2SP%>% discard(~all(is.na(.) | . ==""))
 
  # join by matching id column
 
 df5Werror0SP <- df5Werror1SP%>%
   left_join(df5Werror2SP, by = "id")%>%
   select(-id)
 
  # print error file
 
  if(write == "yes"){
  writexl::write_xlsx(df5Werror0SP, './out/5WErrorReportSP.xlsx')
  } else {
    
  }
  
 # SHINY
 
 return_data$ErrorReportclean <- df5Werror0SP
 return(return_data)
 
  ## remove objects end of script##
  rm(AOlist, IPlist, countrylist, admin2list, df5Werror, sectindiclist,df5Werror1SP,df5Werror2SP)


} 

