# Read data 2022
# function

read_data_2022_SP <- function(data, 
                                 write ="yes")
{ 
  
  # Load packages
  library(activityinfo)
  library(tidyverse)
  library(readxl)
  library(writexl)
  
  # Credentials located in seperate file
  source("R/ai_credentials.R")
  
  # Get data from different sources
  
  df5WSP <- data2
  
  # format column names for easier data processing
  
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
  
  # Short data wrangling for integer values
  
  df5WSP <<- df5WSP %>%
    mutate_at(c("Value",
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
                "Other_above"), as.numeric)%>%
    arrange(Country, Month)
  
  # Not recommended but if needed, chose to write the 5W as a xlsx file in repository
  
  # if(write == "yes"){
  #   write_xlsx(df5W, "./data/RMRP2022ActivitiesSP.xlsx")
  # } 
  # Get other reference table used during the data quality check
  # Loaded from AI regardless the method for 5W used
  
  dfadmin1  <<- queryTable("ct51c85kxeqpu473",
                           "Country" = "c8u26b8kxeqpy0k4",
                           "Admin1" = "c3ns3zikxeqq4h95",
                           "ISOCode" = "cl3sspjkxeqq8yq6",truncate.strings = FALSE)%>%
    rowwise()%>%
    mutate(countryadmin1 = paste(Country, Admin1))%>%
    ungroup()
  
  dfadmin2  <<- queryTable("cn6oysukx6hk2cn3",
                           "Country" = "cnkb6jykxgdeemm4r.c8u26b8kxeqpy0k4",
                           "Admin1" = "cnkb6jykxgdeemm4r.c3ns3zikxeqq4h95",
                           "Admin2" = "cs2esadkx6hkt7j6", truncate.strings = FALSE)%>%
    rowwise()%>%
    mutate(admin1and2 = paste(Admin1, Admin2))%>%
    ungroup()
  
  dfindSP <<- queryTable("cqt45yktk2m8ky3",
                         "Codigo" = "cob8rivktedzp0f3",
                         "SectorSP" = "c84rjfckxgbve582",
                         "Indicador" = "cwkj9p4kteeh4ls5",
                         "Indicatortype"= "cprepl2ktk2l76a3",truncate.strings = FALSE)%>%
    rowwise()%>%
    mutate(sectindic = paste(SectorSP, Indicador))%>%
    ungroup  
  
  dfAOSP  <<- queryTable("cbisyyxkumvyhy57",
                       "AOIDORG" = "cnhvpo4kumvyqla8",
                       "Name" = "ckj5zamkumvyysv9",
                       "Nombre" = "cpmcp88kumvz7bsa", truncate.strings = FALSE)
  
  dfIPSP  <<- queryTable("cuy0fjukumwabck4",
                       "IPID" = "cd2ow0jkumwazdl1h",
                       "Name" = "ckj5zamkumvyysv9",
                       "Nombre" = "cpmcp88kumvz7bsa", truncate.strings = FALSE)
  
  return(df5WSP)
  
} 