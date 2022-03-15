# Translation from Spanish to English

# For countries reporting their 5W in Spanish, to translate categorical values into English

rmrp_translate <- function()
{ 
  # load packages
  
  library(activityinfo)
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(stringdist)
  
  ########################## Create and generate dictionnary ##################
  
  # Translations obtained through already downloaded reference tables, missing values locally created
  # Edit colnames for easier referencing
  
  colnames(dfAO) <- c("AOIDORG",
                      "NameAO", 
                      "NombreAO")
  
  colnames(dfIP) <- c("IPID",
                      "NameIP", 
                      "NombreIP")
  
  indicatortransl <- dfindicator %>%
    left_join(dfindSP, by = c("Code" = "Codigo"))
  
  
  # Delivery mechanism
  
  EnglishDelivery <- c("Physical cash ", "Mobile money transfer", "Bank transfer","Other electronic cash mechanisms", "Vouchers", "Others")
  SpanishDelivery <- c("Dinero en efectivo", "Transferencia via móvil", "Transferencia Bancaria", "Otros mecanismos de dinero electrónico", "Cupones", "Otros")
  
  dfmechanism <- data.frame(EnglishDelivery, SpanishDelivery)

  ################################# Get data and translate #######################
  
  # Insert local file to get data in Spanish
  
  df5WSpanish <- df5WSP
  
  # Change column names to English template version
  
  colnames(df5WSpanish) <- c("Country",
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
  
  df5WSpanish <- df5WSpanish %>%
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
                "Other_above"), as.numeric)
  
 # Create variable list of Yes/No columns for quicker translation
 
  var_list_yes_no = c("Implementation", 
                      "COVID19", 
                      "RMRPActivity",
                      "CVA")
  
  # Translation made through different joins and mutate
  
  df5Wtranslated <- df5WSpanish%>%
    left_join(dfAO, by = c("Appealing_org" = "NombreAO"))%>%
    left_join(dfIP, by = c("Implementing_partner" = "NombreIP"))%>%
    left_join(indicatortransl, by = c( "Subsector" = "SectorSP", "Indicator" = "Indicador"))%>%
    left_join(dfmechanism, by = c( "Delivery_mechanism" = "SpanishDelivery"))%>%
    mutate(Appealing_org = NameAO,
           Implementing_partner = NameIP,
           Subsector = Subsector.y,
           Indicator = Indicator.y,
           Delivery_mechanism = EnglishDelivery)%>%
    mutate_at(var_list_yes_no, 
              function(x) gsub("Si","Yes",x)) %>%
    mutate_at(var_list_yes_no, 
              function(x) gsub("Sí","Yes",x)) %>% 
    mutate_at(var_list_yes_no, 
              function(x) gsub("No","No",x))%>%
    select("Country",
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
  
  # Change colnames to Activity Info original ones for easier upload
  
  colnames(df5Wtranslated) <- c("Country" ,
                                "Country Admin1" ,
                                "Admin2",
                                "Appealing organisation Name" ,
                                "Implementation Set up",
                                "Implementing partner Name" ,
                                "Month" ,
                                "Subsector" ,
                                "Indicator" ,
                                "Activity Name",
                                "Activity Description" ,
                                "COVID 19 Situation" ,
                                "RMRP Activity" ,
                                "CVA" ,
                                "Value (in USD)" ,
                                "Delivery mechanism" ,
                                "Quantity of output" ,
                                "Total monthly beneficiaries" ,
                                "New beneficiaries of the month" ,
                                "Refugees and Migrants IN DESTINATION" ,
                                "Refugees and Migrants IN TRANSIT" ,
                                "Host Communities Beneficiaries" ,
                                "Refugees and Migrants PENDULARS",
                                "Colombian Returnees" ,
                                "Women under 18" ,
                                "Men under 18" ,
                                "Women above 18",
                                "Men above 18" ,
                                "Other under 18" ,
                                "Other above 18")
  
  # Write file
  
   write_xlsx(df5Wtranslated, './out/Translated5W.xlsx')
   
   rm(indicatortransl,
      EnglishDelivery,
      SpanishDelivery,
      dfmechanism,
      var_list_yes_no
   )
   
  }