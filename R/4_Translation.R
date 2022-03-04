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
  
  # Get categorical values for translation from the reference tables
  # Appealing orgs
  
  dfAO2 <- dfAO%>%
    rename(type = AOIDORG,
           English = Name,
           Spanish = Nombre)%>%
    mutate(type = "AO")
    
  # Implementing Partners
  
  dfIP2 <- dfIP%>%
    rename(type = IPID,
           English = Name,
           Spanish = Nombre)%>%
    mutate(type = "IP")
    
  # Sector and indicator
  
  # Credentials located in seperate file
  source("R/ai_credentials.R")
  
  # Get Spanish version and merge
  dfindSP <- queryTable("cqt45yktk2m8ky3",
                   "Código" = "cob8rivktedzp0f3",
                   "Sector" = "c84rjfckxgbve582",
                   "Indicador" = "cwkj9p4kteeh4ls5")%>%
    left_join(dfindicator, by = c("Código" = "Code"))%>%
    select(-Indicatortype, -sectindic)
  
  dfsector <- dfindSP %>%
    select(Sector, Subsector, Código)%>%
    rename(type = Código,
           English = Subsector,
           Spanish = Sector)%>%
    mutate(type = "Sector")%>%
    group_by(Spanish, English, type)%>%
    summarise()
  
  dfindicators <- dfindSP %>%
    select(Indicador, Indicator, Código)%>%
    rename(type = Código,
           English = Indicator,
           Spanish = Indicador)%>%
    mutate(type = "Indicator")%>%
    group_by(Spanish, English, type)%>%
    summarise()
    
  # Delivery mechanism
  # BUild from scratch as there are no ref table in Activity info
  
  EnglishDelivery <- c("Physical cash ", "Mobile money transfer", "Bank transfer","Other electronic cash mechanisms", "Vouchers", "Others")
  SpanishDelivery <- c("Efectivo", "Transferencia via móvil", "Transferencia Bancaria", "Otros mecanismos de dinero electrónico", "Cupones", "Otros")
  
  dfmechanism <- data.frame(EnglishDelivery, SpanishDelivery)%>%
    rename(English = EnglishDelivery,
           Spanish = SpanishDelivery)%>%
    mutate(type = "mechanism")
  
    
  # Yes/No questions
    
  English <- c("Yes", "Yes", "No")
  Spanish <- c("Sí", "Si", "No")
  type <- c("yesno", "yesno", "yesno")
  
  dfyesno <- data.frame(English, Spanish, type)
  
  # Get dictionnary together
  
  dictionnary <- rbind(dfAO2, dfIP2, dfsector, dfindicators, dfmechanism, dfyesno)%>%
    summarise(Spanish, English, type)
  
  ################################# Get data and translate #######################
  
  # Insert local file to get data in Spanish
  
  df5WSpanish <- read_excel("./data/RMRP2022ActivitiesSP.xlsx")
  
  # Change column names to english templamte version
  
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
  
 
  
  # Translation function
  translate_to_english <- function(df, col_esp, dict, group){
    dict_group <- dict %>% 
      filter(type == group)
    
    eng_term <-  dict_group[amatch(df %>% select(as.name(col_esp)) %>% pull(),
                                   dict_group$Spanish,
                                  maxDist=2),
                           2] %>%
      pull()
    
    return(eng_term)
  }
  
  df5Wtranslated <- df5WSpanish  %>%
    mutate(`Appealing_org` = translate_to_english(df5WSpanish,"Appealing_org", dictionnary, 'AO'))
           
           
      Implementing_partner = translate_to_english(df5WSpanish, "Implementing_partner", dictionnary, 'IP'),
      Implementation = translate_to_english(df5WSpanish,"Implementation", dictionnary, "yesno"),
      Subsector = translate_to_english(df5WSpanish, "Subsector", dictionnary, "Sector"),
      Indicator = translate_to_english(df5WSpanish," Indicator", dictionnary, "Indicator"),
      COVID19 = translate_to_english(df5WSpanish, "COVID19", dictionnary, "yesno"),
      RMRPActivity = translate_to_english(df5WSpanish,"RMRPActivity", dictionnary, "yesno"),
      CVA = translate_to_english(df5WSpanish, "CVA", dictionnary, "yesno"),
      Delivery_mechanism = translate_to_english(df5WSpanish, "Delivery_mechanism", dictionnary, "mechanism")
    ) 
  
  write_xlsx(df5Wtranslated, './out/Translated5W.xlsx')
  
  }