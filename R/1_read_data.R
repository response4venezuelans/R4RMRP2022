# Read data 2022
# function

read_data_2022 <- function(data, 
                           write ="yes")
{ 

# Credentials located in seperate file
source("R/ai_credentials.R")

# Get data from different sources

    df5W <- queryTable("cw1o8nbkx69lc9l3",
                     "Country" = "cezj1rqkxeqrsy57.c8u26b8kxeqpy0k4",
                     "Country Admin1" = "cezj1rqkxeqrsy57.c3ns3zikxeqq4h95",
                     "Admin2" = "c89klrbkx6hp4j58.cs2esadkx6hkt7j6",
                     "Appealing organisation Name" = "c5648gjkx69ra2v9.ckj5zamkumvyysv9",
                     "Implementation Set up" = "ckjtet4kx69smeog",
                     "Implementing partner Name" = "cflyi17kx69vdxei.ckj5zamkumvyysv9",
                     "Month" = "clqgqrqkyueahma8",
                     "Subsector" = "cq7t4s8kx6a7fyx3.cgdeh97ktn4sdek3s.cfvkmslkpy3tg94n",
                     "Indicator" = "cq7t4s8kx6a7fyx3.cwkj9p4kteeh4ls5",
                     "Activity Name" = "c3p669wkx6a7oyo4",
                     "Activity Description" = "c8hxf50kx6a7vp65",
                     "COVID 19 Situation" = "c3sg2p7kx6am3uk8",
                     "RMRP Activity" = "cuf3og8kx6amylmf",
                     "CVA" = "cbvqg4jkx6b1kii7",
                     "Value (in USD)" = "clwkfmckx6b2msu9",
                     "Delivery mechanism" = "cg3rikqkx6b3z1kf",
                     "Quantity of output" = "cm6no26kx6b8fqoh",
                     "Total monthly beneficiaries" = "cto1biukx6kwvnj4k",
                     "New beneficiaries of the month" = "c43j49ikx6kxyyc4l",
                     "Refugees and Migrants IN DESTINATION" = "cz3yof2kx6l024p4m",
                     "Refugees and Migrants IN TRANSIT" = "c8kl5o2kx6l0jip4n",
                     "Host Communities Beneficiaries" = "c5z8bvakx6l10d84o",
                     "Refugees and Migrants PENDULARS" = "c72dmskkx6l1hl04p",
                     "Colombian Returnees" = "cmoqhuckx6l4q9z4q",
                     "Women under 18" = "cwrxeaekx6l63na4s",
                     "Men under 18" = "ccx7xhekx6l6jnk4t",
                     "Women above 18" = "c3l36n2kx6l70kp4u",
                     "Men above 18" = "ctd27ackx6l7g814v",
                     "Other under 18" = "ckjcuiokx6l9a504w",
                     "Other above 18" = "cq4hs3skx6lggpj4x", truncate.strings = FALSE)

 # format column names for easier data processing
  
  colnames(df5W) <- c("Country",
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
  
  df5W <<- df5W %>%
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

  if(write == "yes"){
    write_xlsx(df5W, "./data/Activities_5W_2022.xlsx")
    } 
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

  dfindicator  <<- queryTable("c49gyhmktedz4uj2",
                   "Code" = "cob8rivktedzp0f3",
                   "Subsector" = "cgdeh97ktn4sdek3s.cfvkmslkpy3tg94n",
                   "Indicator" = "cwkj9p4kteeh4ls5",
                   "Indicatortype" = "cprepl2ktk2l76a3", truncate.strings = FALSE)%>%
    rowwise()%>%
    mutate(sectindic = paste(Subsector, Indicator))%>%
    ungroup
  
  dfindSP <<- queryTable("cqt45yktk2m8ky3",
                        "Codigo" = "cob8rivktedzp0f3",
                        "SectorSP" = "c84rjfckxgbve582",
                        "Indicador" = "cwkj9p4kteeh4ls5", truncate.strings = FALSE)  
  
  dfAO  <<- queryTable("cbisyyxkumvyhy57",
                   "AOIDORG" = "cnhvpo4kumvyqla8",
                   "Name" = "ckj5zamkumvyysv9",
                   "Nombre" = "cpmcp88kumvz7bsa", truncate.strings = FALSE)

  dfIP  <<- queryTable("cuy0fjukumwabck4",
                   "IPID" = "cd2ow0jkumwazdl1h",
                   "Name" = "ckj5zamkumvyysv9",
                   "Nombre" = "cpmcp88kumvz7bsa", truncate.strings = FALSE)
 return(df5W)
  
} 
