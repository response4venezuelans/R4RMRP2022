# Consolidated report RMRP 2022

# function writing

r4v_consolidated <- function(data,countryname = NULL, totalmodel = "sum")

# Packages

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)

# Filter by the needed country and PiN indicators only

if (is.null(countryname) || (countryname=="All")) {
  df5Wconsolidated <- df5W %>%
    left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
    select(-Code, -sectindic)%>%
    filter(Indicatortype == "PiN" & RMRPActivity == "Yes")
} else {
  df5Wconsolidated <- df5W %>% filter(Country == countryname)%>%
    left_join(dfindicator, by = c("Subsector", "Indicator"))%>%
    select(-Code, -sectindic)%>%
    filter(Indicartortype == "PiN" & RMRPActivity == "Yes")  
}

# Get consolidated template file

dftemplate <- read_excel("data/Consolidated_Template.xlsx")

if (is.null(countryname) || (countryname=="All")) {
  dftemplate <- dftemplate
} else {
  dftemplate <- dftemplate%>%
    filter(Country == countryname) 
}

#################### 1. Total Monthly figures #################################################################

# Get monthly total figures of persons per Admin1 per sector
# sum Total beneficiaries of every activities 

monthlysectors <- df5Wconsolidated%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise('Monthly Total Beneficiaries' = sum(Total_monthly))

monthlytotal <- monthlysectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector", 'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`))

monthly<- rbind(monthlysectors, monthlytotal)

# Get CVA Beneficiaries monthly total figures of persons per Admin1 per sector
# sum Total beneficiaries of every activities 

CVAmonthlysectors <- df5Wconsolidated%>%
  filter(CVA == 'Yes')%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise('Monthly CVA Beneficiaries' = sum(Total_monthly))

CVAmonthlytotal <- CVAmonthlysectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector", 'Monthly CVA Beneficiaries' = sum(`Monthly CVA Beneficiaries`))

CVAmonthly<- rbind(CVAmonthlysectors, CVAmonthlytotal)

finalmonthly <- monthly%>%
  full_join(CVAmonthly, by = c("Country", "Admin1", "Month", "Subsector"))

#################### 2. Consolidated figures #################################################################

###### 2.1 Sector level ################
# Figures at sector and admin1 level are solely calculated through a sum
# If required by some platform, we can develop a indicator based system tailored for countries.

conssectors <-  df5Wconsolidated%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise(Monthly_Consolidated = sum(New_beneficiaries),
            Consolidated_RMindestination = sum(IN_DESTINATION), 
            Consolidated_RM_in_transit = sum(IN_TRANSIT),
            Consolidated_Host_Community = sum(Host_Communities),
            Consolidated_RM_Pendulars = sum(PENDULARS),
            Consolidated_Colombian_Returnees = sum(Returnees),
            Consolidated_Girls = sum(Girls), 
            Consolidated_Boys = sum(Boys),
            Consolidated_Women = sum(Women),
            Consolidated_Men = sum(Men),
            Consolidated_Other_under_18 = sum(Other_under),
            Consolidated_Other_above_18 = sum(Other_above)
  )

consCVAsectors <- df5Wconsolidated%>%
  filter(CVA == "Yes")%>%
  group_by(Country, Admin1, Month, Subsector)%>%
  summarise(Consolidated_CVA_Beneficiaries = sum(New_beneficiaries))

consallsectors <- conssectors%>%
  full_join(consCVAsectors, by = c("Country", "Admin1", "Month", "Subsector"))

###### 2.2 Intersector level ################
# Chose the consoldated model you want to use (see "Consolidated guidance" document 
# in GitHub repository)


# Model 1: Sum All: sums all beneficiaries of all sectors to get intersector figures
# per admin1 level and then at national level
if (totalmodel == "sum")
  conssumadm1 <- consallsectors%>%
  group_by(Country, Admin1, Month)%>%
  summarise(Subsector = "Intersector",
            Monthly_Consolidated = sum(Monthly_Consolidated),
            Consolidated_RMindestination = sum( Consolidated_RMindestination), 
            Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
            Consolidated_Host_Community = sum(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
            Consolidated_Girls = sum(Consolidated_Girls), 
            Consolidated_Boys = sum(Consolidated_Boys),
            Consolidated_Women = sum( Consolidated_Women),
            Consolidated_Men = sum(Consolidated_Men),
            Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
            Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
            Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries)
            )
# Join Admin1-Intersector table to Admin1-Sector table
consfinaladmin1 <- rbind(consallsectors, conssumadm1)

# Sum Admin1 figures for sector and intersector to get the full final table
consfinalcountrylevel <- consfinaladmin1 %>%
  group_by(Country, Month, Subsector)%>%
  summarise(Admin1 = "Country level",
            Monthly_Consolidated = sum(Monthly_Consolidated),
            Consolidated_RMindestination = sum( Consolidated_RMindestination), 
            Consolidated_RM_in_transit = sum(Consolidated_RM_in_transit),
            Consolidated_Host_Community = sum(Consolidated_Host_Community),
            Consolidated_RM_Pendulars = sum(Consolidated_RM_Pendulars),
            Consolidated_Colombian_Returnees = sum(Consolidated_Colombian_Returnees),
            Consolidated_Girls = sum(Consolidated_Girls), 
            Consolidated_Boys = sum(Consolidated_Boys),
            Consolidated_Women = sum( Consolidated_Women),
            Consolidated_Men = sum(Consolidated_Men),
            Consolidated_Other_under_18 = sum(Consolidated_Other_under_18),
            Consolidated_Other_above_18 = sum(Consolidated_Other_above_18),
            Consolidated_CVA_Beneficiaries = sum(Consolidated_CVA_Beneficiaries)
  )

consfinal <- rbind(consfinaladmin1, consfinalcountrylevel)%>%
  arrange(Country, Admin1)

