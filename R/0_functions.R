# The following steps have been thought to integrally cover the data cleaning process of the RMRP 2022 database. 
# It's use can be adapted to obtain general or country specific data.

## Author: R4V IM Team, Francis Fayolle and James Léon-Dufour

## 

# 1_read_data
# Mandatory step to obtain 5W data that will then be process in the following steps of the process
# Options:source: "file": reads data from XLS file  "activityinfo": Activity Info API download
# Default: "activityinfo" 
# write: "yes": saves files, "no": do not save
# default: "no"

source("R/1_read_data.R")
read_data_2022(source = "activityinfo",
               write = "no")


## put dictionary in AI 