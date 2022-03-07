# The following steps have been thought to integrally cover the data cleaning process of the RMRP 2022 database. 
# It's use can be adapted to obtain general or country specific data.

## Author: R4V IM Team, Francis Fayolle and James Léon-Dufour

# 1_read_data
# Mandatory step to obtain 5W data that will then be process in the following steps of the process
# Options:source: "file": reads data from XLS file "activityinfo": Activity Info API download
# Default: "activityinfo" 
# write: "yes": saves files, "no": do not save
# default: "no"

source("R/1_read_data.R")
read_data_2022(source = "activityinfo",
               write = "yes")


# Data Quality Check
# Option: filter your country
# write the report in a local repository
source("R/2_data_quality_check.R")
r4v_error_report(countryname = NULL,
               write = "yes")


# Consolidated report
# Filter by country and by consolidated methodology
# Consolidating methodologies:
# Sum: sum  all beneficiaries at admin1 level per sector and intersector

source("R/3_consolidated_report.R")
r4v_consolidated(data,countryname = "Costa Rica", 
                 proportions = "pin", 
                 totalmodel = "maxsector")


# Translation: translate fro; spanish to English
source("R/4_Translation.R")
rmrp_translate()