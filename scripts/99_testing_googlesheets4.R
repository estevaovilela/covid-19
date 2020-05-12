

# Library -----------------------------------------------------------------

library(data.table)
library(googlesheets4)
library(tidyverse)

# Reading -----------------------------------------------------------------

current_day <- Sys.Date()

# daily deaths accumulated
df <- fread(paste0("./data/treated/deaths_br_", 
                   paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                   "_covid_project_accumulated.csv"))

# testing Spreadsheet
sheet_write(df, 
            ss = "https://docs.google.com/spreadsheets/d/1pmcgZZwOXt5XRioupSRpXXYEf2_kOAP7zLnMVMQZT0M/edit#gid=0", 
            sheet = "database")
