

# Library -----------------------------------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

clean_data <- function(raw) {
  
  data <- raw
  data <- gsub("\"", replacement = "", x = data)
  data <- gsub(" ", replacement = "", x = data)
  data <- gsub("\\[", replacement = "", x = data)
  data <- gsub("\\]", replacement = "", x = data)
  data <- str_split(data, pattern = ",")
  data
}

# Reading -----------------------------------------------------------------

# the HTML is read in lines
covid_alberta_html <- readLines("./data/raw/covid_alberta_10_05_2020.html")

# Where is the data -------------------------------------------------------
current_day <- Sys.Date()

occurrences <- grep(pattern = "data-for=\"htmlwidget-32e17777be5dc623828e\"", 
                    x = covid_alberta_html)

# with mannual inspection we know the correct line is 2255
raw <- str_extract(string = covid_alberta_html[occurrences], 
                   # everything enclosed in two brackets
                   # the data is in 'list-of-lists'
                   # data: [[1,2,3], [male, female]]
                   pattern = "(\\[{2}.*\\]{2})")

# so, we'll start getting the data in vectors, then 
# we'll create a data frame. We capture each variable 
# one-by-one and then remove it from the raw data
# we only capture the first bracket
# "Instead you can make your dot-star non-greedy, 
# which will make it match as few characters as possible:"
# https://stackoverflow.com/questions/2503413/regular-expression-to-stop-at-first-match

# id row
id_vec <- str_extract(string = raw, pattern = "(\\[{1}.*?\\]{1})")
id_vec <- clean_data(id_vec)
raw_treated <- str_remove(string = raw, pattern = "(\\[{1}.*?\\]{1})")

# date reported
date_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
date_vec <- clean_data(date_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

# zone
zone_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
zone_vec <- clean_data(zone_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

# gender
gender_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
gender_vec <- clean_data(gender_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

# age group
age_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
age_vec <- clean_data(age_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

# case status
case_status_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
case_status_vec <- clean_data(case_status_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

# case type
case_type_vec <- str_extract(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")
case_type_vec <- clean_data(case_type_vec)
raw_treated <- str_remove(string = raw_treated, pattern = "(\\[{1}.*?\\]{1})")

df <- tibble(id = id_vec ,
             date_reported = date_vec,
             zone = zone_vec,
             gender = gender_vec,
             age_group = age_vec,
             case_status = case_status_vec,
             case_type = case_type_vec)

df <- df %>% 
  unnest()

# Output ------------------------------------------------------------------

write.csv(df, file = paste0("./data/treated/cases_alberta_canada_", 
                                               paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                                               "_covid_project.csv") , 
          row.names = FALSE)
  

