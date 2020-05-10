

# Library -----------------------------------------------------------------

library(tidyverse)

# Functions ---------------------------------------------------------------

clean_data <- function(data_url, index, column) {
  
  data <- data_url[index]
  data <- gsub("\"", replacement = "", x = data)
  data <- gsub(" ", replacement = "", x = data)
  data <- gsub("\\[", replacement = "", x = data)
  data <- str_split(data, pattern = ",")
  if(column != 7) {
    data <- data[[1]][-6158]
    data
  } else {
    unlist(data)
  }
}

# Reading -----------------------------------------------------------------

# the HTML is read in lines, and each line is a element
# in a vector
covid_alberta_html <- readLines("/home/estevao/Desktop/helloworld.html")

# Where is the data -------------------------------------------------------
current_day <- Sys.Date()

occurrences <- grep(pattern = "data-for=\"htmlwidget-32e17777be5dc623828e\"", 
                    x = covid_alberta_html)

# with mannual inspection we know the correct line is 5389, or the last one
index_html <- rev(occurrences)[1]

grep(pattern = "data", covid_alberta_html[occurrences])

# so, we'll start getting the data in vectors, then 
# we'll create a data frame. The firts columns (the row id)
# is in the next element of the vector: index_html + 1, and so on.

# id row
id_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 1, column = 1)

# date reported
date_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 2, column = 2)

# zone
zone_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 3, column = 3)

# gender
gender_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 4, column = 4)

# age group
age_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 5, column = 5)

# case status
case_status_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 6, column = 6)

# case type
case_type_vec <- clean_data(data_url = covid_alberta_html, index = index_html + 7, column = 7)

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
                                               "_covid_project_.csv") , 
          row.names = FALSE)
  

