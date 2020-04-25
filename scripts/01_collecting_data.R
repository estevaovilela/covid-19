
# Library -----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(lubridate)

# Functions ---------------------------------------------------------------

url_function <- function(state, start_date, end_date) {
  paste0("https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&search=death-covid&state=",
         state,
         "&start_date=",
         start_date,
         "&end_date=",
         end_date,
         "&groupBy=gender")
}

# Collecting --------------------------------------------------------------

# we are collecting a JSON file

# here we are collecting deaths that happenened in Bahia from Covid-19
# from 01/01/2020 to 23/04/2020, grouped by age (10 years group) and gender.
url <- "https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&search=death-covid&state=BA&start_date=2020-01-01&end_date=2020-04-23&groupBy=gender"

data <- jsonlite::fromJSON(url)

# so, we need to iterate through states and dates to get our data:
# number of deaths by gender and age group (10 years) for each
# state and date since the first death in Brazil

current_day <- Sys.Date()

states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
            "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
            "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

dates <- seq.Date(dmy("16/03/2020"), current_day, by = "day")

list_state <- list()
list_date <- list()
list_age <- list()
list_gender <- list()
list_death <- list()

df <- tibble()

for(i in 1:length(states)) {
  for(j in 1:length(dates)) {
    # we retrieving data day by day
    url <- url_function(states[i], dates[j], dates[j])
    
    raw_data <- jsonlite::fromJSON(url)
    
    age_groups <- names(raw_data["chart"][[1]])
    
    for(k in 1:length(age_groups)) {
      
      genders <- names(raw_data["chart"][[1]][[k]])
      
      if(length(genders) != 0) {
        for(m in 1:length(genders)) {
          list_state <- append(list_state, states[i])
          list_date <- append(list_date, dates[j])
          list_age <- append(list_age, age_groups[k])
          list_gender <- append(list_gender, genders[m])
          
          # k: age group
          # m: gender F or M
          deaths <- raw_data["chart"][[1]][k][[1]][[m]]
          list_death <- append(list_death, deaths)
        }
      } 
    }
    
    df_temp <- tibble(list_state, list_date, list_age, list_gender, list_death) %>% 
      unnest()
    
    list_state <- list()
    list_date <- list()
    list_age <- list()
    list_gender <- list()
    list_death <- list()
    
    df <- df %>% 
      bind_rows(df_temp)
    
    # Pause for 0.1 seconds
    Sys.sleep(0.1)
  }
}

# Output ------------------------------------------------------------------

df <- df %>% 
  setNames(c("date", "state", "age_group", "gender", "number_deaths"))

write.csv(df, file = paste0("./data/treated/deaths_br_", 
                            paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                            "_by_gender_age_date_state.csv") , 
          row.names = FALSE)
