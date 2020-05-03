
# Library -----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(lubridate)

# Functions ---------------------------------------------------------------

url_function_state <- function(start_date, end_date) {
  paste0("https://transparencia.registrocivil.org.br/api/record/death?start_date=",
         start_date,
         "&end_date=",
         end_date)
}

url_function_city <- function(start_date, end_date, state) {
  paste0("https://transparencia.registrocivil.org.br/api/record/death?start_date=",
         start_date,
         "&end_date=",
         end_date,
         "&state=",
         state)
}

# Collecting --------------------------------------------------------------

# so, we need to iterate through dates to get our data:
# number of deaths by for each state and for each city -
# iterating through each state

may <- dmy("01/05/2020")
current_day <- Sys.Date()
dates <- seq.Date(dmy("01/01/2015"), may, by = "month")

# States ------------------------------------------------------------------

# we are collecting a JSON file: number of deaths in each brazilian state
# staring in 2015, monthly

# example: january/2015
url <- "https://transparencia.registrocivil.org.br/api/record/death?start_date=2015-01-01&end_date=2015-01-31"

data <- jsonlite::fromJSON(url)

df_states <- tibble()

for(i in 1:(length(dates) - 1)) {
  # we retrieving data by month
  # so, for example, we are retrivieng data between
  # 01/06/2018 and 30/06/2018 (01/07/2018 - 1)
  url <- url_function_state(dates[i], dates[i+1]-1)
  
  raw_data <- jsonlite::fromJSON(url)
  
  df_states_temp <- as_tibble(raw_data$data) %>% 
    mutate(month = lubridate::month(dates[i]),
           year = lubridate::year(dates[i]))
  
  df_states <- df_states %>% 
    bind_rows(df_states_temp)
  
  cat("i: ", i, "\n")
  # Pause for 0.1 seconds
  Sys.sleep(0.1)
}

# Cities ------------------------------------------------------------------

states <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
            "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
            "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")


# we are collecting a JSON file: number of deaths in each brazilian state
# staring in 2015, monthly

# january/2015 just for Minas Gerais
url <- "https://transparencia.registrocivil.org.br/api/record/death?start_date=2015-01-01&end_date=2015-01-31&state=MG"

data <- jsonlite::fromJSON(url)

df_cities <- tibble()

for(i in 1:length(states)) {
  for(j in 1:(length(dates) - 1)) {
    # we retrieving data by month
    # so, for example, we are retrivieng data between
    # 01/06/2018 and 30/06/2018 (01/07/2018 - 1)
    url <- url_function_city(dates[j], dates[j+1]-1, states[i])
    
    raw_data <- jsonlite::fromJSON(url)
  
    df_cities_temp <- as_tibble(raw_data$data) %>% 
      mutate(state = states[i],
             month = lubridate::month(dates[j]),
             year = lubridate::year(dates[j]))
  
    df_cities <- df_cities %>% 
      bind_rows(df_cities_temp)
  
    # Pause for 0.1 seconds
    Sys.sleep(0.1)
  }
}

# Output ------------------------------------------------------------------

# the results are the same, as we should expect

# states
df_states <- df_states %>% 
  setNames(c("state", "number_deaths", "month", "year"))

write.csv(df_states, file = paste0("./data/treated/all_deaths_br_", 
                            paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                            "_by_month_state_since_2015.csv") , 
          row.names = FALSE)

# cities
df_cities <- df_cities %>% 
  setNames(c("number_deaths", "city", "state", "month", "year"))

write.csv(df_cities, file = paste0("./data/treated/all_deaths_br_", 
                                   paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                                   "_by_month_city_since_2015.csv") , 
          row.names = FALSE)
