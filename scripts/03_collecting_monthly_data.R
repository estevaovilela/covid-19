
# Library -----------------------------------------------------------------

library(httr)
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
# getting the needed token
# login
login_url <- "https://transparencia.registrocivil.org.br/registros"
# Start with a fresh handle
h <- curl::new_handle()
# Ask server to set some cookies
request <- curl::curl_fetch_memory(login_url, handle = h)
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#reading_cookies
cookies <- curl::handle_cookies(h)
token <- cookies$value[which(cookies$name == "XSRF-TOKEN")]

# dates
june <- dmy("01/06/2020")
current_day <- Sys.Date()
year <- "2019"
dates <- seq.Date(dmy(paste0("01/01/", year)), june, by = "month")

# curl into httr:
# https://curl.trillworks.com/#r
headers <- c(
  `X-XSRF-TOKEN` = token,
  # dont know if it is a private info
  `User-Agent` = # Your User-Agent
)

# States ------------------------------------------------------------------

# number of deaths in each brazilian state staring in 'year', monthly
# example: january/2019
# "https://transparencia.registrocivil.org.br/api/record/death?start_date=2019-01-01&end_date=2019-01-31"

df_states <- tibble()

list_death <- list()
list_state <- list()

for(i in 1:(length(dates) - 1)) {
  
  # curl into httr
  # https://curl.trillworks.com/#r
  # we retrieving data by month
  # so, for example, we are retrivieng data between
  # 01/06/2018 and 30/06/2018 (01/07/2018 - 1)
  params <- list(
    `start_date` = dates[i],
    `end_date` = dates[i+1]-1
  )
  
  response <- httr::GET(url = 'https://transparencia.registrocivil.org.br/api/record/death', 
                        httr::add_headers(.headers=headers), 
                        query = params)
  
  raw_data <- httr::content(response)
  
  for(j in 1:length(raw_data$data)) {
    states <- raw_data["data"][[1]][[j]]["name"][[1]]
    list_state <- append(list_state, states)
    
    deaths <- raw_data["data"][[1]][[j]]["total"][[1]]
    list_death <- append(list_death, deaths)
  }
  
  df_states_temp <- tibble(list_state, list_death) %>% 
    unnest() %>% 
    mutate(month = lubridate::month(dates[i]),
           year = lubridate::year(dates[i]))
  
  df_states <- df_states %>% 
    bind_rows(df_states_temp)
  
  list_death <- list()
  list_state <- list()
  
  cat("i: ", i, "\n")
  # Pause for 0.1 seconds
  Sys.sleep(0.1)
}

# Cities ------------------------------------------------------------------

df_states_aux <- tibble(
  state = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
            "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
            "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  state_code = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21,
                 51, 50, 31, 15, 25, 41, 26, 22, 33, 24, 43,
                 11, 14, 42, 35, 28, 17))

# number of deaths in each brazilian state staring in 2019, monthly
# january/2019 just for Minas Gerais
# "https://transparencia.registrocivil.org.br/api/record/death?start_date=2015-01-01&end_date=2015-01-31&state=MG"

df_cities <- tibble()

list_death <- list()
list_city <- list()

for(i in 1:length(df_states_aux$state)) {
  for(j in 1:(length(dates) - 1)) {
    # curl into httr
    # https://curl.trillworks.com/#r
    # we retrieving data by month
    # so, for example, we are retrivieng data between
    # 01/06/2019 and 30/06/2019 (01/07/2019 - 1)
    params <- list(
      `start_date` = dates[j],
      `end_date` = dates[j+1]-1,
      `state` = df_states_aux$state[i]
    )
    
    response <- httr::GET(url = 'https://transparencia.registrocivil.org.br/api/record/death', 
                          httr::add_headers(.headers=headers), 
                          query = params)
    
    raw_data <- httr::content(response)
    
    for(k in 1:length(raw_data$data)) {
      city <- raw_data["data"][[1]][[k]]["name"][[1]]
      list_city <- append(list_city, city)
      
      deaths <- raw_data["data"][[1]][[k]]["total"][[1]]
      list_death <- append(list_death, deaths)
    }
    
    df_cities_temp <- tibble(list_city, list_death) %>% 
      unnest() %>% 
      mutate(state = df_states_aux$state[i],
             state_code = df_states_aux$state_code[i],
             month = lubridate::month(dates[j]),
             year = lubridate::year(dates[j]))
    
    list_death <- list()
    list_city <- list()
  
    df_cities <- df_cities %>% 
      bind_rows(df_cities_temp)
  
    cat("i: ", i, " j: ", j, "\n")
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
                            "_by_month_state_since_2019.csv") , 
          row.names = FALSE)

# cities
df_cities <- df_cities %>% 
  setNames(c("city", "number_deaths", "state", "state_code", "month", "year"))

write.csv(df_cities, file = paste0("./data/treated/all_deaths_br_", 
                                   paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                                   "_by_month_city_since_2019.csv") , 
          row.names = FALSE)
