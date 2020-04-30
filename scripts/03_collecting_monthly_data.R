
# Library -----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(lubridate)

# Functions ---------------------------------------------------------------

url_function <- function(start_date, end_date) {
  paste0("https://transparencia.registrocivil.org.br/api/record/death?start_date=",
         start_date,
         "&end_date=",
         end_date)
}

# Collecting --------------------------------------------------------------

# we are collecting a JSON file: number of deaths in each brazilian state
# staring in 2015, monthly

# january/2015
url <- "https://transparencia.registrocivil.org.br/api/record/death?start_date=2015-01-01&end_date=2015-01-31"

data <- jsonlite::fromJSON(url)

# so, we need to iterate through dates to get our data:
# number of deaths by for each state

may <- dmy("01/05/2020")
current_day <- Sys.Date()
dates <- seq.Date(dmy("01/01/2015"), may, by = "month")

df <- tibble()

for(i in 1:(length(dates) - 1)) {
  # we retrieving data by month
  # so, for example, we are retrivieng data between
  # 01/06/2018 and 30/06/2018 (01/07/2018 - 1)
  url <- url_function(dates[i], dates[i+1]-1)
  
  raw_data <- jsonlite::fromJSON(url)
  
  df_temp <- as_tibble(raw_data$data) %>% 
    mutate(month = lubridate::month(dates[i]),
           year = lubridate::year(dates[i]))
  
  df <- df %>% 
    bind_rows(df_temp)
  
  # Pause for 0.1 seconds
  Sys.sleep(0.1)
}

# Output ------------------------------------------------------------------

df <- df %>% 
  setNames(c("state", "number_deaths", "month", "year"))

write.csv(df, file = paste0("./data/treated/all_deaths_br_", 
                            paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                            "_by_month_state_since_2015.csv") , 
          row.names = FALSE)
