
# Library -----------------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(lubridate)

# Functions ---------------------------------------------------------------

url_function <- function(start_date, end_date) {
  paste0("https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=",
         start_date,
         "&end_date=",
         end_date,
         "&state=Todos&search=death-covid&groupBy=gender")
}

# Collecting --------------------------------------------------------------

# we are collecting a JSON file

# here we are collecting deaths that happenened in Brazil from Covid-19
# from 01/01/2020 to 30/04/2020, grouped by age (10 years group) and gender.
url <- "https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-01-01&end_date=2020-04-30&state=Todos&search=death-covid&groupBy=gender"

data <- jsonlite::fromJSON(url)

# so, we need to iterate through only dates to get our data:
# number of daily deaths by gender and age group (10 years) 
# since the first death in Brazil

current_day <- Sys.Date()

dates <- seq.Date(dmy("14/03/2020"), current_day, by = "day")

list_date <- list()
list_age <- list()
list_gender <- list()
list_death <- list()

df <- tibble()

for(i in 1:length(dates)) {
  
  # we retrieving data day by day
  url <- url_function(dates[i], dates[i])
  raw_data <- jsonlite::fromJSON(url)
  age_groups <- names(raw_data["chart"][[1]])

  for(j in 1:length(age_groups)) {
    
    genders <- names(raw_data["chart"][[1]][[j]])
      
    if(length(genders) != 0) {
      for(k in 1:length(genders)) {
        list_date <- append(list_date, dates[i])
        list_age <- append(list_age, age_groups[j])
        list_gender <- append(list_gender, genders[k])
        
        # j: age group
        # k: gender F or M
        deaths <- raw_data["chart"][[1]][j][[1]][[k]]
        list_death <- append(list_death, deaths)
      }
    } 
  }
  
  df_temp <- tibble(list_date, list_age, list_gender, list_death) %>% 
    unnest()
    
  list_date <- list()
  list_age <- list()
  list_gender <- list()
  list_death <- list()
  
  df <- df %>% 
    bind_rows(df_temp)
  
  # Pause for 0.1 seconds
  Sys.sleep(2)
  
  cat("i: ", i, " j: ", j, "\n")
}

# All combinations dataset ------------------------------------------------

df_combinations <- purrr::cross_df(
  .l = list(
    "Date" = df %>% select(list_date) %>% unique() %>% pull(),
    "Sex" = df %>% select(list_gender) %>% unique() %>% pull(),
    "Age" = df %>% select(list_age) %>% unique() %>% pull())
)

df_combinations <- df_combinations %>% 
  mutate(Date = lubridate::as_date(Date)) 

# Output ------------------------------------------------------------------

# daily deaths
df_output <- df_combinations %>% 
  left_join(df, by = c("Date" = "list_date", 
                       "Age" = "list_age", 
                       "Sex" = "list_gender")) %>% 
  mutate(Value = ifelse(is.na(list_death), 0, list_death)) %>% 
  select(-list_death)

# daily deaths accumulated
df_output_accumulated <-  df_output %>%
  arrange(Sex, Age, Date) %>% 
  group_by(Sex, Age) %>% 
  mutate(Value_accumulated = cumsum(Value))

# Fixing columns ----------------------------------------------------------

# daily deaths
df_output <- df_output %>% 
  mutate(Age = case_when(
    Age == "< 9" ~ "0",
    Age == "> 100" ~ "100",
    TRUE ~ str_extract(Age, pattern = "^\\d{2}"))
  ) %>% 
  mutate(Age = as.integer(Age)) %>% 
  arrange(Date, Sex, Age) %>% 
  separate(col = Date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  unite(col = Date, ... = c("day", "month", "year"), sep = ".", remove = TRUE) %>% 
  mutate(Sex = ifelse(Sex == "F", "f", "m")) %>% 
  mutate(Country = "Brazil",
         Region = "All",
         AgeInt = 10,
         Metric = "Count",
         Measure = "Deaths")

df_output <- df_output %>% 
  select(Country, Region, Date,
         Sex, Age, AgeInt, Metric, Measure,
         Value)

# daily deaths accumulated
df_output_accumulated <- df_output_accumulated %>% 
  ungroup() %>% 
  mutate(Age = case_when(
    Age == "< 9" ~ "0",
    Age == "> 100" ~ "100",
    TRUE ~ str_extract(Age, pattern = "^\\d{2}"))
  ) %>% 
  mutate(Age = as.integer(Age)) %>% 
  arrange(Date, Sex, Age) %>% 
  separate(col = Date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>% 
  unite(col = Date, ... = c("day", "month", "year"), sep = ".", remove = TRUE) %>% 
  mutate(Sex = ifelse(Sex == "F", "f", "m")) %>% 
  mutate(Country = "Brazil",
         Region = "All",
         AgeInt = 10,
         Metric = "Count",
         Measure = "Deaths") %>% 
  mutate(Value = Value_accumulated)

df_output_accumulated <- df_output_accumulated %>% 
  select(Country, Region, Date,
         Sex, Age, AgeInt, Metric, Measure,
         Value, -Value_accumulated)

# Writing -----------------------------------------------------------------

# daily deaths
write.csv(df_output, file = paste0("./data/treated/deaths_br_", 
                            paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                            "_covid_project.csv") , 
          row.names = FALSE)

# daily deaths accumulated
write.csv(df_output_accumulated, file = paste0("./data/treated/deaths_br_", 
                                   paste(rev(str_split(current_day, pattern = "-")[[1]]), collapse = "_"), 
                                   "_covid_project_accumulated.csv") , 
          row.names = FALSE)
