
# Library -----------------------------------------------------------------

library(httr)
library(tidyverse)
library(lubridate)
library(googlesheets4)

# Collecting --------------------------------------------------------------

# deaths that happenened in Brazil from Covid-19
# from 01/01/2020 to 30/04/2020, grouped by age (10 years group) and gender.
# "https://transparencia.registrocivil.org.br/api/covid?data_type=data_ocorrido&start_date=2020-01-01&end_date=2020-04-30&state=Todos&search=death-covid&groupBy=gender"
# but, for now we need a token to request the data

# getting the needed token
# login
login_url <- "https://transparencia.registrocivil.org.br/especial-covid"
# Start with a fresh handle
h <- curl::new_handle()
# Ask server
request <- curl::curl_fetch_memory(login_url, handle = h)
# https://cran.r-project.org/web/packages/curl/vignettes/intro.html#reading_cookies
cookies <- curl::handle_cookies(h)
# we need the "XRSF-TOKEN"
token <- cookies$value[which(cookies$name == "XSRF-TOKEN")]
# if this don't succed need to get it manually
token <- "eyJpdiI6IkRlazNaY0ZLY0RrVERrdEx5MU5tc2c9PSIsInZhbHVlIjoib2tPdGJnY0s0MkJXTmUrSmIrOVdcL2hsK3FUZXNrdVczRVl3MUlSeUV2a1N5d0lwM1pneFd5V3NGUE15a0xzZnUiLCJtYWMiOiI2NTg5YWUwMTI5YWMwYTFhM2FjY2VkZjQzOWQ5ZmZkNmJkMGE4ZGM2Y2NkNDg5MDQyZjU5NGFkZDg0YTgzZGY5In0="
# so, we need to iterate through only dates to get our data:
# number of daily deaths by gender and age group (10 years) 
# since the first death in Brazil
current_day <- Sys.Date()
dates <- seq.Date(dmy("16/03/2020"), current_day, by = "day")

# curl into httr:
# https://curl.trillworks.com/#r
headers <- c(
  `X-XSRF-TOKEN` = token,
  # dont know if it is a private info
  `User-Agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.62 Safari/537.36'
)

list_date <- list()
list_age <- list()
list_gender <- list()
list_death <- list()

df_states <- tibble(uf = c("AM", "MG", "RJ", "RS", "SP"),
                    state = c("Amazonas", "Minas Gerais", "Rio de Janeiro",
                              "Rio Grande do Sul", "São Paulo"))

df <- tibble()

for(i in 1:length(df_states$uf)) {
  for(j in 1:length(dates)) {
    
    # curl into httr
    # https://curl.trillworks.com/#r
    params <- list(
      `chart` = 'chartEspecial4',
      `data_type` = 'data_ocorrido',
      # we retrieving data day by day
      `start_date` = dates[j],
      `end_date` = dates[j],
      `state` = df_states$uf[i],
      `search` = 'death-covid',
      `causa` = 'insuficiencia_respiratoria',
      `groupBy` = 'gender'
    )
    
    response <- httr::GET(url = 'https://transparencia.registrocivil.org.br/api/covid', 
                          httr::add_headers(.headers=headers), 
                          query = params)
    
    raw_data <- httr::content(response)
    age_groups <- names(raw_data["chart"][[1]])
    
    for(k in 1:length(age_groups)) {
      
      genders <- names(raw_data["chart"][[1]][[k]])
      
      if(length(genders) != 0) {
        for(m in 1:length(genders)) {
          list_date <- append(list_date, dates[j])
          list_age <- append(list_age, age_groups[k])
          list_gender <- append(list_gender, genders[m])
          
          # j: age group
          # k: gender F or M
          deaths <- raw_data["chart"][[1]][k][[1]][[m]]
          list_death <- append(list_death, deaths)
        }
      } 
    }
    
    df_temp <- tibble(list_date, list_age, list_gender, list_death) %>% 
      unnest() %>% 
      mutate(state = df_states$state[i],
             uf = df_states$uf[i])
    
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
}

# All combinations dataset ------------------------------------------------

df_combinations <- purrr::cross_df(
  .l = list(
    "uf" = df_states %>% select(uf) %>% unique() %>% pull(),
    "Date" = df %>% select(list_date) %>% unique() %>% pull(),
    "Sex" = df %>% select(list_gender) %>% unique() %>% pull(),
    "Age" = df %>% select(list_age) %>% unique() %>% pull())
)

df_combinations <- df_combinations %>% 
  mutate(Date = lubridate::as_date(Date))

# Output ------------------------------------------------------------------

# daily deaths
df_output <- df_combinations %>% 
  left_join(df %>% select(-state), 
            by = c("uf" = "uf",
                   "Date" = "list_date", 
                   "Age" = "list_age", 
                   "Sex" = "list_gender")) %>% 
  mutate(Value = ifelse(is.na(list_death), 0, list_death)) %>% 
  left_join(df_states, by = c("uf")) %>% 
  select(-list_death)

# treating cases reported with gender 'I'  (ignored)
# create a category 'B' (both genders) that includes
# 'F', 'M', and 'I'
df_output <- df_output %>% 
  filter(Sex != "I") %>% 
  bind_rows(
    df_output %>%
      group_by(uf, state, Date, Age) %>% 
      summarise(Value = sum(Value)) %>% 
      mutate(Sex = "B") %>% 
      ungroup()
  )

# daily deaths accumulated
df_output_accumulated <-  df_output %>%
  arrange(uf, Sex, Age, Date) %>% 
  group_by(uf, state, Sex, Age) %>% 
  mutate(Value_accumulated = cumsum(Value))

# Fixing columns ----------------------------------------------------------

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
  mutate(Sex = case_when(
    Sex == "F" ~ "f",
    Sex == "M" ~ "m",
    Sex == "B" ~ "b")) %>% 
  mutate(Country = "Brazil",
         AgeInt = ifelse(Age == "100", 5, 10),
         Metric = "Count",
         Measure = "Deaths",
         Code = paste0("BR_", uf, Date))

df_output_accumulated <- df_output_accumulated %>% 
  select(Country, Region = state, Code, Date,
         Sex, Age, AgeInt, Metric, Measure,
         Value = Value_accumulated)

# Writing -----------------------------------------------------------------
# Amazonas ----------------------------------------------------------------

df_am <- df_output_accumulated %>% filter(Region == "Amazonas")
df_am <- df_am %>% 
  semi_join(
    df_am %>% 
      group_by(Date) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Value > 0),
    by = c("Date"))

# daily deaths accumulated
# writing in google sheets
sheet_write(df_am, 
            ss = "https://docs.google.com/spreadsheets/d/1p4IkI713sHHW6eU_v18bI6YHcVdfRHYkJKIoFIU69wM/edit?ts=5ecbc7ba#gid=0", 
            sheet = "database")

# Minas Gerais ----------------------------------------------------------------

df_mg <- df_output_accumulated %>% filter(Region == "Minas Gerais")
df_mg <- df_mg %>% 
  semi_join(
    df_mg %>% 
      group_by(Date) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Value > 0),
    by = c("Date"))

# daily deaths accumulated
# writing in google sheets
sheet_write(df_mg, 
            ss = "https://docs.google.com/spreadsheets/d/1wTCB5Z4OTqSJ2mmnfzWBfIsR-VvcM8yhfnB3BIIaCK8/edit?ts=5ecbc7cf", 
            sheet = "database")

# Rio de Janeiro ----------------------------------------------------------------

df_rj <- df_output_accumulated %>% filter(Region == "Rio de Janeiro")
df_rj <- df_rj %>% 
  semi_join(
    df_rj %>% 
      group_by(Date) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Value > 0),
    by = c("Date"))

# daily deaths accumulated
# writing in google sheets
sheet_write(df_rj, 
            ss = "https://docs.google.com/spreadsheets/d/1jzaO8p3h8m6H-APYQxU_Ggwk4XjzkH8sRtYN0i-l3iE/edit?ts=5ecbc7e6#gid=1079196673", 
            sheet = "database")

# Rio Grande do Sul ----------------------------------------------------------------

df_rs <- df_output_accumulated %>% filter(Region == "Rio Grande do Sul")
df_rs <- df_rs %>% 
  semi_join(
    df_rs %>% 
      group_by(Date) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Value > 0),
    by = c("Date"))

# daily deaths accumulated
# writing in google sheets
sheet_write(df_rs, 
            ss = "https://docs.google.com/spreadsheets/d/1gnt97pyA5CrcuLPeMWZKFyH6UHDcgP9f2is1x6uAX-A/edit?usp=sharing_eil&ts=5ecbc7f8&urp=gmail_link", 
            sheet = "database")

# São Paulo ----------------------------------------------------------------

df_sp <- df_output_accumulated %>% filter(Region == "São Paulo")
df_sp <- df_sp %>% 
  semi_join(
    df_sp %>% 
      group_by(Date) %>% 
      summarise(Value = sum(Value)) %>% 
      filter(Value > 0),
    by = c("Date"))

# daily deaths accumulated
# writing in google sheets
sheet_write(df_sp, 
            ss = "https://docs.google.com/spreadsheets/d/1xfV80Pk52ThcDrW7tzj4ku50ahs-nnG-LVDZkQLUZw4/edit?ts=5ecbc808#gid=0", 
            sheet = "database")
