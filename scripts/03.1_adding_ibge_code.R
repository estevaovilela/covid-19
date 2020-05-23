

# Library -----------------------------------------------------------------

library(data.table)
library(tidyverse)
library(readxl)
library(fuzzyjoin)

# Functions ---------------------------------------------------------------

# Provided by:
# http://www.thomazrossito.com.br/retirar-acentos-de-um-data-frame-com-a-linguagem-r/
rm_symbols <- function(str, pattern="all") {
  
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all", "al", "a", "todos", "t", "to", "tod", "todo") %in% pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

# Reading -----------------------------------------------------------------

day <- "23_05_2020"

df_cities <- fread(paste0("./data/treated/all_deaths_br_", 
                          day, 
                          "_by_month_city_since_2019.csv"))

df_cities_ibge <- read_excel("./data/raw/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

# metropolitan capitals
df_rm <- read_excel("./data/raw/Óbitos mensais - capitais de RM.xlsx",
                    sheet = "cidades")
df_rm$responsável <- NULL

# Preparing ---------------------------------------------------------------

df_cities <- df_cities %>% 
  as_tibble()

df_cities_ibge <- df_cities_ibge %>% 
  select(-Município) %>% 
  setNames(c("uf_code", "uf", "mesorregiao_code", "mesorregiao",
             "microrregiao_code", "microrregiao", "municipio_code",
             "municipio"))

states <- tibble(state = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                           "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
                           "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                 state_code = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21,
                                51, 50, 31, 15, 25, 41, 26, 22, 33, 24, 43,
                                11, 14, 42, 35, 28, 17))

df_rm[which(df_rm$UF == "DF"), "cod_ibge"] <- 5300108
df_rm[which(df_rm$UF == "DF"), "cidade"] <- "Brasília"

# Adding city code --------------------------------------------------------

# IBGE only consider one city in the Federal District, 'Brasília'. 'Portal
# da Transparência - Registro Civil' considers a lot of small 'sattelite' cities
# as well, like 'Ceilândia', 'Gama', etc. We'll consider in this dataset
# the IBGE methodology, so we have to group all cities.
df_cities <- df_cities %>% 
  filter(!state == "DF") %>% 
  bind_rows(df_cities %>% 
              filter(state == "DF") %>% 
              mutate(city = "Brasília") %>% 
              group_by(city, state, month, year) %>% 
              summarise(number_deaths = sum(number_deaths)) %>% 
              ungroup())

# next, we'll select only columns to join the two tables
df_cities_adm_record <- df_cities %>% 
  select(state, city) %>% 
  distinct() %>% 
  left_join(states, by = c("state")) %>% 
  mutate(state_code = as.character(state_code),
         # removing symbols, like ^, ~
         city_without_symbol = rm_symbols(city)) %>%
  # remove ''' as well
  mutate(city_without_symbol = str_remove(city_without_symbol, pattern = "'")) %>% 
  mutate(city_without_symbol = str_to_lower(city_without_symbol))

df_cities_ibge <- df_cities_ibge %>% 
  mutate(municipio_without_symbol = rm_symbols(municipio)) %>% 
  mutate(municipio_without_symbol = str_remove(municipio_without_symbol, pattern = "'")) %>% 
  mutate(municipio_without_symbol = str_to_lower(municipio_without_symbol))

# 4709/4732 cities get the correct 'join'
df_city_joined <- df_cities_adm_record %>% 
  left_join(df_cities_ibge, by = c("state_code" = "uf_code", 
                                   "city_without_symbol" = "municipio_without_symbol")) %>% 
  filter(!is.na(municipio))

# 23 cities don't get the correct 'join'
df_to_correct <- df_cities_adm_record %>% 
  left_join(df_cities_ibge, by = c("state_code" = "uf_code", 
                                   "city_without_symbol" = "municipio_without_symbol")) %>% 
  filter(is.na(municipio)) %>% 
  select(state:city_without_symbol)

# we'll atempt to use fuzzyjoin package,
# only in the cities not yet 'joined'
df_fuzzy <- df_to_correct %>% 
  fuzzyjoin::stringdist_left_join(
    df_cities_ibge %>% 
      anti_join(df_city_joined, by = c("municipio_without_symbol" = "city_without_symbol")),
    by = c("city_without_symbol" = "municipio_without_symbol"),
    # levenshtein distance
    method = "lv",
    max_dist = 2,
    distance_col = "distance"
  )

# With manual inspection:
# *** need to remove hard code
# 1) Discard lines 6, 16, 18. The 'join' brought a city from another state
df_fuzzy <- df_fuzzy[-c(6, 16, 18), ]

# 2) Not founded: for now, we'll remove this cities
df_fuzzy <- df_fuzzy %>% 
  filter(!city %in% c("Campo Grande",
                      "Couto de Magalhães",
                      "Santarém",
                      "São Valério da Natividade",
                      "Seridó"))
# 6: RN - Campo Grande: according to wikipedia
# (https://pt.wikipedia.org/wiki/Campo_Grande_(Rio_Grande_do_Norte)), 
# the name of the city was changed in 1991 from 'Augusto Severo' to 'Campo Grande', 
# but in IBGE it remains 'Augusto Severo': 
# (https://www.ibge.gov.br/cidades-e-estados/rn/augusto-severo.html)
# 7: TO - Couto de Magalhães: according to IBGE the name is 'Couto Magalhães'
# 23: PB - Santarém: according to wikipedia
# (https://pt.wikipedia.org/wiki/Joca_Claudino)
# the name of the city was changed in 2010 from 'Santarém' to 'Joca Claudino'.
# 24: TO - São Valério da Natividade: according to wikipedia
# (https://pt.wikipedia.org/wiki/S%C3%A3o_Val%C3%A9rio)
# the name of the city was changed in 2017 from 'São Valério da Natividade' to 
# 'São Valério'.
# 27: PB - Seridó: according to wikipedia
# (https://pt.wikipedia.org/wiki/S%C3%A3o_Vicente_do_Serid%C3%B3)
# the name is 'São Vicente do Seridó'

df_city_complete <- df_city_joined %>% 
  bind_rows(df_fuzzy %>% 
              select(-uf_code, -municipio_without_symbol, -distance))

# Output ------------------------------------------------------------------

# cities
df_output <- df_cities %>% 
  inner_join(df_city_complete, by = c("city", "state")) %>% 
  select(state_code.x, state, uf, mesorregiao_code:municipio,
         city_without_symbol, city, year, month, number_deaths)
colnames(df_output)[1] <- "state_code"

write.csv(df_output, file = paste0("./data/treated/all_deaths_br_", 
                                   day, 
                                   "_by_month_city_since_2019_ibge_code.csv") , 
          row.names = FALSE)

# cities - capitals of metropolitan regions
df_rm_output <- df_cities %>% 
  inner_join(df_city_complete, by = c("city", "state")) %>% 
  mutate(municipio_code = as.numeric(municipio_code)) %>% 
  inner_join(df_rm %>% select(RM, cod_ibge), 
             by = c("municipio_code" = "cod_ibge")) %>% 
  select(state_code.x, state, uf, mesorregiao_code:microrregiao,
         RM, municipio_code, municipio,
         city_without_symbol, city, year, month, number_deaths)

write.csv(df_rm_output, file = paste0("./data/treated/all_deaths_br_", 
                                   day, 
                                   "_by_month_metropolitan_capital_since_2019_ibge_code.csv") , 
          row.names = FALSE)
