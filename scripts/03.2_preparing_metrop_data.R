
# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(readxl)

# Reading -----------------------------------------------------------------

day <- "23_05_2020"

df_cities <- fread(paste0("./data/treated/all_deaths_br_", 
                          day,
                          "_by_month_city_since_2019_ibge_code.csv"))

df_rm_capitals <- read_excel("./data/raw/Óbitos mensais - capitais de RM.xlsx",
                             sheet = "cidades")
df_rm_capitals$responsável <- NULL

df_rm <- read_excel("./data/raw/RMS.xlsx", 
                    sheet = "municipios_RM")

df_cod_ibge <- read_excel("./data/raw/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

# Cleaning and Joining ----------------------------------------------------

# First step
df_cities <- df_cities %>% 
  as_tibble()

df_rm_capitals[which(df_rm_capitals$UF == "DF"), "cod_ibge"] <- 5300108
df_rm_capitals[which(df_rm_capitals$UF == "DF"), "cidade"] <- "Brasília"

df_rm <- df_rm %>% 
  select(-Qt) %>% 
  gather(key = "rm", value = "cidade") %>% 
  filter(!is.na(cidade))

df_rm[which(df_rm$cidade == "Distrito Federal"), "cidade"] <- "Brasília"

df_cod_ibge <- df_cod_ibge %>% 
  select(UF, `Código Município Completo`, Nome_Município) %>% 
  setNames(c("uf", "cod_ibge", "municipio"))

# Auxiliar
states <- tibble(state = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                           "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
                           "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                 state_code = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21,
                                51, 50, 31, 15, 25, 41, 26, 22, 33, 24, 43,
                                11, 14, 42, 35, 28, 17))

df_rm_state <- tibble(rm = c(df_rm %>% pull(rm) %>% unique()),
                      state = c("SP", "RJ", "MG", "DF", "RS",
                                "PE", "CE", "BA", "PR", "SP",
                                "AM", "SP", "GO", "PA", "SP",
                                "ES", "SP", "SP", "RN", "MA",
                                "SP", "SC", "AL", "PB", "PI",
                                "SC", "PR")) %>% 
  left_join(states, by = c("state"))

# Second: more cleaning and joining
df_rm <- df_rm %>% 
  mutate(cidade = str_remove_all(cidade, pattern = "\\(PI\\)")) %>% 
  mutate(cidade = str_remove_all(cidade, pattern = "\\(MA\\)")) %>% 
  mutate(cidade = str_remove_all(cidade, pattern = "\\*")) %>% 
  mutate(cidade = str_remove_all(cidade, pattern = "\\[7\\]")) %>% 
  mutate(cidade = str_remove_all(cidade, pattern = "\\[8\\]")) %>% 
  mutate(cidade = str_trim(cidade)) %>% 
  left_join(df_rm_state, by = c("rm"))

# Timon é uma cidade do Maranhão que pertence a uma RM 
# com municípios majoritariamente do Piauí
df_rm[which(df_rm$cidade == "Timon"), "state"] <- "MA"
df_rm[which(df_rm$cidade == "Timon"), "state_code"] <- 21

df_rm <- df_rm %>% 
  mutate(state = ifelse(rm == "RM DF e entorno" & cidade != "Brasília", "GO", state),
         state_code = ifelse(rm == "RM DF e entorno" & cidade != "Brasília", 52, state_code))

df_rm[which(df_rm$cidade == "Arinos"), c("state", "state_code")] <- list("MG", 31)
df_rm[which(df_rm$cidade == "Buritis"), c("state", "state_code")] <- list("MG", 31)
df_rm[which(df_rm$cidade == "Cabeceira Grande"), c("state", "state_code")] <- list("MG", 31)
df_rm[which(df_rm$cidade == "Unaí"), c("state", "state_code")] <- list("MG", 31)

# identificando capitais das rm
df_rm_joined <- df_rm %>% 
  left_join(df_rm_capitals %>% select(cidade, cod_ibge), 
            by = c("cidade")) %>% 
  mutate(capital = ifelse(!is.na(cod_ibge), 1, 0))

# Third: more cleaning, in the name of the cities
# continuando o join, por nome
df_rm_joined[which(df_rm_joined$cidade == "B. Jesus do Amparo"), "cidade"] <- "Bom Jesus do Amparo"
df_rm_joined[which(df_rm_joined$cidade == "São G. do Rio Abaixo"), "cidade"] <- "São Gonçalo do Rio Abaixo"
df_rm_joined[which(df_rm_joined$cidade == "Santa Barbára d'Oeste"), "cidade"] <- "Santa Bárbara d'Oeste"
df_rm_joined[which(df_rm_joined$cidade == "Cruz do Espirito Santo"), "cidade"] <- "Cruz do Espírito Santo"
df_rm_joined[which(df_rm_joined$cidade == "São João d’Aliança"), "cidade"] <- "São João d'Aliança"

df_rm_joined_2 <- df_rm_joined %>% 
  filter(!capital) %>% 
  select(-cod_ibge) %>% 
  left_join(df_cod_ibge %>% mutate(uf = as.numeric(uf)), 
            by = c("state_code" = "uf", "cidade" = "municipio")) 

# combining the two data frames
df_rm_correct <- df_rm_joined %>% 
  filter(capital == 1) %>% 
  mutate(cod_ibge = as.character(cod_ibge)) %>% 
  bind_rows(df_rm_joined_2)

df_rm_obitos <- df_rm_correct %>% 
  left_join(df_cities %>% 
              select(municipio_code, year, month, number_deaths) %>% 
              mutate(municipio_code = as.character(municipio_code)),
            by = c("cod_ibge" = "municipio_code"))

# final cleaning
df_rm_obitos <- df_rm_obitos %>% 
  mutate(rm = str_remove(rm, pattern = "^RM "))

df_rm_obitos[which(df_rm_obitos$rm == "BH"), "rm"] <- "Belo Horizonte"
df_rm_obitos[which(df_rm_obitos$rm == "DF e entorno"), "rm"] <- "Distrito Federal e entorno"
df_rm_obitos[which(df_rm_obitos$rm == "POA"), "rm"] <- "Porto Alegre"
df_rm_obitos[which(df_rm_obitos$rm == "RJ"), "rm"] <- "Rio de Janeiro"
df_rm_obitos[which(df_rm_obitos$rm == "SP"), "rm"] <- "São Paulo"
df_rm_obitos[which(df_rm_obitos$rm == "Vale"), "rm"] <- "Vale do Paraíba e Litoral Norte"

# Output ------------------------------------------------------------------

write.csv(df_rm_obitos, file = paste0("./data/treated/all_deaths_br_", 
                                      day, 
                                      "_by_month_metropolitan_region_since_2019_ibge_code.csv") , 
          row.names = FALSE)
