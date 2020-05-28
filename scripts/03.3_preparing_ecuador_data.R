
# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)

# Reading -----------------------------------------------------------------

# https://www.registrocivil.gob.ec/cifras/
df_provincia <- fread("./data/raw/deaths_ecuador_by_month_provincia_since_2018.csv")

# Preparing output --------------------------------------------------------

df_output <- df_provincia %>% 
  as_tibble() %>% 
  gather(key = "mes", value = "number_deaths", -c("provincia", "ano")) %>% 
  mutate(mes = case_when(
    mes == "enero" ~ 1,
    mes == "febrero" ~ 2,
    mes == "marzo" ~ 3,
    mes == "abril" ~ 4
  )) %>% 
  setNames(c("provincia", "year", "month", "number_deaths"))

# Output ------------------------------------------------------------------

write.csv(df_output,
          file = "./data/treated/all_deaths_ecuador_by_month_provincia_since_2018.csv",
          row.names = FALSE)
