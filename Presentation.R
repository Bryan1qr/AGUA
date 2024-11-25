# ïndice de calidad de agua -----------------------------------------------
# Creando las funciones: --------------------------------------------------

 #  __  __  ______   _____   ____   __  __  _____ 
 # |  \/  ||  ____| / ____| / __ \ |  \/  ||_   _|
 # | \  / || |__   | (___  | |  | || \  / |  | |  
 # | |\/| ||  __|   \___ \ | |  | || |\/| |  | |  
 # | |  | || |____  ____) || |__| || |  | | _| |_ 
 # |_|  |_||______||_____/  \____/ |_|  |_||_____|
 #                                                


source("ICARS_DEF.R")
library(tidyverse)

tab1 <- tabla_agua2(ruta = "minidata",
                    tabla_agua = "cuerpos de agua.xlsx",
                    eca_icarhs = "tabla_icarhs.xlsx")


test <- tab1 %>% mutate(Date = as.Date(fecha_larga)) %>% 
  rename("Variable" = "PARAMETROS", "Value" = "valor",
         "LowerLimit" = "lower", "UpperLimit" = "upper", "Units" = "UNIDAD") %>% 
  mutate(DetectionLimit = NA_real_) %>% 
  filter(estacion == "Rcapl1" & cuenca == "CAPLINA") %>% 
  select(Date, Variable, Value, DetectionLimit, LowerLimit, UpperLimit, Units)


calc_wqi(test)  
  
                        