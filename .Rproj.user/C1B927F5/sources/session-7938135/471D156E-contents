wqi_data <- function(date1, date2, cuenca, punto, data, cat, subindice){
  r1 <- data %>%
    filter(fecha_larga >= date1 & fecha_larga <= date2 & cuenca == cuenca & tipo == "lotico" & estacion == punto) %>%
    select(fecha_larga, PARAMETROS, valor,Scateg, UNIDAD, val_eca1) %>% mutate(DetectionLimit = NA) %>%
    filter(PARAMETROS %in% c("Demanda Bioquímica de Oxígeno (DBO5)",
                             "Demanda Química de Oxígeno (DQO)", "Oxígeno Disuelto",
                             "Coliformes Termotolerantes", "Fósforo Total", "Amoniaco-N",
                             "Nitratos (NO3-)",
                             "pH", "Arsénico", "Aluminio", "Manganeso", "Hierro", "Cadmio", "Plomo",
                             "Boro", "Cobre", "Mercurio", "Zinc", "Sólidos Suspendidos Totales")) %>%
    filter(!is.na(val_eca1))
  
  tab <- r1 %>% separate(val_eca1, into = c("LowerLimit", "UpperLimit"), sep = " – ") %>% 
    mutate(
      # Si el parámetro es Oxígeno Disuelto o pH, mantenemos el UpperLimit original
      UpperLimit = if_else(
        PARAMETROS %in% c("pH", "Oxígeno Disuelto"),
        UpperLimit, # Mantener el valor de UpperLimit tal cual está
        if_else(!PARAMETROS %in% c("pH", "Oxígeno Disuelto"),LowerLimit, NA)),
      LowerLimit = if_else(PARAMETROS %in% c("pH", "Oxígeno Disuelto"),LowerLimit,NA)) %>% 
    select(fecha_larga, PARAMETROS, valor, DetectionLimit, LowerLimit, UpperLimit, UNIDAD) %>%
    rename("Date" = "fecha_larga", "Variable" = "PARAMETROS", "Value" = "valor", "units" = "UNIDAD") %>%
    mutate(Value = str_remove_all(Value, "^(>+|<+|<=|>=)") %>%
             str_remove_all("[=\\-\\s]") %>% str_replace(",", ".") %>% as.numeric(),
           UpperLimit = str_remove_all(UpperLimit, "^(>+|<+|<=|>=|=+)")%>%
             str_remove_all("[=\\-\\s]") %>% str_replace( ",", ".") %>% as.numeric(),
           LowerLimit = str_remove_all(LowerLimit, "^(>+|<+|<=|>=|=+)")%>%
             str_remove_all("[=\\-\\s]") %>% str_replace( ",", ".") %>% as.numeric(),
           DetectionLimit = as.numeric(DetectionLimit),
           Date = as.Date(Date))
  
  if (cat == "C1A2" & subindice == "1") {
    tab <- tab %>% filter(Variable %in% c("Amoniaco-N","Demanda Bioquímica de Oxígeno (DBO5)",
                                          "Demanda Química de Oxígeno (DQO)", "Fósforo Total",
                                          "Oxígeno Disuelto",
                                          "Coliformes Termotolerantes"))
  } else if (cat == "C1A2" & subindice == "2") {
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo"))
  } else if (cat == "C3D1" & subindice == "2") {
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo", "Cobre"))
  } else if (cat == "C3D1" & subindice == "1") {
    tab <- tab %>% filter(Variable %in% c("Demanda Bioquímica de Oxígeno (DBO5)",
                                          "Demanda Química de Oxígeno (DQO)",
                                          "Oxígeno Disuelto",
                                          "Coliformes Termotolerantes"))
  } else if (cat == "C4E2" & subindice == "2") {
    tab <- tab %>% filter(Variable %in% c("pH", "Arsénico", "Plomo", "Cobre", "Mercurio", 
                                          "Zinc", "Sólidos Suspendidos Totales"))
  } else if (cat == "C4E2" & subindice == "1") {
    tab <- tab %>% filter(Variable %in% c("Demanda Bioquímica de Oxígeno (DBO5)",
                                          "Oxígeno Disuelto",
                                          "Coliformes Termotolerantes",
                                          "Fósforo Total"))
    }
  
}

inspect(df1, cod_estacion = "QKovi1")

a <- wqi_data(date1 = "2016-01-01", date2 = "2019-12-31",
         cuenca = "SAMA",punto =  "QKovi1", data = df1, cat = "C4E2", subindice = "1")



calc_wqi(a)




library(tidyverse)

source("script_de_agua.R")
# La ejecución puede tardar un poco:
df1 <- dataset_agua(ruta = "data", tabla_agua = "cuerpos de agua.xlsx")

wqi_data(date1 = "2016-01-01",date2 = "2019-12-31",
         cuenca = "LOCUMBA", punto =  "RLocu3", data = df1) %>% 
  mutate(Date = as.Date(Date)) %>% View()

openxlsx::write.xlsx(df1, "data_agua_v6.xlsx")
