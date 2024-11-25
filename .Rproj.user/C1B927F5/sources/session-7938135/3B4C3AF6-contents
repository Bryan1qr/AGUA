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
  } else if (cat == "C3D1" & subindice == "1"){
    tab <- tab %>% filter(Variable %in% c("Amoniaco-N","Demanda Bioquímica de Oxígeno (DBO5)",
                                      "Demanda Química de Oxígeno (DQO)", "Fósforo Total",
                                      "Oxígeno Disuelto",
                                      "Coliformes Termotolerantes"))
  } else if (cat == "C4E2" & subindice == "1"){
    tab <- tab %>% filter(Variable %in% c("Amoniaco-N","Demanda Bioquímica de Oxígeno (DBO5)",
                                      "Demanda Química de Oxígeno (DQO)", "Fósforo Total",
                                      "Oxígeno Disuelto",
                                      "Coliformes Termotolerantes"))
  } else if (cat == "C4E1" & subindice == "1"){
    tab <- tab %>% filter(Variable %in% c("Amoniaco-N","Demanda Bioquímica de Oxígeno (DBO5)",
                                          "Demanda Química de Oxígeno (DQO)", "Fósforo Total",
                                          "Oxígeno Disuelto",
                                          "Coliformes Termotolerantes"))
    
  } else if (cat == "C1A2" & subindice == "2"){
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo"))
  } else if (cat == "C3D1" & subindice == "2"){
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo"))
  } else if (cat == "C4E1" & subindice == "2"){
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo"))
  } else if (cat == "C4E2" & subindice == "2"){
    tab <- tab %>% filter(Variable %in% c("pH","Aluminio", "Arsénico", "Boro", "Cadmio",
                                          "Hierro", "Manganeso", "Plomo"))
  } else {
    print("establece que categoría de ECA corresponde!!! c1, c3 o c4")
  }
  
  wqindex::calc_wqi(tab, messages = FALSE)
  
}

inspect <- function(tabla, cod_estacion){
  tabla %>% filter(estacion == cod_estacion) %>% select(Scateg, cuenca, estacion) %>% distinct()
}


inspect(tabla = df1, cod_estacion = "QCari1")

wqi_data(date1 = "2016-01-01",
         date2 = "2019-12-31",
         cuenca = "USHUSUMA", punto = "QCari1",
         data = df1,
         cat = "C4E2",
         subindice = "2")

