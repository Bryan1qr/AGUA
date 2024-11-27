tabla_agua2 <- function(tabla, cod, watershed, tipo, subindice, fecha_inicio, fecha_fin){
  tabla <- tabla %>% filter(fecha_larga >= fecha_inicio & fecha_larga <= fecha_fin)
  if (tipo == "revision") {
    return(tabla %>% 
             filter(estacion == cod) %>%
             select(estacion, cuenca, categoría) %>%
             distinct())
  } else if (tipo == "tabla") {
    if (subindice == "s1") {
      a <- tabla %>% 
        filter(estacion == cod & cuenca == watershed, TIPO == "MO") %>% 
        select(Date, Variable, Value, DetectionLimit,
               LowerLimit, UpperLimit, Units)
      
      return(wqindex::calc_wqi(a))  # Retorna el resultado de calc_wqi
    } else if (subindice == "s2") {
      a <- tabla %>% 
        filter(estacion == cod & cuenca == watershed, TIPO == "FQM") %>% 
        select(Date, Variable, Value, DetectionLimit,
               LowerLimit, UpperLimit, Units)
      
      return(wqindex::calc_wqi(a))  # Retorna el resultado de calc_wqi
    }
  }
  
  # Si no se cumple ninguna condición, retorna NULL o algún valor por defecto
  return(NULL)
}
