data_agua2 <- function(ruta = "data", tabla_agua = "cuerpos de agua.xlsx", eca_icarhs){
  agua <- function(origen_df){
    a <- readxl::read_xls(origen_df, skip = 19) %>%
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>% 
      select(where(~ any(!is.na(.)))) %>% select(-starts_with("Cat")) %>% mutate(periodo = origen_df) %>% 
      pivot_longer(cols = -c(PARAMETROS, UNIDAD, periodo), names_to = "estacion", values_to = "valor") %>% 
      mutate(cuenca = str_extract(periodo, "(?<=/)(.*?)(?=_)"),
             año = str_extract(periodo, "(?<=_)(.*?)(?=_)"),
             periodo = str_extract(periodo, "(?<=_)([^_\\.]+)(?=\\.)")) %>%
      filter(valor != "----") %>%
      select(estacion, cuenca, año, periodo, PARAMETROS, UNIDAD, valor)
    
    b <- readxl::read_xls(origen_df, skip = 15, col_names = TRUE) %>% 
      filter(`Fecha monitoreo` %in% c("Hora Monitoreo", "PARAMETROS")) %>% 
      select(where(~ any(!is.na(.)))) %>% select(-starts_with(c( "DD", "..."))) %>% 
      pivot_longer(cols = -`Fecha monitoreo`, names_to = "fecha", values_to = "hora") %>% 
      pivot_wider(id_cols = fecha, names_from = `Fecha monitoreo`, values_from = hora) %>% 
      mutate(fecha = str_replace(fecha, "\\.\\.\\.\\d+$", ""),
             fecha_larga = as.POSIXct(paste(fecha, `Hora Monitoreo`), format = "%d/%m/%Y %H:%M")) %>%
      select(fecha_larga, PARAMETROS) %>% rename("estacion" = "PARAMETROS")
    
    a %>% left_join(b, by = "estacion")
    
  }
  
  archivos_xls <- list.files(path = ruta, pattern = "\\.xls$", full.names = TRUE)
  listita <- map(archivos_xls, agua)
  df_combinado <- bind_rows(listita)%>% 
    mutate(
      valor = if_else(valor != "Ausencia", valor %>%
                        str_replace_all("[^0-9,.-]", "") %>%
                        str_replace(",", ".") %>% as.numeric(), 0)) %>% 
    mutate(estacion = if_else(
      grepl("^RSala", estacion),
      paste0(estacion, " (",cuenca, ")"),
      if_else(
        grepl("^RUshu", estacion),
        paste0(estacion, " (",cuenca, ")"),
        estacion)))
  
  eca <- openxlsx::read.xlsx(tabla_agua) %>% 
    mutate(cuenca = case_when(
      UNIDAD.HIDROGRÁFICA == "Intercuenca 13155" ~ "INTERCUENCA13155",
      UNIDAD.HIDROGRÁFICA == "Cuenca Sama" ~ "SAMA",
      UNIDAD.HIDROGRÁFICA == "Cuenca Locumba" ~ "LOCUMBA",
      UNIDAD.HIDROGRÁFICA == "Cuenca Caplina" ~ "CAPLINA",
      UNIDAD.HIDROGRÁFICA == "Cuenca Ushusuma" ~ "USHUSUMA",
      UNIDAD.HIDROGRÁFICA == "Cuenca Mauri" ~ "MAURI"
    )) %>% rename("estacion" =  "CÓDIGO.FINAL", "CAT_ECA" = "CLASIFICACÍON.DE.CUERPOS.DE.AGUA") %>%
    select(cuenca, estacion, ESTE, NORTE, ALTITUD.m.s.n.m., DESCRIPCION , CAT_ECA) %>% 
    mutate(estacion = if_else(
      grepl("^RSala", estacion),
      paste0(estacion, " (",cuenca, ")"),
      if_else(
        grepl("^RUshu", estacion),
        paste0(estacion, " (",cuenca, ")"),
        estacion)),
      categoría = case_when(
        CAT_ECA == "Categoría 1 A2" ~ "C1A2",
        CAT_ECA == "Categoría 3" ~ "C3D1",
        CAT_ECA == "Categoría 4" ~ "C4E2"))
  
  icarhs <- openxlsx::read.xlsx(eca_icarhs)
  
  left_join(df_combinado, eca, by = c("estacion", "cuenca")) %>% 
    left_join(icarhs, by = c("categoría", "PARAMETROS" = "parametro")) %>% filter(!is.na(TIPO)) %>% 
    select(estacion, cuenca, fecha_larga, PARAMETROS, UNIDAD, valor, lower, upper, ESTE, NORTE, categoría, TIPO) %>% 
    mutate(Date = as.Date(fecha_larga)) %>% 
    rename("Variable" = "PARAMETROS", "Value" = "valor",
           "LowerLimit" = "lower", "UpperLimit" = "upper", "Units" = "UNIDAD") %>% 
    mutate(DetectionLimit = NA_real_)
}
