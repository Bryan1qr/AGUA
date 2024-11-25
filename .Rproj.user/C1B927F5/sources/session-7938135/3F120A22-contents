dataset_agua <- function(ruta = "data", tabla_agua = "cuerpos de agua.xlsx"){
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
  df_combinado <- bind_rows(listita)
  dfx <- df_combinado %>% mutate(valor_mod = str_remove(valor, "^(>+|<+|<=|>=)")) %>% 
    mutate(
      valor_mod = as.numeric(str_replace(valor_mod, ",", ".")))
  
  dfx <- dfx %>%
    mutate(UNIDAD = if_else(is.na(UNIDAD), "mg/L", UNIDAD)) %>% 
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
        estacion)))
  
  dfx <- dfx %>% left_join(eca, by = "estacion") %>% 
    mutate(cuenca = cuenca.x) %>% select(-c(cuenca.x, cuenca.y)) %>% 
    mutate(
      tipo = case_when(
        grepl("^E", estacion) ~ "lentico", 
        grepl("^L", estacion) ~ "lentico",
        TRUE ~ "lotico"
      ), Scateg = case_when(
        tipo == 'lentico' & CAT_ECA == "Categoría 4" ~ "C4E1",
        tipo == 'lotico' & CAT_ECA == "Categoría 4" ~ "C4E2",
        CAT_ECA == "Categoría 1 A2" ~ "C1A2",
        CAT_ECA == "Categoría 3" ~ "C3D1"),
      scateg2 = if_else(CAT_ECA == "Categoría 3", "C3D2", NA))
  
  tab_eca <- function(ruta = "valores_eca.xlsx"){
    
    c1a1 <- openxlsx::read.xlsx(ruta, sheet = 1, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c1a2 <- openxlsx::read.xlsx(ruta, sheet = 2, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c3d1 <- openxlsx::read.xlsx(ruta, sheet = 3, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c3d2 <- openxlsx::read.xlsx(ruta, sheet = 4, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c4e1 <- openxlsx::read.xlsx(ruta, sheet = 5, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c4e2 <- openxlsx::read.xlsx(ruta, sheet = 6, na.strings = c("-","--","---","----")) %>% 
      filter(!PARAMETROS %in% c("FISICOS - QUIMICOS", "INORGANICOS", "MICROBIOLOGICO Y PARASITOLOGICOS", NA, "Caudal")) %>%
      select(where(~ any(!is.na(.))))
    
    c1a1 %>%
      full_join(c1a2, by = c("PARAMETROS", "UNIDAD")) %>%
      full_join(c3d1, by = c("PARAMETROS", "UNIDAD")) %>%
      full_join(c3d2, by = c("PARAMETROS", "UNIDAD")) %>%
      full_join(c4e1, by = c("PARAMETROS", "UNIDAD")) %>% 
      full_join(c4e2, by = c("PARAMETROS", "UNIDAD")) %>% 
      pivot_longer(cols = -c(PARAMETROS, UNIDAD), names_to = "Scateg", values_to = "val_eca") %>% 
      filter(!is.na(val_eca))
  }
  
  ecca <- tab_eca(ruta = "valores_eca.xlsx")
  
  dfx1 <- dfx %>% left_join(ecca %>% select(-UNIDAD), by = c("PARAMETROS", "Scateg")) %>% 
    left_join(ecca %>% select(-UNIDAD), by = c("PARAMETROS", "scateg2" = "Scateg")) %>% 
    rename("val_eca1" = "val_eca.x", "val_eca2" = "val_eca.y")
  
  dfx1 %>% mutate(
    val_ecca1_num = str_remove_all(val_eca1, "^(>+|<+|<=|>=)") %>%
      str_remove_all("[=\\-\\s]"),
    val_ecca2_num = str_remove_all(val_eca2, "^(>+|<+|<=|>=)") %>%
      str_remove_all("[=\\-\\s]")
  ) %>% 
    mutate(
      val_ecca1_num = as.numeric(str_replace(val_ecca1_num, ",", ".")),
      val_ecca2_num = as.numeric(str_replace(val_ecca2_num, ",", "."))) %>% 
    mutate(fecha_larga = format(fecha_larga, format = "%Y-%m-%d %H:%M:%S")) %>% 
    mutate(Scateg_descrip = case_when(
      Scateg == "C1A2" ~ "Categoría 1 subcategoría A2",
      Scateg == "C3D1" ~ "Categoría 3 subcategoría D1",
      Scateg == "C4E1" ~ "Categoría 4 subcategoría E1",
      Scateg == "C4E2" ~ "Categoría 4 subcategoría E2"),
      Scateg_descrip2 = case_when(
        scateg2 == "C3D2" ~ "Categoría 3 subcategoría D2")) %>% 
    select(cuenca, estacion, tipo, Scateg,Scateg_descrip, val_eca1, scateg2,Scateg_descrip2,
           val_eca2, año, periodo, ESTE, NORTE, ALTITUD.m.s.n.m.,
           DESCRIPCION, CAT_ECA, PARAMETROS, UNIDAD, fecha_larga, valor, valor_mod,val_ecca1_num, val_ecca2_num)
}
