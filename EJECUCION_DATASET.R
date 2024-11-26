# Creación del dataset de AGUA --------------------------------------------
## EN ESTE APARTADO SE MUESTRA COMO SE CREÓ EL DATASET DE AGUA (BASE DE DATOS PRINCIPAL)

library(tidyverse)

source("Scripts/DATASET_V2.R")
# La ejecución puede tardar un poco:
df1 <- dataset_agua2(ruta = "data", tabla_agua = "Hojas_datos/cuerpos de agua.xlsx")


head(df1)
