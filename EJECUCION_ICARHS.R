# ïndice de calidad de agua -----------------------------------------------
# Creando las funciones: --------------------------------------------------

 #  __  __  ______   _____   ____   __  __  _____ 
 # |  \/  ||  ____| / ____| / __ \ |  \/  ||_   _|
 # | \  / || |__   | (___  | |  | || \  / |  | |  
 # | |\/| ||  __|   \___ \ | |  | || |\/| |  | |  
 # | |  | || |____  ____) || |__| || |  | | _| |_ 
 # |_|  |_||______||_____/  \____/ |_|  |_||_____|
 #                                                

# RUshu2 (USHUSUMA)
# RCapl3
# RCapl1
# RPist1
# RSama5
# RSama2
# RSama3
# RSala2 (SAMA)
# QVila3
# QVila1
# RUchu1


# QKovi1 ERROR EN LA PAGINA (selecciona parámetros de categoría 3 y compara con eca categoría 4)
# "RTica2" ERROR EN LA PAGINA (selecciona parámetros de categoría 3 y compara con eca categoría 4)
# QCari1 NO SE TIENE información de este punto en el periodo 2 de 2019 
# RMaur3 SE APRECIA UN ERROR SERIO EN EL NIVEL DE SENSIBILIDAD DE LOS ANÁLISIS DE LA VARIABLE PLOMO
# RMaur4 SE APRECIA UN ERROR SERIO EN EL NIVEL DE SENSIBILIDAD DE LOS ANÁLISIS DE LA VARIABLE PLOMO


# Para ver una variable:
# tabbb %>% filter(estacion == "RMaur3" & Variable == "Plomo"& Date > "2016-01-01"& Date<"2019-12-31") %>% View()

library(tidyverse)
library(wqindex)
source("input/Scripts/ICARS_DEF.R")
source("input/Scripts/data_agua_final.R")
tabbb <- data_agua2(ruta = "minidata", 
                    tabla_agua = "input/Hojas_datos/cuerpos de agua.xlsx",
                    eca_icarhs = "input/Hojas_datos/tabla_icarhs.xlsx")

tabla_icarhs <- tabla_agua2(tabla = tabbb, cod = "RTala1", watershed = "SAMA",
            tipo = "tabla", subindice = "s2",fecha_inicio = "2016-01-01", fecha_fin = "2019-12-31")


