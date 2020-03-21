library(haven)
library(microbenchmark)
library(DataCombine)
library(dplyr)
library(lubridate)
library(tidyr)
library(cluster) # Paquete que nos proporciona métodos para el análisis de clusters
library(factoextra)
library(igraph)
library(fpc)
library(berryFunctions)
library(R.utils)
library(lubridate)
library(haven)

# ------------------------------------------------------------------

# Leemos nuestros datos
data <- read_sas("datos/tabla_1.sas7bdat")
View(data)

# ------------------------------------------------------------------
# ------------------------------------------------------------------

# PARA PROBAR LAS COSAS HACEMOS UNA MUESTRA
# muestra <- head(data, 5000)

# Como sabemos que solo nos falta el 29 de Marzo, es necesario añadir
# una fila para cada CUPS y para dicha fecha, por eso vamos a obtener 
# los índices de todos los 28 de Marzo, ya que queremos añadir una fila
# después de todas las filas que contienen el 28 de Marzo

# Obtenemos el índice anterior a la fila que queremos poner 
indices_28Marzo_data <- which(data$fecha=="2015-03-28")
indices_28Marzo_data

# Debemos tener en cuenta que cuando añadimos una fila, los índices
# varían. Por ejemplo, yo quiero añadir una fila en la posición 3, 8 y 9
# por tanto, añado en la posición 3, pero eso hace que las posiciones 
# aumenten en donde la posición 8 sea la 9, y la posición 10 sea la 11

# Haciendo pruebas me he dado cuenta de que la primera posición es
# crucial, por lo que a la primera le doy la posición 29, y me centro en
# las otras, sumando en una iteración la posición que debería de tener 
indices_28Marzo_data <- c(indices_28Marzo_data[1]+1,
                          c(indices_28Marzo_data[2:length(indices_28Marzo_data)]))
indices_28Marzo_data

# Aquí vamos crear el vector de índices final
indices_29Marzo_data <- c()
for (i in 2:length(indices_28Marzo_data)){
  index_fila_nueva <- indices_28Marzo_data[i]+i
  indices_29Marzo_data <- c(indices_29Marzo_data, index_fila_nueva)
}
indices_29Marzo_data <- c(indices_28Marzo_data[1],indices_29Marzo_data)
indices_29Marzo_data

# Ya solo nos queda insertar en nuesto datframe las nuevas filas vacías
# a partir de los índices que hemos creado previamente
data_filas_vacias <- insertRows(data, indices_29Marzo_data, 
                                rcurrent=FALSE) # after current line, not at it

# Comprobamos el resultado
View(data_filas_vacias)

# CON ESTO YA TENEMOS NUESTRO DATAFRAME CON TODAS LAS NUEVAS FILAS VACÍAS 

# Antes de nada debemos rellenar las filas con el índice y con la fecha
# a las filas vacías

# Para añadir el ID 
id <- seq(1, 1000, by = 1)
data_filas_vacias[is.na(data_filas_vacias$cups), "cups"] <- id
View(data_filas_vacias)

# Para añadir la fecha
strDates <- c("2015-03-29")
dates <- as.Date(strDates, "%Y-%m-%d")
data_filas_vacias[is.na(data_filas_vacias$fecha), "fecha"] <- dates
View(data_filas_vacias)

# ------------------------------------------------------------------

# Lo siguiente que vamos hacer es crear una nueva columna, en donde
# sepamos el día de la semana para la fecha de dicha fila

# Nuestra tabla empieza en 2015-03-01	y termina en 2016-02-29, en donde 
# el 1 de Marzo de 2015 empieza en Domingo y el 29 de Feberero de 2016 
# que es Lunes

fechas <- data_filas_vacias$fecha
dias_semana <- weekdays(fechas)

# Nos creamos una nueva columna donde metemos dichas fechas
data_final <- cbind(data_filas_vacias, dias_semana)
View(data_final)

# ------------------------------------------------------------------

# AHORA VAMOS A IMPUTAR LOS VALORES

# Para imputar los valores lo que haremos será coger la media de todos 
# los domingos que tenemos para cada CUPS

media_cups_fecha <- data_final %>% 
  group_by(cups, dias_semana) %>% 
  summarise(mean_pct_h1 = mean(pct_h1, na.rm = TRUE),
            mean_pct_h2 = mean(pct_h2, na.rm = TRUE),
            mean_pct_h3 = mean(pct_h3, na.rm = TRUE),
            mean_pct_h4 = mean(pct_h4, na.rm = TRUE),
            mean_pct_h5 = mean(pct_h5, na.rm = TRUE),
            mean_pct_h6 = mean(pct_h6, na.rm = TRUE),
            mean_pct_h7 = mean(pct_h7, na.rm = TRUE),
            mean_pct_h8 = mean(pct_h8, na.rm = TRUE),
            mean_pct_h9 = mean(pct_h9, na.rm = TRUE),
            mean_pct_h10 = mean(pct_h10, na.rm = TRUE),
            mean_pct_h11 = mean(pct_h11, na.rm = TRUE),
            mean_pct_h12 = mean(pct_h12, na.rm = TRUE),
            mean_pct_h13 = mean(pct_h13, na.rm = TRUE),
            mean_pct_h14 = mean(pct_h14, na.rm = TRUE),
            mean_pct_h15 = mean(pct_h15, na.rm = TRUE),
            mean_pct_h16 = mean(pct_h16, na.rm = TRUE),
            mean_pct_h17 = mean(pct_h17, na.rm = TRUE),
            mean_pct_h18 = mean(pct_h18, na.rm = TRUE),
            mean_pct_h19 = mean(pct_h19, na.rm = TRUE),
            mean_pct_h20 = mean(pct_h20, na.rm = TRUE),
            mean_pct_h21 = mean(pct_h21, na.rm = TRUE),
            mean_pct_h22 = mean(pct_h22, na.rm = TRUE),
            mean_pct_h23 = mean(pct_h23, na.rm = TRUE),
            mean_pct_h24 = mean(pct_h24, na.rm = TRUE))
View(media_cups_fecha)

# Vamos a coger todos los valores de la media para los días de la
# semana que son domingo
domingos <- media_cups_fecha[media_cups_fecha$dias_semana == "domingo",]
View(domingos)

# Y ahora insertamos esa lista
data_final[is.na(data_final$pct_h1), "pct_h1"] <- domingos$mean_pct_h1
data_final[is.na(data_final$pct_h2), "pct_h2"] <- domingos$mean_pct_h2
data_final[is.na(data_final$pct_h3), "pct_h3"] <- domingos$mean_pct_h3
data_final[is.na(data_final$pct_h4), "pct_h4"] <- domingos$mean_pct_h4
data_final[is.na(data_final$pct_h5), "pct_h5"] <- domingos$mean_pct_h5
data_final[is.na(data_final$pct_h6), "pct_h6"] <- domingos$mean_pct_h6
data_final[is.na(data_final$pct_h7), "pct_h7"] <- domingos$mean_pct_h7
data_final[is.na(data_final$pct_h8), "pct_h8"] <- domingos$mean_pct_h8
data_final[is.na(data_final$pct_h9), "pct_h9"] <- domingos$mean_pct_h9
data_final[is.na(data_final$pct_h10), "pct_h10"] <- domingos$mean_pct_h10
data_final[is.na(data_final$pct_h11), "pct_h11"] <- domingos$mean_pct_h11
data_final[is.na(data_final$pct_h12), "pct_h12"] <- domingos$mean_pct_h12
data_final[is.na(data_final$pct_h13), "pct_h13"] <- domingos$mean_pct_h13
data_final[is.na(data_final$pct_h14), "pct_h14"] <- domingos$mean_pct_h14
data_final[is.na(data_final$pct_h15), "pct_h15"] <- domingos$mean_pct_h15
data_final[is.na(data_final$pct_h16), "pct_h16"] <- domingos$mean_pct_h16
data_final[is.na(data_final$pct_h17), "pct_h17"] <- domingos$mean_pct_h17
data_final[is.na(data_final$pct_h18), "pct_h18"] <- domingos$mean_pct_h18
data_final[is.na(data_final$pct_h19), "pct_h19"] <- domingos$mean_pct_h19
data_final[is.na(data_final$pct_h20), "pct_h20"] <- domingos$mean_pct_h20
data_final[is.na(data_final$pct_h21), "pct_h21"] <- domingos$mean_pct_h21
data_final[is.na(data_final$pct_h22), "pct_h22"] <- domingos$mean_pct_h22
data_final[is.na(data_final$pct_h23), "pct_h23"] <- domingos$mean_pct_h23
data_final[is.na(data_final$pct_h24), "pct_h24"] <- domingos$mean_pct_h24

View(data_final)

# ------------------------------------------------------------------
# ------------------------------------------------------------------

write.csv(data_final, file = "datos_arreglados.csv")
