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

# Establecemos una semilla para obtener siempre los mismos resultados
set.seed(3)

# ----------------------------------------------------------------------

data <- read_sas("datos/tabla_1.sas7bdat")
View(data)

# ----------------------------------------------------------------------

# Reconvertir los porcentajes proporcionados de manera que representen 
# el porcentaje de consumo de cada hora respecto al día (la suma de los
# porcentajes por registro deberá valer 1)

# Quitamos porcentajes

data$pct_h1 <- data$pct_h1*24*365
data$pct_h2 <- data$pct_h2*24*365
data$pct_h3 <- data$pct_h3*24*365
data$pct_h4 <- data$pct_h4*24*365
data$pct_h5 <- data$pct_h5*24*365
data$pct_h6 <- data$pct_h6*24*365
data$pct_h7 <- data$pct_h7*24*365
data$pct_h8 <- data$pct_h8*24*365
data$pct_h9 <- data$pct_h9*24*365
data$pct_h10 <- data$pct_h10*24*365
data$pct_h11 <- data$pct_h11*24*365
data$pct_h12 <- data$pct_h12*24*365
data$pct_h13 <- data$pct_h13*24*365
data$pct_h14 <- data$pct_h14*24*365
data$pct_h15 <- data$pct_h15*24*365
data$pct_h16 <- data$pct_h16*24*365
data$pct_h17 <- data$pct_h17*24*365
data$pct_h18 <- data$pct_h18*24*365
data$pct_h19 <- data$pct_h19*24*365
data$pct_h20 <- data$pct_h20*24*365
data$pct_h21 <- data$pct_h21*24*365
data$pct_h22 <- data$pct_h22*24*365
data$pct_h23 <- data$pct_h23*24*365
data$pct_h24 <- data$pct_h24*24*365

# Sumamos los registros diarios de cada CUPS

data$pct_htotal <- (  data$pct_h1 + data$pct_h2 + data$pct_h3 + data$pct_h4 +
                      data$pct_h5 + data$pct_h6 + data$pct_h7 + data$pct_h8 +
                      data$pct_h9 + data$pct_h10 + data$pct_h11 + data$pct_h12 +
                      data$pct_h13 + data$pct_h14 + data$pct_h15 + data$pct_h16 +
                      data$pct_h17 + data$pct_h18 + data$pct_h19 + data$pct_h20 +
                      data$pct_h21 + data$pct_h22 + data$pct_h23 + data$pct_h24
                    )

View(data)

# Hallamos los nuevos porcentajes

data$pct_h1 <- ifelse(!data$pct_h1, 0, data$pct_h1 / data$pct_htotal)
data$pct_h2 <- ifelse(!data$pct_h2, 0, data$pct_h2 / data$pct_htotal)
data$pct_h3 <- ifelse(!data$pct_h3, 0, data$pct_h3 / data$pct_htotal)
data$pct_h4 <- ifelse(!data$pct_h4, 0, data$pct_h4 / data$pct_htotal)
data$pct_h5 <- ifelse(!data$pct_h5, 0, data$pct_h5 / data$pct_htotal)
data$pct_h6 <- ifelse(!data$pct_h6, 0, data$pct_h6 / data$pct_htotal)
data$pct_h7 <- ifelse(!data$pct_h7, 0, data$pct_h7 / data$pct_htotal)
data$pct_h8 <- ifelse(!data$pct_h8, 0, data$pct_h8 / data$pct_htotal)
data$pct_h9 <- ifelse(!data$pct_h9, 0, data$pct_h9 / data$pct_htotal)
data$pct_h10 <- ifelse(!data$pct_h10, 0, data$pct_h10 / data$pct_htotal)
data$pct_h11 <- ifelse(!data$pct_h11, 0, data$pct_h11 / data$pct_htotal)
data$pct_h12 <- ifelse(!data$pct_h12, 0, data$pct_h12 / data$pct_htotal)
data$pct_h13 <- ifelse(!data$pct_h13, 0, data$pct_h13 / data$pct_htotal)
data$pct_h14 <- ifelse(!data$pct_h14, 0, data$pct_h14 / data$pct_htotal)
data$pct_h15 <- ifelse(!data$pct_h15, 0, data$pct_h15 / data$pct_htotal)
data$pct_h16 <- ifelse(!data$pct_h16, 0, data$pct_h16 / data$pct_htotal)
data$pct_h17 <- ifelse(!data$pct_h17, 0, data$pct_h17 / data$pct_htotal)
data$pct_h18 <- ifelse(!data$pct_h18, 0, data$pct_h18 / data$pct_htotal)
data$pct_h19 <- ifelse(!data$pct_h19, 0, data$pct_h19 / data$pct_htotal)
data$pct_h20 <- ifelse(!data$pct_h20, 0, data$pct_h20 / data$pct_htotal)
data$pct_h21 <- ifelse(!data$pct_h21, 0, data$pct_h21 / data$pct_htotal)
data$pct_h22 <- ifelse(!data$pct_h22, 0, data$pct_h22 / data$pct_htotal)
data$pct_h23 <- ifelse(!data$pct_h23, 0, data$pct_h23 / data$pct_htotal)
data$pct_h24 <- ifelse(!data$pct_h24, 0, data$pct_h24 / data$pct_htotal)

# Comprobamos que todos los registros suman 1

data$suma_registros <- (  data$pct_h1 + data$pct_h2 + data$pct_h3 + data$pct_h4 +
                          data$pct_h5 + data$pct_h6 + data$pct_h7 + data$pct_h8 +
                          data$pct_h9 + data$pct_h10 + data$pct_h11 + data$pct_h12 +
                          data$pct_h13 + data$pct_h14 + data$pct_h15 + data$pct_h16 +
                          data$pct_h17 + data$pct_h18 + data$pct_h19 + data$pct_h20 +
                          data$pct_h21 + data$pct_h22 + data$pct_h23 + data$pct_h24
                        )

View(data)

# ----------------------------------------------------------------------

# Calcular la media de dichos porcentajes por CUPS para días laborables
# y festivos de manera que se obtengan dos conjuntos con 1.000 CUPS cada 
# una de ellas: work.consumoHorasFestivos y work.consumoHorasLaborables

# Nuestra tabla empieza en 2015-03-01	y termina en 2016-02-29, en donde 
# el 1 de Marzo de 2015 empieza en Domingo y el 29 de Feberero de 2016 
# que es Lunes

# NO TENEMOS EL DÍA 29 DE MARZO

# PARA OBTENER LOS DÍAS LABORALES
# SE ME OCURRE SACAR LOS ÍNDICES QUE NECESITAMOS, Y A PARTIR DE AHÍ
# SACAR LOS ÍNDICES CORRESPONDIENTES A LOS DATAFRAME
seq1 <- seq(from=2, to=dim(data)[1], by=7); seq1
seq2 <- seq(from=3, to=dim(data)[1], by=7); seq2
seq3 <- seq(from=4, to=dim(data)[1], by=7); seq3
seq4 <- seq(from=5, to=dim(data)[1], by=7); seq4
seq5 <- seq(from=6, to=dim(data)[1], by=7); seq5
seq <- sort(c(seq1, seq2, seq3, seq4, seq5)); seq
length(seq)

# OBTENEMOS LAS FILAS A PARTIR DE LOS ÍNDICES
# PARA OBTENER LOS DÍAS LABORALES
data_laborales <- data[c(seq),]
dim(data_laborales)
View(data_laborales)

# PARA OBTENER LOS DÍAS FESTIVOS 
# QUITAMOS LOS ÍNDICES DE LOS LABORALES
data_festivos <- data[-c(seq),]
dim(data_festivos)
View(data_festivos)

# Una vez que tenemos las dos tablas: calcular la media de dichos porcentajes por CUPS

data_media_laborales <- data_laborales %>%
  group_by(cups) %>%
  summarise(mean_h1 = mean(pct_h1), 
            mean_h2 = mean(pct_h2), 
            mean_h3 = mean(pct_h3), 
            mean_h4 = mean(pct_h4), 
            mean_h5 = mean(pct_h5), 
            mean_h6 = mean(pct_h6), 
            mean_h7 = mean(pct_h7), 
            mean_h8 = mean(pct_h8), 
            mean_h9 = mean(pct_h9), 
            mean_h10 = mean(pct_h10), 
            mean_h11 = mean(pct_h11), 
            mean_h12 = mean(pct_h12), 
            mean_h13 = mean(pct_h13), 
            mean_h14 = mean(pct_h14), 
            mean_h15 = mean(pct_h15), 
            mean_h16 = mean(pct_h16), 
            mean_h17 = mean(pct_h17), 
            mean_h18 = mean(pct_h18), 
            mean_h19 = mean(pct_h19), 
            mean_h20 = mean(pct_h20), 
            mean_h21 = mean(pct_h21), 
            mean_h22 = mean(pct_h22),
            mean_h23 = mean(pct_h23), 
            mean_h24 = mean(pct_h24))

View(data_media_laborales)

# Eliminamos la primera columna, ya que es lo mismo que el índice
data_media_laborales <- subset(data_media_laborales, select = -cups)

View(data_media_laborales)

# ----------------------------------------------------------------------

# Realizar una segmentación sobre una y otra tabla PERO NO a partir de
# los 24 valores porcentuales originales, sino de las variables que 
# resulten tras un Análisis de Componentes Principales realizado sobre
# dichas variables porcentuales.

# La función prcomp() es una de las múltiples funciones en R que realizan 
# PCA. Por defecto, prcomp() centra las variables para que tengan media 
# cero, pero si se quiere además que su desviación estándar sea de uno, 
# hay que indicar scale = TRUE.

pca_laborales <- prcomp(data_media_laborales, center = TRUE, scale. = TRUE)
View(pca_laborales)
summary(pca_laborales)
View(pca_laborales$x)
pca_laborales_dataframe <- data.frame(pca_laborales)


# Algoritmo K-means con los datos de test y 3 clusters

# Para ello, utilizaremos la función `kmeans`, que nos permitirá obtener la 
# agrupación que hemos estado comentando anteriormente. Como parámetros, mencionar, 
# que hemos establecido el número de clústers a 2 (tal y como hemos expuesto en el 
# párrafo anterior), y que la distancia que utilizaremos en el algoritmo será la 
# distancia *euclídea*. En la siguiente imagen, podemos ver el resultado alcanzado en 
# este caso. Aunque resulta poco visible debido a la gran cantidad de información que 
# hay en ella, podemos ver cómo prácticamente el espacio está dividido en tres clústeres, 
# medianamente bien predefinidos.
pca_laborales_clustering <- kmeans(pca_laborales$x, 3)
# Lo mostramos con eqtiquetas
fviz_cluster(object = pca_laborales_clustering, data = pca_laborales$x, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize = 9) +
  labs(title = "Resultados clustering K-means (con etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")
# Lo mostramos sin eqtiquetas
fviz_cluster(object = pca_laborales_clustering, data = pca_laborales$x, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize=0) +
  labs(title = "Resultados clustering K-means (sin etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")
# Sin embargo, para validar nuestro modelo, podemos utilizar también algún 
# tipo de medida que nos permita saber cómo de bueno es el agrupamiento o 
# asignación que se ha llevado a cabo con la técnica. Para llevar a cabo 
# este procedimiento, vamos a utilizar una de las técnicas que hemos visto 
# en la asignatura: el método *Average Siluette*. Para ello vamos a utilizar 
# lo que se denomina *índice silueta*, el cuál es un coeficiente que compara la 
# distancia de un item con el resto de items, ya sea del mismo cluster, o del 
# resto de ellos. Se mueve en el rango [-1,1], de forma que, cuánto mayor sea 
# el valor, mayor es su adecuación en el agrupamiento realizado. A su vez, 
# valores negativos llevarán a pensar que se ha llevado a cabo una asignación 
# incorrecta y poco fiable. Este método es muy potente, ya que nos permite evaluar 
# el agrupamiento tanto a nivel de cada uno de los items, como a nivel de clúster 
# concreto, o a clústeres totales en el agrupamiento. 
fviz_nbclust(x = pca_laborales$x, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")
# A su vez, podemos realizar una validación interna del clúster haciendo uso 
# del ya mencionado *índice silueta*. Se puede visualizar el resultado en la 
# siguiente gráfica.
km_clusters <- eclust(x = pca_laborales$x, FUNcluster = "kmeans", k = 2, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 
# ESTARIA BIEN HACER UN MUESTRA CON NUESTROS DATOS Y VER:
# mediante el clustering que nos ha generado train, buscamos clasificar cada item de 
# test y comprobaremos el acierto de dicha predicción. Posteriormente, comprobaremos 
# si ese mismo fármaco, ha sido asignado al mismo grupo. 

#  K-medioides Clustering
# Identificamos el número optimo de clusters
fviz_nbclust(x = pca_laborales$x, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(pca_laborales$x, method = "manhattan"))
fviz_nbclust(x = pca_laborales$x, FUNcluster = pam, method = "silhouette", k.max = 15,
             diss = dist(pca_laborales$x, method = "manhattan") ) +labs(title = "Número óptimo de clusters")

laborales_clusters <- pam(x = pca_laborales$x, k = 2, metric = "manhattan")
fviz_cluster(object = laborales_clusters, data = pca_laborales$x, ellipse.type = "t", repel = TRUE,
             labelsize = 9) +
  theme_bw() +
  labs(title = "Resultados clustering PAM (k=6)") +
  theme(legend.position = "none")

# Clustering Jerárquico

# El *clustering jerárquico* es una alternativa a los métodos de clustering 
# particional, que no requiere que se pre-especifique el número de clusters 
# al algoritmo. Tal y como hemos visto en la asignatura, se pueden seguir dos 
# estrategias, aglomerativa o divisiva, en este caso vamos a usar la primera, la cuál 
# es la más común. 

# Como ya sabemos, en el método aglomerativo el agrupamiento se inicia en la base 
# del árbol, siendo cada observación un cluster individual. Entonces, los cluster 
# se van combinando conforme la estructura crece hasta unirse en única rama central. 
# De forma análoga a como hemos estado llevando a cabo la aplicación de las demás 
# técnicas, en primer lugar, vamos a determinar el número de clusters óptimo. 
# Como podemos ver en la gráfica, el valor óptimo es x, a partir del cuál 
# comienza a descender. Sin embargo, la diferencia con un tamaño x de clusters 
# es mínima, y computacionalmente nos mejora bastante, por lo que, desde un 
# punto de vista práctico, hemos considerado que es el valor más adecuado en 
# nuestra situación. 
hc_euclidea_completo <- hclust(d = dist(x = pca_laborales$x, method = "euclidean"), method = "complete")

# Una de las ventajas que nos da este algoritmo, es que nos permite 
# visualizar, a través del dendrograma, una posible división en clusters, 
# de forma que podemos, "a priori", intentar dar con un valor de cluster 
# adecuado. De esta forma, podemos hacer uso de las facilidades que nos da 
# visualizar el dendrograma para ver qué cifra es la más adecuada para K. 
fviz_dend(x = hc_euclidea_completo, k = 10, cex = 0.6) +
  geom_hline(yintercept = 2.85, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=10")

fviz_cluster(object = list(data=pca_laborales$x, cluster=cutree(hc_euclidea_completo, k=10)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 0)  +
  labs(title = "Hierarchical clustering PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=6") +
  theme_bw() +
  theme(legend.position = "bottom")
