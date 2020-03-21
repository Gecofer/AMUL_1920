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
library(plyr)
library(NbClust)
library(factoextra)
library(surveillance)

# Establecemos una semilla para obtener siempre los mismos resultados
set.seed(3)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

data <- read.csv("datos_arreglados.csv")
# quitar la ultima columna
head(data, 10)
data <- subset(data, select = -c(X))
head(data, 10)
#data <- read_sas("datos/tabla_1.sas7bdat")
View(data)

# ----------------------------------------------------------------------

# Reconvertir los porcentajes proporcionados de manera que representen 
# el porcentaje de consumo de cada hora respecto al día (la suma de los
# porcentajes por registro deberá valer 1)

# Quitamos porcentajes

data$pct_h1 <- data$pct_h1*24*366
data$pct_h2 <- data$pct_h2*24*366
data$pct_h3 <- data$pct_h3*24*366
data$pct_h4 <- data$pct_h4*24*366
data$pct_h5 <- data$pct_h5*24*366
data$pct_h6 <- data$pct_h6*24*366
data$pct_h7 <- data$pct_h7*24*366
data$pct_h8 <- data$pct_h8*24*366
data$pct_h9 <- data$pct_h9*24*366
data$pct_h10 <- data$pct_h10*24*366
data$pct_h11 <- data$pct_h11*24*366
data$pct_h12 <- data$pct_h12*24*366
data$pct_h13 <- data$pct_h13*24*366
data$pct_h14 <- data$pct_h14*24*366
data$pct_h15 <- data$pct_h15*24*366
data$pct_h16 <- data$pct_h16*24*366
data$pct_h17 <- data$pct_h17*24*366
data$pct_h18 <- data$pct_h18*24*366
data$pct_h19 <- data$pct_h19*24*366
data$pct_h20 <- data$pct_h20*24*366
data$pct_h21 <- data$pct_h21*24*366
data$pct_h22 <- data$pct_h22*24*366
data$pct_h23 <- data$pct_h23*24*366
data$pct_h24 <- data$pct_h24*24*366

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


# NOS CARGAMOS LOS DOMINGOS PRIMEROS Y LOS LUNES ULTIMOS (NO HECHO)
# data_sin_domingos_lunes <- data[data$fecha != '2015-03-01' & data$fecha != '2016-02-29',]

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# Para obtener los festivos
data_festivos <- data[data$dias_semana %in% c("sábado", "domingo"), ]
View(data_festivos)

# Para obtener los laborales
data_laborales <- data[data$dias_semana %in% c("lunes", "martes", "miércoles", "jueves", "viernes"), ]
View(data_laborales)

# Una vez que tenemos las dos tablas: calcular la media de dichos porcentajes por CUPS

detach(package:plyr)
install.packages(plyr)
library(plyr)
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

data_media_festivos <- data_festivos %>%
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
View(data_media_festivos)

# ----------------------------------------------------------------------

# LABORALES

# Quitamos la columna cups para hacer las componentes principales
data_laborales_aux<-select(data_media_laborales, -cups)

# Hacemos componentes
pca_laborales <- princomp(data_laborales_aux, cor=FALSE, scores = TRUE, covmat = NULL) # matriz covarianzas
pca_laborales

# Vamos a probar con 8 componentes, 5 componentes, y con 3 componentes
eig.val <- get_eigenvalue(pca_laborales)
View(eig.val)

# Gráfico porcentaje de varianza explicada
fviz_eig(pca_laborales) + labs(title = "PCA para laborales") #Nos quedamos con tres o más -> decidir

pca_laborales$loadings

View(cbind(pca_laborales$loadings[,1], pca_laborales$loadings[,2], pca_laborales$loadings[,3], pca_laborales$loadings[,4], pca_laborales$loadings[,5]))

# Vectores con coeficientes de las componentes
comp1lab_vector <- unname(pca_laborales$loadings[,1])
comp2lab_vector <- unname(pca_laborales$loadings[,2])
comp3lab_vector <- unname(pca_laborales$loadings[,3])
comp4lab_vector <- unname(pca_laborales$loadings[,4])
comp5lab_vector <- unname(pca_laborales$loadings[,5])
#comp6lab_vector <- unname(pca_laborales$loadings[,6])
#comp7lab_vector <- unname(pca_laborales$loadings[,7])
#comp8lab_vector <- unname(pca_laborales$loadings[,8])

# CREAMOS DOS COLUMNAS EN LA TABLA DE MEDIAS CON EL VALOR DE CADA COMPONENTE 
# PARA CADA CUP (Se sustituyen las h_i en la ecuación de la componente)
for (i in 1:1000){
  data_media_laborales$comp1[i] <- sum(data_media_laborales[i,2:25]*comp1lab_vector)
  data_media_laborales$comp2[i] <- sum(data_media_laborales[i,2:25]*comp2lab_vector)
  data_media_laborales$comp3[i] <- sum(data_media_laborales[i,2:25]*comp3lab_vector)
  data_media_laborales$comp4[i] <- sum(data_media_laborales[i,2:25]*comp4lab_vector)
  data_media_laborales$comp5[i] <- sum(data_media_laborales[i,2:25]*comp5lab_vector)
  #data_media_laborales$comp6[i] <- sum(data_media_laborales[i,2:25]*comp6lab_vector)
  #data_media_laborales$comp7[i] <- sum(data_media_laborales[i,2:25]*comp7lab_vector)
  #data_media_laborales$comp8[i] <- sum(data_media_laborales[i,2:25]*comp8lab_vector)
}

laborales_componentes <- data_media_laborales[c("cups","comp1","comp2", "comp3", "comp4", "comp5")]
View(laborales_componentes)

laborales_componentes_pca <- data_media_laborales[c("comp1", "comp2", "comp3", "comp4", "comp5")]
View(laborales_componentes_pca)

# ----------------------------------------------------------------------

# FESTIVOS

# Quitamos la columna cups para hacer las componentes principales
data_festivos_aux<-select(data_media_festivos, -cups)

# Hacemos componentes
pca_festivos <- princomp(data_festivos_aux, cor=FALSE, scores = TRUE, covmat = NULL) # matriz covarianzas

# Vamos a probar con 8 componentes, 5 componentes, y con 3 componentes
eig.val <- get_eigenvalue(pca_festivos)
View(eig.val)

# Gráfico porcentaje de varianza explicada
fviz_eig(pca_festivos) + labs(title = "PCA para festivos") 
#Nos quedamos con tres o más -> decidir

View(cbind(pca_festivos$loadings[,1], pca_festivos$loadings[,2], pca_festivos$loadings[,3], pca_festivos$loadings[,4], pca_festivos$loadings[,5]))


# Vectores con coeficientes de las componentes
comp1fest_vector <- unname(pca_festivos$loadings[,1])
comp2fest_vector <- unname(pca_festivos$loadings[,2])
comp3fest_vector <- unname(pca_festivos$loadings[,3])
comp4fest_vector <- unname(pca_festivos$loadings[,4])
comp5fest_vector <- unname(pca_festivos$loadings[,5])
#comp6lab_vector <- unname(pca_laborales$loadings[,6])
#comp7lab_vector <- unname(pca_laborales$loadings[,7])
#comp8lab_vector <- unname(pca_laborales$loadings[,8])

# CREAMOS DOS COLUMNAS EN LA TABLA DE MEDIAS CON EL VALOR DE CADA COMPONENTE 
# PARA CADA CUP (Se sustituyen las h_i en la ecuación de la componente)
for (i in 1:1000){
  data_media_festivos$comp1[i] <- sum(data_media_festivos[i,2:25]*comp1fest_vector)
  data_media_festivos$comp2[i] <- sum(data_media_festivos[i,2:25]*comp2fest_vector)
  data_media_festivos$comp3[i] <- sum(data_media_festivos[i,2:25]*comp3fest_vector)
  data_media_festivos$comp4[i] <- sum(data_media_festivos[i,2:25]*comp4fest_vector)
  data_media_festivos$comp5[i] <- sum(data_media_festivos[i,2:25]*comp5fest_vector)
  #data_media_laborales$comp6[i] <- sum(data_media_laborales[i,2:25]*comp6lab_vector)
  #data_media_laborales$comp7[i] <- sum(data_media_laborales[i,2:25]*comp7lab_vector)
  #data_media_laborales$comp8[i] <- sum(data_media_laborales[i,2:25]*comp8lab_vector)
}

festivos_componentes <- data_media_festivos[c("cups","comp1","comp2", "comp3", "comp4", "comp5")]
View(festivos_componentes)

festivos_componentes_pca <- data_media_festivos[c("comp1", "comp2", "comp3", "comp4", "comp5")]
View(festivos_componentes_pca)

# ----------------------------------------------------------------------

# Determinación óptima del número de cluster LABORALES
optimo_laborales <- fviz_nbclust(laborales_componentes_pca, kmeans, method = "gap_stat")
optimo_laborales + labs(title = "Número óptimo de cluster para laborales") 

# the method to be used for estimating the optimal number of clusters. Possible 
# values are "silhouette" (for average silhouette width), "wss" (for total within 
# sum of square) and "gap_stat" (for gap statistics).
# If NULL, dist(x) is computed with the default method = "euclidean"

# Determinación óptima del número de cluster FESTIVOS
optimo_festivos <- fviz_nbclust(festivos_componentes_pca, kmeans, method = "gap_stat")
optimo_festivos + labs(title = "Número óptimo de cluster para festivos") 

# ----------------------------------------------------------------------

# K-MEANS 

# LABORALES
# La función kmeans() del paquete stats realiza K-mean-clustering. Entre sus 
# argumentos destacan: centers, que determina el número K de clusters que se 
# van a generar y nstart, que determina el número de veces que se va a repetir 
# el proceso, cada vez con una asignación aleatoria inicial distinta. Es recomendable 
# que este último valor sea alto, entre 25-50, para no obtener resultados malos 
# debido a una iniciación poco afortunada del proceso.
laborales_km_cluster <- kmeans(laborales_componentes_pca, centers=4, nstart=25)
View(laborales_km_cluster$cluster)

fviz_cluster(object = laborales_km_cluster, data = laborales_componentes_pca, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize = 9) +
  labs(title = "Resultados clustering laborales K-means (con etiquetas)") +
  theme_bw() + theme(legend.position = "none")

fviz_cluster(object = laborales_km_cluster, data = laborales_componentes_pca, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize=0) +
  labs(title = "Resultados clustering laborales K-means (sin etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")

View(laborales_km_cluster$size)

frame <- data.frame(laborales_km_cluster$size)
cluster <- row.names(frame)
graph <- ggplot(frame, aes(x="", y=laborales_km_cluster.size, fill=cluster))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
graph



# FESTIVOS
festivos_km_cluster <- kmeans(festivos_componentes_pca, centers=4, nstart=25)
View(festivos_km_cluster$cluster)
View(festivos_km_cluster$size)

fviz_cluster(object = festivos_km_cluster, data = festivos_componentes_pca, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize = 9) +
  labs(title = "Resultados clustering festivos K-means (con etiquetas)") +
  theme_bw() + theme(legend.position = "none")

fviz_cluster(object = festivos_km_cluster, data = festivos_componentes_pca, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize=0) +
  labs(title = "Resultados clustering festivos K-means (sin etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")

frame <- data.frame(festivos_km_cluster$size)
cluster <- row.names(frame)
graph <- ggplot(frame, aes(x="", y=festivos_km_cluster.size, fill=cluster))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
graph

# ----------------------------------------------------------------------

# K-MEDIOIDES 
# no lo haría

# ----------------------------------------------------------------------

# CLUSTERING JERÁRQUICO

# Compute hierarchical clustering and cut into 4 clusters LABORALES
jerar_laborales <- hcut(laborales_componentes_pca, k = 4, stand = TRUE)

dendograma_laborales <- fviz_dend(jerar_laborales, rect = TRUE, cex = 0.5) +
  labs(title = "Clustering jerárquico clustering Laborales",
       subtitle = "Distancia euclídea, Lincage complete, K=4")
dendograma_laborales


cluster_laborales <- hclust(d = dist(x = laborales_componentes_pca, method = "euclidean"), 
                            method = "ward.D")
plot(cluster_laborales, cex = 0.6)
x <- rect.hclust(cluster_laborales,k=4,border=2:5) 
x + labs(title = "Cluster Dendograma Laborales K=4")

fviz_dend(x = hc_euclidea_completo, k = 10, cex = 0.6) +
  geom_hline(yintercept = 2.85, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=10")

cluster_laborales <- hclust(laborales_componentes_pca, method = "ward.D") 
dendograma_laboral <- fviz_dend(cluster_laborales, rect = TRUE, cex = 0.5) +
  labs(title = "Clustering jerárquico clustering Laborales",
       subtitle = "Distancia euclídea, Lincage complete, K=4")
dendograma_laboral


# Compute hierarchical clustering and cut into 4 clusters FESTIVOS
jerar_festivos <- hcut(festivos_componentes_pca, k = 4, stand = TRUE)
dendograma_festivos <- fviz_dend(jerar_festivos, rect = TRUE, cex = 0.5) +
  labs(title = "Herarchical clustering Festivos",
       subtitle = "Distancia euclídea, Lincage complete, K=4")
dendograma_festivos


cluster_festivos <- hclust(d = dist(x = festivos_componentes_pca, method = "euclidean"), 
                            method = "ward.D")
plot(cluster_festivos, cex = 0.6)
rect.hclust(cluster_festivos,k=4,border=2:5) 

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

data_modificado <- data
View(data_modificado)

# AHORA VAMOS A COMPROBAR NUESTROS RESULTADOS

# LABORALES
# Obtenemos los días laborales
data_laborales_cluster <- data_modificado[data_modificado$dias_semana %in% c("lunes", "martes", "miércoles", "jueves", "viernes"), ]
View(data_laborales_cluster)
# Agrupamos por cups
laborales_group_by_cluster <- data_laborales_cluster %>%
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
View(laborales_group_by_cluster)

# Obtenemos la clasificación de los cluster para los laborales
cluster_laborales <- laborales_km_cluster$cluster
View(cluster_laborales)

# Asociamos cada CUPS a su cluster
l <- cbind(laborales_group_by_cluster, cluster_laborales)
View(l)

# Agrupamos por cluster
cluster_laborales <- l %>%
  group_by(cluster_laborales) %>%
  summarise(mean_h1 = mean(mean_h1), 
            mean_h2 = mean(mean_h2), 
            mean_h3 = mean(mean_h3), 
            mean_h4 = mean(mean_h4), 
            mean_h5 = mean(mean_h5), 
            mean_h6 = mean(mean_h6), 
            mean_h7 = mean(mean_h7), 
            mean_h8 = mean(mean_h8), 
            mean_h9 = mean(mean_h9), 
            mean_h10 = mean(mean_h10), 
            mean_h11 = mean(mean_h11), 
            mean_h12 = mean(mean_h12), 
            mean_h13 = mean(mean_h13), 
            mean_h14 = mean(mean_h14), 
            mean_h15 = mean(mean_h15), 
            mean_h16 = mean(mean_h16), 
            mean_h17 = mean(mean_h17), 
            mean_h18 = mean(mean_h18), 
            mean_h19 = mean(mean_h19), 
            mean_h20 = mean(mean_h20), 
            mean_h21 = mean(mean_h21), 
            mean_h22 = mean(mean_h22),
            mean_h23 = mean(mean_h23), 
            mean_h24 = mean(mean_h24))
View(cluster_laborales)

# Entonces ahora vamos hacer el gráfico para cada cluster
cluster1 <- cluster_laborales[cluster_laborales$cluster_laborales==1,]
cluster1 <- cluster1[-1]
lista_cluster1 <- as.numeric(cluster1)

cluster2 <- cluster_laborales[cluster_laborales$cluster_laborales==2,]
cluster2 <- cluster2[-1]
lista_cluster2 <- as.numeric(cluster2)

cluster3 <- cluster_laborales[cluster_laborales$cluster_laborales==3,]
cluster3 <- cluster3[-1]
lista_cluster3 <- as.numeric(cluster3)

cluster4 <- cluster_laborales[cluster_laborales$cluster_laborales==4,]
cluster4 <- cluster4[-1]
lista_cluster4 <- as.numeric(cluster4)


t_cluster_laborales <- as.data.frame(t(cluster_laborales))
t_cluster_laborales <- t_cluster_laborales[-c(1), ]
t_cluster_laborales <- cbind(index = seq(1, 24, 1), t_cluster_laborales)
View(t_cluster_laborales)

ggplot() +
  geom_line(t_cluster_laborales, mapping=aes(x=index, y=V1, color='red'), size = 1) +
  geom_line(t_cluster_laborales, mapping=aes(x=index, y=V2, color='blue'), size = 1) +
  geom_line(t_cluster_laborales, mapping=aes(x=index, y=V3, color='green'), size = 1) +
  geom_line(t_cluster_laborales, mapping=aes(x=index, y=V4, color='pink'), size = 1) +
  xlab("Horas") +
  ylab("Frecuencia") +
  labs(title = "Laborales") +
  scale_color_discrete(name = "Cluster", labels = c("1", "2", "3", "4"))

p + labs(colour = "Cylinders") 

plot(lista_cluster1, type="l", col="green")
axis(1, at = seq(1, 24, by = 1))
plot(lista_cluster2, type="l", col="red")
par(new=T)
plot(lista_cluster3, type="l", col="blue")
par(new=T)
plot(lista_cluster4, type="l", col="pink")
title("Laborales")
legend("topleft", c("cluster 1","cluster 2", "cluster 3", "cluster 4"),
       col=c("green","red", "blue", "pink"))
par(new=F)

# ----------------------------------------------------------------------

# FESTIVOS
# Obtenemos los días laborales

data_festivos_cluster <- data_modificado[data_modificado$dias_semana %in% c("sábado", "domingo"), ]
View(data_festivos_cluster)
# Agrupamos por cups
festivos_group_by_cluster <- data_festivos_cluster %>%
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
View(festivos_group_by_cluster)

# Obtenemos la clasificación de los cluster para los laborales
cluster_festivos <- festivos_km_cluster$cluster
View(cluster_festivos)

# Asociamos cada CUPS a su cluster
f <- cbind(festivos_group_by_cluster, cluster_festivos)
View(f)

# Agrupamos por cluster
cluster_festivos <- f %>%
  group_by(cluster_festivos) %>%
  summarise(mean_h1 = mean(mean_h1), 
            mean_h2 = mean(mean_h2), 
            mean_h3 = mean(mean_h3), 
            mean_h4 = mean(mean_h4), 
            mean_h5 = mean(mean_h5), 
            mean_h6 = mean(mean_h6), 
            mean_h7 = mean(mean_h7), 
            mean_h8 = mean(mean_h8), 
            mean_h9 = mean(mean_h9), 
            mean_h10 = mean(mean_h10), 
            mean_h11 = mean(mean_h11), 
            mean_h12 = mean(mean_h12), 
            mean_h13 = mean(mean_h13), 
            mean_h14 = mean(mean_h14), 
            mean_h15 = mean(mean_h15), 
            mean_h16 = mean(mean_h16), 
            mean_h17 = mean(mean_h17), 
            mean_h18 = mean(mean_h18), 
            mean_h19 = mean(mean_h19), 
            mean_h20 = mean(mean_h20), 
            mean_h21 = mean(mean_h21), 
            mean_h22 = mean(mean_h22),
            mean_h23 = mean(mean_h23), 
            mean_h24 = mean(mean_h24))
View(cluster_festivos)

# Entonces ahora vamos hacer el gráfico para cada cluster
cluster1 <- cluster_festivos[cluster_festivos$cluster_festivos==1,]
cluster1 <- cluster1[-1]
lista_cluster1 <- as.numeric(cluster1)

cluster2 <- cluster_festivos[cluster_festivos$cluster_festivos==2,]
cluster2 <- cluster2[-1]
lista_cluster2 <- as.numeric(cluster2)

cluster3 <- cluster_festivos[cluster_festivos$cluster_festivos==3,]
cluster3 <- cluster3[-1]
lista_cluster3 <- as.numeric(cluster3)

cluster4 <- cluster_festivos[cluster_festivos$cluster_festivos==4,]
cluster4 <- cluster4[-1]
lista_cluster4 <- as.numeric(cluster4)

plot(lista_cluster1, type="o", pch=21, bg="white", cex=0.6, col="green")
axis(1, at = seq(1, 24, by = 1))
par(new=T)
plot(lista_cluster2, type="o", pch=21, bg="white", cex=0.6, col="red")
par(new=T)
plot(lista_cluster3, type="o", pch=21, bg="white", cex=0.6, col="blue")
par(new=T)
plot(lista_cluster4, type="o", pch=21, bg="white", cex=0.6, col="pink")
par(new=T)
plot(lista_cluster5, type="o", pch=21, bg="white", cex=0.6, col="brown")
title("Festivos")
legend("topright",c("cluster 1","cluster 2", "cluster 3", "cluster 4", "cluster 5"), 
       col=c("green","red", "blue", "pink", "brown"),
       inset=c(0,1), xpd=TRUE, horiz=FALSE, bty="l", lty=1, cex=0.8)
par(new=F)



t_cluster_festivos <- as.data.frame(t(cluster_festivos))
t_cluster_festivos <- t_cluster_festivos[-c(1), ]
t_cluster_festivos <- cbind(index = seq(1, 24, 1), t_cluster_festivos)
View(t_cluster_festivos)

ggplot() +
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V1, group = 1), size = 1)+
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V2, group = 1), size = 1)+
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V3, group = 1), size = 1)+
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V4, group = 1), size = 1)+
  xlab("x axis") +
  ylab("y axis")

ggplot() +
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V1, color='red'), size = 1) +
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V2, color='blue'), size = 1) +
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V3, color='green'), size = 1) +
  geom_line(t_cluster_festivos, mapping=aes(x=index, y=V4, color='pink'), size = 1) +
  xlab("Horas") +
  ylab("Frecuencia") +
  labs(title = "Festivos") +
  scale_color_discrete(name = "Cluster", labels = c("1", "2", "3", "4"))


