library(dplyr)

##### APARTADO 3 PERSONAS #####

data <- read.csv("datos_arreglados.csv")
#View(data)

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

#Nos cargamos el primer día (domingo) y último (lunes) para que nos queden semanas
#completas españolas Lunes-Domingo.
#Nota: Al quitar esos días nos quedamos con 364 días por CUP -> 12 semanas

data_new<-data[data$fecha!='2015-03-01'& data$fecha!='2016-02-29',]

# Nueva columna que indica la semana
data_new$semana<-rep(0)

#Asignar número de la semana (Un poco cutre así que tarda)

vec_semana_1<-c(rep(1:52,each=7))
vec_semana_1000<-c(rep(vec_semana_1,1000))


for (i in 1:(dim(data_new)[1])){
  data_new$semana[i]<-vec_semana_1000[i]
}

#Crear nuevo csv
write.csv(data_new,"datos/datos_arreglados_semanas.csv")

data_new<-read.csv("datos/datos_arreglados_semanas.csv")
View(data_new)
data_new<-data_new[,3:30] #Quitamos columnas índices raros
View(data_new)

#Nueva columna suma consumo cada día
data_new$sum_dia=rowSums(data_new[,3:26]) #Suma columnas horas

#Limpiar tabla
datos<-data_new[c("cups","semana","dias_semana","sum_dia")]
View(datos)

# Hacer group by por cups y por semanas para hacer el total de consumo de la semana

total_semana<- datos  %>% 
  group_by(cups, semana) %>% 
  summarise(sum_semana = sum(sum_dia))

#Left Join
datos_merge<-merge(datos,total_semana, by=c("cups","semana"), all.x=TRUE)

#Ordenamos datos por cups y semana
datos<-datos_merge[order(datos_merge$cups,datos_merge$semana),]
View(datos)

#Nueva columna porcentaje día respecto semana
#datos$Dia_percent<-(datos$sum_dia/datos$sum_semana) 
datos$Dia_percent<-ifelse(!datos$sum_semana, 0, datos$sum_dia/datos$sum_semana)

miau<-total_semana[,1:2]
dias_semana<-c("lunes","martes","miercoles","jueves","viernes","sabado","domingo")
miau[ ,dias_semana]<-NA

dias_percent<-datos$Dia_percent

# TARDA 10 MINUTOS
for (i in 1:dim(miau)[1]){
  for (j in 3:9){
    miau[i,j] <- dias_percent[1]
    dias_percent <- dias_percent[-1]
    
  }
}

View(miau)


#### PONER TABLA BIEN ##### -> Combinar tabla total_semanas con los días_ percent
# de los 7 días

View(media_semanas)

media_semanas<- miau  %>% 
  group_by(cups) %>% 
  summarise(mean_lunes = mean(lunes, na.rm=TRUE), 
            mean_martes = mean(martes, na.rm=TRUE), 
            mean_miercoles = mean(miercoles, na.rm=TRUE), 
            mean_jueves = mean(jueves, na.rm=TRUE), 
            mean_viernes = mean(viernes, na.rm=TRUE), 
            mean_sabado = mean(sabado, na.rm=TRUE), 
            mean_domingo = mean(domingo, na.rm=TRUE))

View(media_semanas)

write.csv(media_semanas, "medias_semanas.csv")

# ----------------------------------------------------------------------

# NO HACE FALTA HACER PCA, SERÍA INTERESANTE VERLO

# Quitamos la columna cups para hacer las componentes principales
data_media_semanas_aux<-select(media_semanas, -cups)

# Hacemos componentes
pca_semanas <- princomp(data_media_semanas_aux, cor=FALSE, scores = TRUE, covmat = NULL) # matriz covarianzas

# Vamos a probar con 8 componentes, 5 componentes, y con 3 componentes
eig_val_semanas <- get_eigenvalue(pca_semanas)
eig_val_semanas

# Gráfico porcentaje de varianza explicada
fviz_eig(pca_semanas) #Nos quedamos con tres o más -> decidir

# Vectores con coeficientes de las componentes
comp1semanas_vector <- unname(pca_semanas$loadings[,1])
comp2semanas_vector <- unname(pca_semanas$loadings[,2])

# CREAMOS DOS COLUMNAS EN LA TABLA DE MEDIAS CON EL VALOR DE CADA COMPONENTE 
# PARA CADA CUP (Se sustituyen las h_i en la ecuación de la componente)
for (i in 1:1000){
  media_semanas$comp1[i] <- sum(media_semanas[i,2:8]*comp2semanas_vector)
  media_semanas$comp2[i] <- sum(media_semanas[i,2:8]*comp2semanas_vector)
}

semanas_componentes_pca <- data_media_festivos[c("comp1", "comp2")]
View(semanas_componentes_pca)


# ----------------------------------------------------------------------

# Determinación óptima del número de cluster
optimo_media_semanas <- fviz_nbclust(data_media_semanas_aux, kmeans, method = "gap_stat")
optimo_media_semanas + labs(title = "Número óptimo de cluster para las semanas") 


prueba <- fviz_nbclust(data_media_semanas_aux, kmeans)
prueba

# ----------------------------------------------------------------------

# K-MEANS 
semanas_km_cluster <- kmeans(data_media_semanas_aux, centers=3, nstart=25)
View(semanas_km_cluster$cluster)
View(semanas_km_cluster$size)

write.csv(semanas_km_cluster$cluster, "semanas_cluster.csv")

fviz_cluster(object = semanas_km_cluster, data = data_media_semanas_aux, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize = 9) +
  labs(title = "Resultados clustering semanas K-means (con etiquetas)") +
  theme_bw() + theme(legend.position = "none")

fviz_cluster(object = semanas_km_cluster, data = data_media_semanas_aux, 
             show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, 
             repel = TRUE, labelsize=0) +
  labs(title = "Resultados clustering semanas K-means (sin etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")

frame <- data.frame(semanas_km_cluster$size)
cluster <- row.names(frame)
graph <- ggplot(frame, aes(x="", y=semanas_km_cluster.size, fill=cluster))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
graph

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

# CLUSTERING JERÁRQUICO

# Compute hierarchical clustering and cut into 4 clusters LABORALES
jerar_semanas <- hcut(data_media_semanas_aux, k = 3, stand = TRUE)
dendograma_semanas <- fviz_dend(jerar_semanas, rect = TRUE, cex = 0.5) +
  labs(title = "Clustering jerárquico clustering Semanas",
       subtitle = "Distancia euclídea, Lincage complete, K=3")
dendograma_semanas

cluster_semanas <- hclust(d = dist(x = data_media_semanas_aux, method = "euclidean"), 
                           method = "ward.D")
plot(cluster_semanas, cex = 0.6)
rect.hclust(cluster_semanas,k=3,border=2:5) 

# ----------------------------------------------------------------------

data_modificado_semanas <- data
data_modificado_semanas <- subset(data_modificado_semanas, select = -c(X))
View(data_modificado_semanas)

# AHORA VAMOS A COMPROBAR NUESTROS RESULTADOS
semanas_group_by_cluster <- data_modificado_semanas %>%
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
View(semanas_group_by_cluster)



semanas_group_by_cluster <- data_modificado_semanas %>%
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
View(semanas_group_by_cluster)

cluster_laborales <- laborales_km_cluster$cluster

# --

library(ggplot)
data_semanas<-read.csv("medias_semanas.csv")
data_cluster<-read.csv("semanas_cluster.csv")

names (data_cluster)[1]="cups"
data<-merge(data_semanas,data_cluster, by="cups", all.x=TRUE)

media_cluster<- data  %>% 
  group_by(x) %>% 
  summarise(mean_lunes = mean(mean_lunes, na.rm=TRUE), 
            mean_martes = mean(mean_martes, na.rm=TRUE), 
            mean_miercoles = mean(mean_miercoles, na.rm=TRUE), 
            mean_jueves = mean(mean_jueves, na.rm=TRUE), 
            mean_viernes = mean(mean_viernes, na.rm=TRUE), 
            mean_sabado = mean(mean_sabado, na.rm=TRUE), 
            mean_domingo = mean(mean_domingo, na.rm=TRUE))

cluster1 <- media_cluster[media_cluster$x==1,]
cluster1 <- cluster1[-1]
lista_cluster1 <- as.numeric(cluster1)

cluster2 <- media_cluster[media_cluster$x==2,]
cluster2 <- cluster2[-1]
lista_cluster2 <- as.numeric(cluster2)

cluster3 <- media_cluster[media_cluster$x==3,]
cluster3 <- cluster3[-1]
lista_cluster3 <- as.numeric(cluster3)

t_cluster<- as.data.frame(t(media_cluster))
t_cluster <- t_cluster[-c(1), ]
t_cluster<- cbind(index = seq(1, 7, 1), t_cluster)
View(t_cluster)

ggplot() +
  geom_line(t_cluster, mapping=aes(x=index, y=V1, color='red'), size = 1) +
  geom_line(t_cluster, mapping=aes(x=index, y=V2, color='blue'), size = 1) +
  geom_line(t_cluster, mapping=aes(x=index, y=V3, color='green'), size = 1) +
  xlab("Días") +
  ylab("Consumo medio") +
  labs(title = "Consumo medio semanal por clúster") +
  scale_color_discrete(name = "Cluster", labels = c("1", "2", "3"))



