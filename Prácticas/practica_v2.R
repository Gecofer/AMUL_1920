##COMPONENTES PRINCIPALES## -> COVARIANZAS



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

## DÍAS LABORALES ##

#Quitamos la columna cups para hacer las componentes principales
data_laborales_aux<-select(data_media_laborales, -cups)
#Hacemos componentes
pca_laborales <- princomp(data_laborales_aux, cor=FALSE, scores = TRUE, covmat = NULL)
#Gráfico porcentaje de varianza explicada
fviz_eig(pca_laborales) #Nos quedamos con tres o más -> decidir
#Vectores con coeficientes de las componentes
comp1lab_vector <- unname(pca_laborales$loadings[,1])
comp2lab_vector <- unname(pca_laborales$loadings[,2])
comp3lab_vector <- unname(pca_laborales$loadings[,3])

#CREAMOS DOS COLUMNAS EN LA TABLA DE MEDIAS CON EL VALOR DE CADA COMPONENTE 
#PARA CADA CUP (Se sustituyen las h_i en la ecuación de la componente)
for (i in 1:1000){
  data_media_laborales$comp1[i] <- sum(data_media_laborales[i,2:25]*comp1lab_vector)
  data_media_laborales$comp2[i] <- sum(data_media_laborales[i,2:25]*comp2lab_vector)
  data_media_laborales$comp3[i] <- sum(data_media_laborales[i,2:25]*comp3lab_vector)
}

# Nueva tabla solo con Cups y componentes

laborales_componentes <- data_media_laborales[c("cups","comp1","comp2", "comp3")]
View(laborales_componentes)

# ----------

laborales_componentes_pca <- data_media_laborales[c("comp1","comp2", "comp3")]
View(laborales_componentes_pca)

# -------

# Determinación óptima del número de cluster
optimo <- fviz_nbclust(laborales_componentes_pca, kmeans, method = "gap_stat")
optimo

par(mfrow=c(2,2)) 
# Elbow method
elbow <- fviz_nbclust(laborales_componentes_pca, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
silhouette <- fviz_nbclust(laborales_componentes_pca, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
silhouette$plot_env

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
gap <- fviz_nbclust(laborales_componentes_pca, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

library(NbClust)
NbClust(data = laborales_componentes_pca, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "ward.D2")


nc <- NbClust(laborales_componentes_pca, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
table(nc$Best.n[1,])

View(nc$Best.nc)

# Cluster kmeans
km.res <- kmeans(laborales_componentes_pca, 4, nstart = 25)
km.res$size

frame <- data.frame(km.res$size)
cluster <- row.names(frame)
bp<- ggplot(frame, aes(x="", y=km.res.size, fill=cluster))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie



ggplot(frame, aes(x=factor(1), fill=km.res.size))+
  geom_bar(width = 1)+
  coord_polar("x")

fviz_cluster(km.res, data = laborales_componentes_pca, ellipse.type = "convex")

library("factoextra")
# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(laborales_componentes_pca, k = 4, stand = TRUE)
# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
View(res$cluster)

hist(res$cluster)

# Vector con la categoria 1
which(res$cluster==1)
sum(res$cluster==1)

# Vector con la categoria 2
which(res$cluster==2)
sum(res$cluster==2)

# Vector con la categoria 3
which(res$cluster==3)
sum(res$cluster==3)

# Vector con la categoria 4
which(res$cluster==4)
sum(res$cluster==4)

hist(c(sum(res$cluster==4, sum(res$cluster==2))))



pca_laborales_clustering <- kmeans(laborales_componentes_pca, 3)
fviz_cluster(object = pca_laborales_clustering, data = laborales_componentes_pca, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, labelsize=0) +
  labs(title = "Resultados clustering K-means (sin etiquetas)") +
  theme_bw() +
  theme(legend.position = "none")

fviz_nbclust(x = laborales_componentes_pca, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")

km_clusters <- eclust(x = pca_laborales$x, FUNcluster = "kmeans", k = 2, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 






library(tabulizerjars)
install.packages("tabulizer")
install.packages("tabulizerjars")
library(dplyr)
out <- extract_tables(location)
