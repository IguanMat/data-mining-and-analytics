#################### Librerie Utilizzate #######################################

library(cluster)
library(NbClust)
library(plyr)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse) 
library(RColorBrewer) 
library(Hmisc)
library(factoextra)
library(ggplot2)
library(ggcorrplot)
library(ggdendro)
library(yardstick)
library(stargazer)
library(FactoMineR)
library(psych)
library(vtable)
library(leaflet)
library(leaflet.minicharts)
library(magrittr)

################################################################################

View(olive)

# The first column gives the region: (1) Southern Italy, (2) Sardinia, or (3) Northern Italy. 
# The second column gives the area: (1) North Apulia, (2) Calabria, (3) South Apulia, (4) Sicily, 
# (5) Inland Sardinia, (6) Costal Sardinia, (7) East Liguria, (8) West Liguria, and (9) Umbria.

# creiamo un nuovo dataset senza le due variabili "categoriche"
olive_1 <- olive[,c(-1,-2)]
# scaliamo i dati per poter effettuare un'analisi
# La standardizzazione impedisce alle variabili con scale più ampie di dominare 
# il modo in cui vengono definiti i cluster.
olive.scaled <- data.frame(scale(olive_1))

################################################################################

# scambiamo i numeri con i rispettivi nomi
olive$Region <- revalue(x = as.character(olive$Region), 
                        replace = c("1" = "South", 
                                    "2" = "Sardinia", 
                                    "3" = "North"))
olive$Area <- revalue(x = as.character(olive$Area), 
                      replace = c("1" = "North Apulia", 
                                  "2" = "Calabria", 
                                  "3" = "South Apulia",
                                  "4" = "Sicily", 
                                  "5" = "Inland Sardinia", 
                                  "6" = "Costal Sardinia",
                                  "7" = "East Liguria", 
                                  "8" = "West Liguria", 
                                  "9" = "Umbria"))

# rappresentiamo la quantità di campioni di olio d'oliva per "Region"
olive %>% count(Region)
ggplot(olive, aes(Region, ..count..)) + geom_bar(aes(fill = as.factor(Region)), show.legend = FALSE) 

# rappresentiamo la quantità di campioni di olio d'oliva per "Area"
olive %>% count(Area)
ggplot(olive, aes(Area, ..count..)) + geom_bar(aes(fill = as.factor(Area)), show.legend = FALSE)

#################################### MAPPA #####################################

# duplichiamo la colonna Area...
olive$area_lat <- olive$Area
# ...e assegnamo a ogni nome la rispettiva latitudine
olive$area_lat <- revalue(x = as.character(olive$area_lat), 
                          replace = c("North Apulia" = "41.338553", 
                                      "Calabria" = "39.286362", 
                                      "South Apulia" = "40.495929",
                                      "Sicily" = "37.617371", 
                                      "Inland Sardinia" = "40.249017", 
                                      "Costal Sardinia" = "39.235323",
                                      "East Liguria" = "44.405224", 
                                      "West Liguria" = "44.185046", 
                                      "Umbria" = "42.919109"))
olive$area_lat <- as.numeric(olive$area_lat)

# duplichiamo la colonna Area...
olive$area_lon <- olive$Area
# ...e assegnamo a ogni nome la rispettiva longitudine
olive$area_lon <- revalue(x = as.character(olive$area_lon), 
                          replace = c("North Apulia" = "15.726337", 
                                      "Calabria" = "16.443904", 
                                      "South Apulia" = "17.778742",
                                      "Sicily" = "14.147761", 
                                      "Inland Sardinia" = "9.017146", 
                                      "Costal Sardinia" = "9.225886",
                                      "East Liguria" = "9.324763", 
                                      "West Liguria" = "8.259089", 
                                      "Umbria" = "12.532771"))
olive$area_lon <- as.numeric(olive$area_lon)

# duplichiamo la colonna Area...
olive$area_count <- olive$Area
# ...e assegnamo a ogni nome la rispettiva quantitÃ 
olive$area_count <- revalue(x = as.character(olive$area_count),
                            replace = c("North Apulia" = "25", 
                                        "Calabria" = "56", 
                                        "South Apulia" = "206",
                                        "Sicily" = "36", 
                                        "Inland Sardinia" = "65", 
                                        "Costal Sardinia" = "33",
                                        "East Liguria" = "50", 
                                        "West Liguria" = "50", 
                                        "Umbria" = "51"))
olive$area_count <- as.numeric(olive$area_count)

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
# costruiamo la mappa base 
basemap <- leaflet(width = "100%", height = "1000px") %>%
  addTiles(tilesURL)

# duplichiamo la colonna Area...
olive$area_colors <- olive$Area
# ...e assegnamo a ogni nome il rispettivo colore
olive$area_colors <- revalue(x = as.character(olive$area_colors),
                             replace = c("North Apulia" = "#FFFF00", 
                                         "Calabria" = "#964B00", 
                                         "South Apulia" = "#0000FF",
                                         "Sicily" = "#FF69B4", 
                                         "Inland Sardinia" = "#FF0000", 
                                         "Costal Sardinia" = "#00FF00",
                                         "Umbria" = "#8B008B", 
                                         "East Liguria" = "#FF8C00", 
                                         "West Liguria" = "#00CED1"))

# creiamo la mappa della quantitÃ  di campioni di olio d'oliva per ogni Area
leaflet() %>%
  addMinicharts(
    olive$area_lon, olive$area_lat,
    chartdata = olive$area_count,
    fillColor = olive$area_colors,
    showLabels = TRUE,
    width = 45,
    layerId = olive$Area
  ) %>%
  addLegend(
    "topright",
    colors = unique(olive$area_colors), opacity = 1,
    labels = unique(olive$Area)
  ) %>% addTiles()

################################ CLUSTERING ####################################

# stimiamo le distanze utilizzando tre metodi differenti 
dist_euc<-dist(olive.scaled,method="euclidean")
dist_man<-dist(olive.scaled,method="manhattan")
dist_min<-dist(olive.scaled,method="minkowski")

# hclust effettua una cluster analysis gerarchica sulle distanze precedentemente calcolate 
# utilizzando 4 diversi metodi: Single,Complete,Average e Ward
hc.eu_single=hclust(dist_euc,method="single",members=NULL)
hc.eu_com=hclust(dist_euc,method="complete",members=NULL)
hc.eu_avg=hclust(dist_euc,method="average",members=NULL)
hc.eu_ward=hclust(dist_euc,method="ward",members=NULL)

hc.man_single=hclust(dist_man,method="single",members=NULL)
hc.man_com=hclust(dist_man,method="complete",members=NULL)
hc.man_avg=hclust(dist_man,method="average",members=NULL)
hc.man_ward=hclust(dist_man,method="ward",members=NULL)

hc.min_single=hclust(dist_min,method="single",members=NULL)
hc.min_com=hclust(dist_min,method="complete",members=NULL)
hc.min_avg=hclust(dist_min,method="average",members=NULL)
hc.min_ward=hclust(dist_min,method="ward",members=NULL)

#grafichiamo i dendogrammi per i 4 metodi differenti per ogni tipo di distanza e osservando 
#il grafico più rilevante ottenuto con il metodo Ward possiamo osservare 5 cluster.

plot(hc.eu_single)
plot(hc.eu_com)
plot(hc.eu_avg)
plot(hc.eu_ward)

plot(hc.man_single)
plot(hc.man_com)
plot(hc.man_avg)
plot(hc.man_ward)

plot(hc.min_single)
plot(hc.min_com)
plot(hc.min_avg)
plot(hc.min_ward)

# plottiamo il dendogramma ricavato 
# dal metodo Ward applicato sulle distanze euclidee utilizzando GGplot2 
ggdendrogram(hc.eu_ward)

############################### CLUSTERING con NBclust ########################################

# cerchiamo il numero adeguato di cluster per il nostro dataset utilizzando la funzione NbClust 

clus <- NbClust(olive.scaled,method = "ward.D",index="all")
clus
clus$All.index
# osservando i valori per gli indici scegliamo il numero di cluster
# che massimizza l'indice CH Calinski-Harabasz, ovvero 5 Cluster.
# L'indice CH è una misura di quanto un oggetto sia simile al proprio cluster rispetto ad altri cluster

plot(olive.scaled, col=clus$Best.partition,main="Ward's minimum variance - Euclidean distance")
table(olive$region,clus$Best.partition)
# problema nel cluster 1 con i dati sia sud che nord variabile eicosenic
table(olive$area,clus$Best.partition)

hiward=cutree(hc.eu_ward,k=5)
hiward_man=cutree(hc.man_ward,k=5)
hiward_min=cutree(hc.min_ward,k=5)

plot(olive.scaled, col=hiward,main="Ward's minimum variance - Euclidean distance")
plot(olive.scaled, col=hiward_man,main="Ward's minimum variance - Manhattan distance")
plot(olive.scaled, col=hiward_min,main="Ward's minimum variance - Euclidean distance")

################################# K-Means ######################################

# k-means clustering è un metodo che mira a partizionare n osservazioni
# in k cluster in cui ogni osservazione appartiene al cluster con la media più vicina

set.seed(2022)
k.olive <- kmeans(olive.scaled,centers=5,nstart = 20,iter.max = 100)

#plot(olive.scaled[,c(5,4)],col=k.olive$cluster)
#plot(olive.scaled[,c(8,6)],col=k.olive$cluster)
#plot(olive.scaled[,c(1,8)],col=k.olive$cluster)

table(olive$Region,k.olive$cluster)
table(olive$Area,k.olive$cluster)

# valori medi di ogni variabile per ogni cluster
stargazer(k.olive$centers,type="text")

################################# PAM ##########################################

# PAM sta per "partizione attorno ai medoidi". L'algoritmo ha lo scopo di trovare una sequenza di
# oggetti chiamati medoidi che si trovano centralmente nei cluster.

p.olive<-pam(olive.scaled,5,metric = "euclidean",nstart = 50)
plot(olive.scaled,col=p.olive$clustering)

table(olive$Region,p.olive$cluster)
table(olive$Area,p.olive$cluster)
 
# tabella mediani delle variabili per ogni cluster
stargazer(p.olive$medoids,type="text")

################################ Silhouette + Cluster usando Factoextra ################################################

# con i metodi silhouette e wss controlliamo il corretto numero di cluster 
fviz_nbclust(olive.scaled,kmeans,method="silhouette")
# Within-Cluster Sum of Squares (WSS) is a measure of how far away each centroid
# is from their respective class instances. 
fviz_nbclust(olive.scaled,kmeans,method="wss")
#fviz_nbclust(olive.scaled,kmeans,method="gap_stat")

#fviz_cluster(k.olive,data=olive.scaled)
fviz_cluster(k.olive,olive.scaled, ellipse.type = "norm")+
  theme_minimal()

fviz_cluster(p.olive,olive.scaled, ellipse.type = "norm")+
  theme_minimal()

hiward=hcut(olive.scaled,k=5)
fviz_dend(hiward, show_labels = FALSE, rect = TRUE)
# The silhouette value is a measure of how similar an object is to its own cluster (cohesion) 
# compared to other clusters (separation). The silhouette ranges from ???1 to +1, where a high value 
# indicates that the object is well matched to its own cluster and poorly matched to neighboring clusters. 
# If most objects have a high value, then the clustering configuration is appropriate. 
# If many points have a low or negative value, then the clustering configuration may have too many or too few clusters.
fviz_silhouette(hiward)

olive.scaled[c(317,115,257,79,522),]

SIL <- silhouette(k.olive$cluster, dist(olive.scaled))
fviz_silhouette(SIL)
summary_SIL <- summary(SIL)

fviz_cluster(k.olive, data = olive.scaled,
             palette = c("#2E9FDF", "#E1AFBB", "#07B800","#D1B9F0","#E4B800" ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal()
)

######################### Grafici a barre per Cluster da kmeans ##################################################

kmeans_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = k.olive$cluster, olive.scaled)
# head of df
head(kmeans_basic_df)

ggplot(data=kmeans_basic_df, aes(y=Cluster))+
  geom_bar(aes(fill=Region))+
  ggtitle("Count of clusters by region ")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=kmeans_basic_df, aes(y=Cluster))+
  geom_bar(aes(fill=Area))+
  ggtitle("Count of clusters by Area ")+
  theme(plot.title = element_text(hjust = 0.5))

########################### Matrici di Confusione GGPlot #######################

table(kmeans_basic_df %>% dplyr::select(Region,Cluster)) %>% 
  data.frame() %>% 
  ggplot(aes(
    x = Cluster,
    y = fct_reorder(Region, desc(Region)),
    fill = Freq,
    label = Freq
  )) + 
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low = "#FFFFFF", high = "#135B4A", guide="none") +
  labs(
    x = "K-means Clusters", 
    y = "Region", 
    title = "Confusion matrix of k-means clusters and Region of olive oil samples") +
  theme_light()

table(kmeans_basic_df %>% dplyr::select(Area,Cluster)) %>% 
  data.frame() %>% 
  ggplot(aes(
    x = Cluster,
    y = fct_reorder(Area, desc(Area)),
    fill = Freq,
    label = Freq
  )) + 
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low = "#FFFFFF", high = "#135B4A", guide="none") +
  labs(
    x = "K-means Clusters", 
    y = "Area", 
    title = "Confusion matrix of k-means clusters and Area of olive oil samples") +
  theme_light()

################################################################################

sumtable(kmeans_basic_df,group ="Cluster",group.long = F)

#sumtable(kmeans_basic_df,group ="Region",group.long = F)

#by(kmeans_basic_df,factor(kmeans_basic_df$Region),summary)
by(kmeans_basic_df,factor(kmeans_basic_df$Cluster),summary)

################################ Risultati Cluster #############################

# Nel primo cluster( prevalentemente Nord ) notiamo gli oli con valori bassi 
# per gli acidi Palmitic, Palmitoleic , Linoleic ed Eicosenoic e valori alti per acido Oleic

# Il secondo cluster(Nord) evidenzaia oli con bassi valori in Eicosenoic, Linolenic, Palmitic
# e Arachidic ma con  valori alti negli acidi Oleic e Stearic.

# Il terzo cluster(Sud) presenta oli con valori alti per gli acidi Linoleic, Palmitoleic,
# e Palmitic e bassi nell' Oleic.

# Nel quarto cluster (prevalentemente Sud) sono presenti oli con valori elevati in Eicosenoic,
# Linolenic, Arachidic, e Stearic e valori bassi in Linoleic.

# Infine il quinto cluster( prevalentemente Sardegna ) presenta oli con valori bassi
# in Eicosenoic, Linolenic, Palmitoleic and Palmitic e valori elevati in Arachidic e Linoleic

# I south del primo cluster hanno oleic alto per questo vengono raggruppati nel primo cluster
# invece che nel terzo o nel quarto
# I nord del quarto cluster hanno Eicoseoic elevato per questo motivo non sono stati raggruppati
# nei cluster 1 e 2 di prevalenza Nord
# I nord del quinto cluster hanno Arachidic elevato, questo comporta il loro posizionamento 
# nel quinto cluster piuttosto che nel primo o secondo.

################################## Grafici a barre per Cluster da PAM ##############################################

pam_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = p.olive$cluster, olive.scaled)
# head of df
head(pam_basic_df)

ggplot(data=pam_basic_df, aes(y=Cluster))+
  geom_bar(aes(fill=Region))+
  ggtitle("Count of clusters by Region ")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=pam_basic_df, aes(y=Cluster))+
  geom_bar(aes(fill=Area))+
  ggtitle("Count of clusters by Area ")+
  theme(plot.title = element_text(hjust = 0.5))

########################### CORRELAZIONI #######################################

corr <- round(cor(olive.scaled), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of olive", 
           ggtheme=theme_bw)

################################# PCA ##########################################

CR = eigen(corr)
Auto_vet = CR$vectors
Auto_val = CR$values
pvarspiegata = Auto_val/8
pvarspcum = cumsum(pvarspiegata)
plot(Auto_val,type="b",main="scree plot",xlab="numero componenti",ylab = "autoval")
abline(h=1,lwd=3,col="red")
pca=prcomp(olive.scaled)
pca$x
summary(pca)
plot(pca)
screeplot(pca,type=c("lines"))
biplot(pca)

olive.scaled_1 <- cbind(olive.scaled,pca$x[,1:3])
cor(olive.scaled,pca$x[,1:3])

# Compute PCA with ncp = 4
res.pca <- PCA(olive.scaled, ncp = 5, graph = T)
res.pca_1 <- PCA(olive.scaled, ncp = 3, graph = T)
summary.PCA(res.pca)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = F)

fviz_dend(res.hcpc, 
          cex = 0.7,                     
          palette = "jco",               
          rect = TRUE, rect_fill = TRUE,
          rect_border = "jco",           
          labels_track_height = 0.8      
)

fviz_cluster(res.hcpc,
             repel = TRUE,            
             show.clust.cent = TRUE,
             palette = "jco",         
             ggtheme = theme_minimal(),
             main = "Factor map"
)

pca_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = res.hcpc$data.clust$clust, olive.scaled)

stargazer(res.hcpc$desc.var$quanti,type="text")
table(olive$Region,res.hcpc$data.clust$clust)
table(olive$Area,res.hcpc$data.clust$clust)


table(pca_basic_df %>% dplyr::select(Region,Cluster)) %>% 
  data.frame() %>% 
  ggplot(aes(
    x = Cluster,
    y = fct_reorder(Region, desc(Region)),
    fill = Freq,
    label = Freq
  )) + 
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low = "#FFFFFF", high = "#135B4A", guide="none") +
  labs(
    x = "PCA Clusters", 
    y = "Region", 
    title = "Confusion matrix of PCA clusters and Region of olive oil samples") +
  theme_light()

table(pca_basic_df %>% dplyr::select(Area,Cluster)) %>% 
  data.frame() %>% 
  ggplot(aes(
    x = Cluster,
    y = fct_reorder(Area, desc(Area)),
    fill = Freq,
    label = Freq
  )) + 
  geom_tile() +
  geom_text() +
  scale_fill_gradient(low = "#FFFFFF", high = "#135B4A", guide="none") +
  labs(
    x = "PCA Clusters", 
    y = "Area", 
    title = "Confusion matrix of PCA clusters and Area of olive oil samples") +
  theme_light()

