# Packages ----------------------------------------------------------------
packs <- c("shiny", "shinydashboard", "shinyWidgets", "DT", "shinycssloaders", "dplyr", "plotly", "ggplot2","magrittr", 
           "ggtext", "tidyverse", "leaflet", "leaflet.extras", "jsonlite", "geojsonio", "data.table", "IMIFA",
           "cluster","NbClust","plyr","tidyr","stringr","RColorBrewer","Hmisc","factoextra", "ggcorrplot",
           "ggdendro","yardstick","stargazer","FactoMineR","psych","vtable","leaflet","leaflet.minicharts",
           "mclust", "teigen","ContaminatedMixt","sparcl","SelvarMix","clustvarsel","vscc")
# lapply() returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X
# require() (and library) load and attach add-on packages
lapply(packs, require, character.only = TRUE)

# Data --------------------------------------------------------------------
data(olive)
# correggiamo nomi colonne
colnames(olive)<- c("Region","Area","Palmitic","Palmitoleic","Stearic","Oleic","Linoleic","Linolenic","Arachidic","Eicosenoic")
# correggiamo "Coastal Sardinia" nella colonna region
olive$Area <- revalue(x = as.character(olive$Area), 
                            replace = c("Coastal Sardinia" = "Costal Sardinia"))
# creiamo un nuovo dataset senza le due variabili "categoriche"
olive.new <- olive[,c(-1,-2)]
# scaliamo i dati per poter effettuare un'analisi
# La standardizzazione impedisce alle variabili con scale più ampie di dominare 
# il modo in cui vengono definiti i cluster.
olive.scaled <- data.frame(scale(olive.new))
# duplichiamo la colonna region...
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
# duplichiamo la colonna region...
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
# duplichiamo la colonna region...
olive$area_count <- olive$Area
# ...e assegnamo a ogni nome la rispettiva quantita
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
# duplichiamo la colonna region...
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
#
corr <- round(cor(olive.scaled), 1)
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
# cerchiamo il numero adeguato di cluster per il nostro dataset utilizzando la funzione NbClust 
clus <- NbClust(olive.scaled,method = "ward.D",index="all")
#
set.seed(2022)
k.olive <- kmeans(olive.scaled,centers=5,nstart = 20,iter.max = 100)
#
kmeans_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = k.olive$cluster, olive.scaled)
#
p.olive<-pam(olive.scaled,5,metric = "euclidean",nstart = 50)
#
pam_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = p.olive$cluster, olive.scaled)
#
res.pca <- PCA(olive.scaled, ncp = 2, graph = T)
#
res.hcpc <- HCPC(res.pca, graph = F)
#
pca_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = res.hcpc$data.clust$clust, olive.scaled)
#
clustering_5 <- Mclust(olive.scaled,G=1:5)
#
ICL <- mclustICL(olive.scaled,G=1:5)
#
clustering_9 <- Mclust(olive.scaled)
#
teign <- teigen(olive.scaled)
#
contam <- CNmixt(olive.scaled, G =1:5,verbose=T, seed = 2022)
#
km.perm <- KMeansSparseCluster.permute(olive.scaled,K=5,wbounds=seq(3,7,len=15),nperms=5)
km.out <- KMeansSparseCluster(olive.scaled,K=5,wbounds=km.perm$bestw)
ksparse_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = km.out[[1]]$Cs , olive.scaled)
#
clust.vscc <- vscc(olive.scaled,G=1:5, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)
clust.vsccns <- vscc(olive.new,G=1:6, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)
vscc_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = clust.vscc$bestmodel$classification , olive.scaled)
vsccns_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = clust.vsccns$bestmodel$classification , olive.new)
#
lasso <- SelvarClustLasso(x=olive.scaled,nbcluster=5,criterio="BIC")
lasso_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = lasso$partition , olive.scaled)
lassons <- SelvarClustLasso(x=olive.new,nbcluster=6,criterio="BIC")
lassons_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = lassons$partition , olive.new)
#
clust <- clustvarsel(olive.scaled,G=1:5)
varsel_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = clust$model$classification , olive.scaled)
clustns <- clustvarsel(olive.new,G=1:6)
clustns_ <- clustvarsel(olive.new,G=1:6, direction = "backward")
varselns_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = clustns$model$classification , olive.new)
varselns7_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = clustns_$model$classification , olive.new)
#
teign7 <- teigen(olive.scaled[,clustns_$subset], G = 1:6, models = "all")
teigen7_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = teign7$classification , olive.scaled)
teign6 <- teigen(olive.scaled[,clustns$subset], models = "all")
teigen6_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = teign6$classification , olive.scaled)
#
teigenns_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = teign_notsc$classification , olive.new)
#
contam_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = getCluster(contam) , olive.scaled)
#
contam7 <- CNmixt(olive.new[,clustns_$subset], G =1:6,verbose=T, seed = 2022)
contam7_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = getCluster(contam7) , olive.new)
contam6 <- CNmixt(olive.new[,clustns$subset], G =1:6,verbose=T, seed = 2022)
contam6_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = getCluster(contam6) , olive.new)
#
teign_notsc <- teigen(olive.new,scale=FALSE,models="all")
#
clustering_5_notsc <- Mclust(olive.new,G=1:9)
#
ICL_notsc <- mclustICL(olive.new,G=1:9)
#
teigen_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = teign$classification , olive.scaled)
#
teigenns_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = teign_notsc$classification , olive.new)
#
contam_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = getCluster(contam) , olive.scaled)
#
contam_notsc <- CNmixt(olive.new, G = 1:6,verbose=T, seed = 2022)
#
contamns_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = getCluster(contam_notsc) , olive.new)
#
drmod_notsc <- MclustDR(clustering_5_notsc,lambda=1)
drmodnsc_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = drmod_notsc$classification, olive.new)
#
drmod <- MclustDR(clustering_5,lambda=1)
drmod_basic_df <- data.frame(Area = olive$Area,Region=olive$Region,Cluster = drmod$classification, olive.scaled)
#
In_nb<- adjustedRandIndex(olive$Region,clus$Best.partition)
In_nb2<- adjustedRandIndex(olive$Area,clus$Best.partition)

In_km <- adjustedRandIndex(olive$Region,k.olive$cluster)
In_km2 <- adjustedRandIndex(olive$Area,k.olive$cluster)

In_pam <- adjustedRandIndex(olive$Region,p.olive$cluster)
In_pam2 <- adjustedRandIndex(olive$Area,p.olive$cluster)

In_mb <- adjustedRandIndex(olive[,1],clustering_5$classification)
In_mb2 <- adjustedRandIndex(olive[,2],clustering_5$classification)

In_tei<- adjustedRandIndex(olive[,1],teign$classification)
In_tei2<- adjustedRandIndex(olive[,2],teign$classification)

In_cont<- adjustedRandIndex(olive[,1],getCluster(contam))
In_cont2<- adjustedRandIndex(olive[,2],getCluster(contam))

In_kmsparse <- adjustedRandIndex(olive[,1],km.out[[1]]$Cs)
In_kmsparse2 <- adjustedRandIndex(olive[,2],km.out[[1]]$Cs)

In_lss<- adjustedRandIndex(olive[,1],lasso$partition)
In_lss2<- adjustedRandIndex(olive[,2],lasso$partition)

In_cvs <- adjustedRandIndex(olive[,1],clust$model$classification)
In_cvs2 <- adjustedRandIndex(olive[,2],clust$model$classification)

In_invscc <- adjustedRandIndex(olive[,1],clust.vscc$initialrun$classification)
In_invscc2 <- adjustedRandIndex(olive[,2],clust.vscc$initialrun$classification)
In_bestvscc <- adjustedRandIndex(olive[,1],clust.vscc$bestmodel$classification)
In_bestvscc2 <- adjustedRandIndex(olive[,2],clust.vscc$bestmodel$classification)

In_drmod <- adjustedRandIndex(olive[,1],drmod$classification)
In_drmod2 <- adjustedRandIndex(olive[,2],drmod$classification)
data = matrix(cbind(c(In_bestvscc,In_cont,In_cvs,In_invscc,In_km,In_kmsparse,In_nb,In_mb,In_pam,In_tei,In_drmod),c(In_bestvscc2,In_cont2,In_cvs2,In_invscc2,In_km2,In_kmsparse2,In_nb2,In_mb2,In_pam2,In_tei2,In_drmod2)), ncol=2, byrow=F)
colnames(data) = c('Region',"Area")
rownames(data) <- c("ARI-bestvscc","ARI-cont","ARI-cvs","ARI-invscc","ARI-km","ARI-kmsparse","ARI-nb","ARI-mb","ARI-pam","ARI-tei","ARI-drd")
#
In_mb_nsc <- adjustedRandIndex(olive[,1],clustering_5_notsc$classification)
In_mb2_nsc <- adjustedRandIndex(olive[,2],clustering_5_notsc$classification)

In_tei_nsc<- adjustedRandIndex(olive[,1],teign_notsc$classification)
In_tei2_nsc<- adjustedRandIndex(olive[,2],teign_notsc$classification)
In_tei6 <- adjustedRandIndex(olive[,1],teign6$classification)
In_tei6_2 <- adjustedRandIndex(olive[,2],teign6$classification)
In_tei7 <- adjustedRandIndex(olive[,1],teign7$classification)
In_tei7_2 <- adjustedRandIndex(olive[,2],teign7$classification)


In_cont_nsc<- adjustedRandIndex(olive[,1],getCluster(contam_notsc))
In_cont2_nsc<- adjustedRandIndex(olive[,2],getCluster(contam_notsc))
In_cont_nscsub <- adjustedRandIndex(olive[,1],getCluster(contam6))
In_cont_nscsub2 <- adjustedRandIndex(olive[,2],getCluster(contam6))
In_cont_nscsub3 <- adjustedRandIndex(olive[,1],getCluster(contam7))
In_cont_nscsub4 <- adjustedRandIndex(olive[,2],getCluster(contam7))

In_lss_nsc<- adjustedRandIndex(olive[,1],lassons$partition)
In_lss2_nsc<- adjustedRandIndex(olive[,2],lassons$partition)

In_cvs_nsc <- adjustedRandIndex(olive[,1],clustns$model$classification)
In_cvs2_nsc <- adjustedRandIndex(olive[,2],clustns$model$classification)
In_cvs3_nsc <- adjustedRandIndex(olive[,1],clustns_$model$classification)
In_cvs4_nsc <- adjustedRandIndex(olive[,2],clustns_$model$classification)

In_invscc_nsc <- adjustedRandIndex(olive[,1],clust.vsccns$initialrun$classification)
In_invscc2_nsc<- adjustedRandIndex(olive[,2],clust.vsccns$initialrun$classification)
In_bestvscc_nsc<- adjustedRandIndex(olive[,1],clust.vsccns$bestmodel$classification)
In_bestvscc2_nsc<- adjustedRandIndex(olive[,2],clust.vsccns$bestmodel$classification)

In_drmod_notsc <- adjustedRandIndex(olive[,1],drmod_notsc$classification)
In_drmod_notsc2 <- adjustedRandIndex(olive[,2],drmod_notsc$classification)
data2 = matrix(cbind(c(In_mb_nsc,In_cont_nsc,In_cont_nscsub,In_cont_nscsub3,In_cvs_nsc,In_cvs3_nsc,In_invscc_nsc,In_tei_nsc,In_tei6,In_tei7,In_bestvscc_nsc,In_drmod_notsc),c(In_mb2_nsc,In_cvs2_nsc,In_cont2_nsc,In_cont_nscsub2,In_cont_nscsub4,In_cvs4_nsc,In_invscc2_nsc,In_tei2_nsc,In_tei6_2,In_tei7_2,In_bestvscc2_nsc,In_drmod_notsc2)), ncol=2, byrow=F)
colnames(data2) = c('Region',"Area")
rownames(data2) <- c("ARI-mb_nsc","ARI-cont","ARI-cont_sub6","ARI-cont_sub7","ARI-cvs_nsc","ARI-cvs_sub_nsc","ARI-invscc_nsc","ARI-tei_nsc","ARI-tei6_nsc","ARI-tei7_nsc","ARI-bestvscc_nsc","ARI-drd")

#kable(data) %>% kable_styling(latex_options =c("striped",font_size=5,html_font = "helvetica"))
#kable(data2) %>% kable_styling(latex_options =c("striped",font_size=5,html_font = "helvetica"))

# UI ----------------------------------------------------------------------
ui <- dashboardPage(title = "Olive Dataset Analysis - Data Mining 3", 
                    dashboardHeader(title = tags$img(src="https://upload.wikimedia.org/wikipedia/commons/a/a2/Universit%C3%A0-LUMSA-logo.png", height = 45, align = "center"),
                                    titleWidth = 230,
                                    # the <li> tag defines a list item
                                    # the <a> tag defines a hyperlink, which is used to link from one page to another
                                    # _blank	opens the linked document in a new window or tab
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/matteogurrieri/", icon("linkedin"), "Matteo Gurrieri", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/riccardo-bianchi-4928b0251/", icon("linkedin"), "Riccardo Bianchi", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/edoardo-mercuri-b50a04256/", icon("linkedin"), "Edoardo Mercuri", target = "_blank")),
                                    tags$li(class="dropdown", tags$a(href="https://www.linkedin.com/in/paolo-losacco-888278239/", icon("linkedin"), "Paolo Losacco", target = "_blank")),
                                    dropdownMenu(
                                      type = "message",
                                      messageItem(from = "LUMSA", message = "Clicca qui per vedere il nostro corso!", icon("education", lib = "glyphicon"), href = "https://www.lumsa.it/didattica/corsi-di-laurea/roma/triennale/tecniche-informatiche-gestione-dati"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "sidebar",
                        menuItem("Dataset", tabName = "dataset", icon = icon("database")),
                        menuItem("Dendogrammi", tabName = "dendogrammi", icon = icon("tree-conifer", lib = "glyphicon")),
                        menuItem("Clustering", tabName = "clustering", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("K-means", tabName = "kmeans", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("PAM", tabName = "pam", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("PCA", tabName = "pca", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("Mclust", tabName = "mclust", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("Modelli", tabName = "modelli", icon = icon("chevron-right", lib = "glyphicon"),
                                 menuItem("8 variabili", tabName = "modelli8", icon = icon("triangle-right", lib = "glyphicon")),
                                 menuItem("7 variabili", tabName = "modelli7", icon = icon("triangle-right", lib = "glyphicon")),
                                 menuItem("6 variabili", tabName = "modelli6", icon = icon("triangle-right", lib = "glyphicon"))),
                        menuItem("Selezione variabili", tabName = "selection", icon = icon("chevron-right", lib = "glyphicon")),
                        menuItem("Conclusioni", tabName = "conclusions", icon = icon("screenshot", lib = "glyphicon"))
                      )
                    ),
                    dashboardBody(tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #FFFFFF;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #FFFFFF;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #008F39;
                                }        

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #008F39;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #2E8B57;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #008F39;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #FFFFFF;
                                }
                                /* toggle button when hovered  */                    
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #008F39;
                                }
                                '))),
                      tabItems(
                        tabItem(tabName = "dataset",
                                tabBox(id = "t1", width = 12,
                                       tabPanel("Info", icon = icon("info-sign", lib = "glyphicon"),
                                                #  rows are created by the fluidRow() function and include columns defined by the column() function
                                                fluidRow(
                                                  column(width = 8, tags$img(src="https://www.cure-naturali.it/.imaging/default/dam/cure-naturali/enciclopedia-naturale/alimentazione/acido-oleico.jpg/jcr:content.jpg", width = 600, height = 350),
                                                         br(),
                                                         tags$a("Foto dal sito cure-naturali.it", href = "https://www.cure-naturali.it/enciclopedia-naturale/alimentazione/nutrizione/acido-oleico-proprieta-benefici.html"), align = "center"),
                                                  column(width = 4,
                                                         box(status = "success", solidHeader = T,tags$h4("Applicazione web Shiny sull'analisi del dataset",
                                                                                                         tags$a("olive.", href = "https://search.r-project.org/CRAN/refmans/IMIFA/html/olive.html"),br(),
                                                                                                         "Questi dati sono tratti dall'articolo", br(),
                                                                                                         tags$a("'Classification of Olive Oils from their Fatty Acid Composition(1983)'", href = "https://www.researchgate.net/publication/239459050_Classification_of_olive_oils_from_their_fatty_acid_composition"), br(),
                                                                                                         "e possono essere visualizzati su R Studio installando il pacchetto",
                                                                                                         tags$a("IMIFA", href = "https://cran.r-project.org/web/packages/IMIFA/vignettes/IMIFA.html")
                                                         )))
                                                )
                                       ),
                                       tabPanel(title = "Dati", icon = icon("table"), dataTableOutput("dataT")),
                                       tabPanel(title = "Struttura", icon = icon("list-alt"), verbatimTextOutput("structure")),
                                       tabPanel(title = "Statistiche descrittive", icon = icon("chart-pie"), verbatimTextOutput("summary")),
                                       tabPanel(title = "Mappa", icon = icon("map"), value = "mappa",
                                                fluidRow(
                                                  box(width = 12, status = "success", solidHeader = T, withSpinner(leafletOutput(outputId = "mappa"))))),
                                       tabPanel(title = "Correlogramma", icon = icon("sort", lib = "glyphicon"), value = "correlogram",
                                                fluidRow(
                                                  box(width = 12, status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "correlogramma")))))
                                )
                        ),
                        tabItem(tabName = "dendogrammi",
                                tabBox(id = "t2", width = 12,
                                       tabPanel(title = "Euclidean distance", icon = icon("menu-down", lib = "glyphicon"), value = "dendogram1",
                                                fluidRow(
                                                  box(width = 3, div(style = "height:50px"),status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma1"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma2"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma3"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma4"))))),
                                       tabPanel(title = "Manhattan distance", icon = icon("menu-down", lib = "glyphicon"), value = "dendogram2",
                                                fluidRow(
                                                  box(width = 3, div(style = "height:50px"),status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma5"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma6"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma7"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma8"))))),
                                       tabPanel(title = "Minkowski distance", icon = icon("menu-down", lib = "glyphicon"), value = "dendogram3",
                                                fluidRow(
                                                  box(width = 3, div(style = "height:50px"),status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma9"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma10"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma11"))),
                                                  box(width = 3, div(style = "height:50px"), status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "dendogramma12"))))))
                        ),
                        tabItem(tabName = "clustering",
                                tabBox(id = "t3", width = 12,
                                       tabPanel(title = "NbClust", icon = icon("menu-down", lib = "glyphicon"), value = "nbclust",
                                                fluidRow(
                                                  column(6,tags$img(src="https://i.ibb.co/BB0C6Fg/Screenshot-158.png", height = 325, align = "center"), box(status = "success", solidHeader = T,tags$h4("Calinski-Harabasz(CH) Index", width=6)),verbatimTextOutput("ch"), align = "center"),
                                                  column(6,tags$img(src="https://i.ibb.co/nLX79Sy/NbPlot.jpg"))
                                                )),
                                       tabPanel(title = "Best Partition", icon = icon("menu-down", lib = "glyphicon"), value = "best",
                                                fluidRow(
                                                  box(width = 12, status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "quod", height = 600)))
                                                )),
                                       tabPanel(title = "Silhouette", icon = icon("menu-down", lib = "glyphicon"), value = "silhouette",
                                                fluidRow(
                                                  box(width = 12, status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "sil")))
                                                )),
                                       tabPanel(title = "WSS", icon = icon("menu-down", lib = "glyphicon"), value = "Wss",
                                                fluidRow(
                                                  box(width = 12, status = "success", solidHeader = T, withSpinner(plotOutput(outputId = "wss")))
                                                )))),
                        tabItem(tabName = "kmeans",
                                tabBox(id = "t4", width = 12,
                                       tabPanel(title = "Dati", icon = icon("table"), value = "data", DT::dataTableOutput("dati2"),
                                                ),
                                       tabPanel(title = "Matrici di confusione",icon = icon("th", lib = "glyphicon"), value = "mat1",
                                                fluidRow(
                                                  column(6, verbatimTextOutput("starmean"),withSpinner(plotOutput(outputId = "matrix1", width = 500))),
                                                  column(6,withSpinner(plotOutput(outputId = "matrix2", height = 610, width = 500)))
                                                )),
                                       tabPanel(title = "Grafici a barre",icon = icon("align-left", lib = "glyphicon"), value = "bar",
                                                fluidRow(
                                                  box(status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bar1", height = 610))),
                                                  box(status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bar2", height = 610)))
                                                ))
                                       )),
                        tabItem(tabName = "pam",
                                tabBox(id = "t5", width = 12,
                                       tabPanel(title = "Dati", icon = icon("table"), value = "data", DT::dataTableOutput("dati3"),
                                       ),
                                       tabPanel(title = "Matrici di confusione",icon = icon("th", lib = "glyphicon"), value = "mat1",
                                                fluidRow(
                                                  column(6, verbatimTextOutput("starpam"),withSpinner(plotOutput(outputId = "matrix3", width = 500))),
                                                  column(6,withSpinner(plotOutput(outputId = "matrix4", height = 610, width = 500)))
                                                )),
                                       tabPanel(title = "Grafici a barre",icon = icon("align-left", lib = "glyphicon"), value = "bar0",
                                                fluidRow(
                                                  box(status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bar3", height = 610))),
                                                  box(status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bar4", height = 610)))
                                                ))
                                )),
                        tabItem(tabName = "pca",
                                tabBox(id = "t6", width = 12,
                                       tabPanel(title = "Grafico delle variabili",icon = icon("fullscreen", lib = "glyphicon"), value = "pca_",
                                                fluidRow(
                                                  column(8,box(width = 12, status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "pca_summary")))),
                                                  column(4,box(width = 12, status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "pca_graph"))))
                                                )),
                                       tabPanel(title = "Dati", icon = icon("table"), value = "data", DT::dataTableOutput("dati4"),
                                       ),
                                       tabPanel(title = "Matrici di confusione",icon = icon("th", lib = "glyphicon"), value = "mat2",
                                                fluidRow(
                                                  column(6,withSpinner(plotOutput(outputId = "matrix5", height = 610, width = 500))),
                                                  column(6,withSpinner(plotOutput(outputId = "matrix6", height = 610, width = 500)))
                                                )),
                                       tabPanel(title = "Grafico dei cluster",icon = icon("dashboard", lib = "glyphicon"), value = "clus_p",
                                                fluidRow(
                                                  box(width=12, status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "clus_plot", height = 600)))
                                                ))
                                )),
                        tabItem(tabName = "mclust",
                                tabBox(id = "t7", width = 12,
                                       tabPanel(title = "BIC 5",icon = icon("menu-down", lib = "glyphicon"), value = "5bic",
                                                fluidRow(
                                                  column(6,box(width= 12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/ZxVgGZk/Screenshot-160.png", align= "center"))),
                                                  column(6,box(width= 12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/9TKR7GP/Screenshot-162.png", align = "center")))
                                                ),
                                                fluidRow(
                                                  column(6,box(width= 12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab1"))),
                                                         box(width= 12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab2")))),
                                                  column(6,box(width=12, status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bic5", width = 500))))
                                                )),
                                       tabPanel(title = "Uncertainty 5",icon = icon("menu-down", lib = "glyphicon"), value = "5un",
                                                fluidRow(
                                                  column(12,tags$img(src="https://i.ibb.co/yNwxbzG/Immagine-2022-11-14-003204.png", width = 800, align = "center"))
                                                )),
                                       tabPanel(title = "Classification 5",icon = icon("menu-down", lib = "glyphicon"), value = "5cl",
                                                fluidRow(
                                                  column(12,tags$img(src="https://i.ibb.co/Swd8jJd/Immagine-2022-11-14-161707.png", width = 800, align = "center"))
                                                )),
                                       tabPanel(title = "BIC 9",icon = icon("menu-down", lib = "glyphicon"), value = "9bic",
                                                fluidRow(
                                                  column(6,box(width=12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/ySSpvYx/Immagine-2022-11-13-225654.png", align= "center")),
                                                         box(width=12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab3"))),
                                                         box(width=12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab4")))),
                                                  column(6,box(width=12,status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bic9", height = 500, width = 500))))
                                                )),
                                       tabPanel(title = "Uncertainty 9",icon = icon("menu-down", lib = "glyphicon"), value = "9un",
                                                fluidRow(
                                                  column(12,tags$img(src="https://i.ibb.co/vQY57SH/Immagine-2022-11-13-230509.png", width = 800))
                                                )),
                                       tabPanel(title = "BIC NS",icon = icon("menu-down", lib = "glyphicon"), value = "nsbic",
                                                fluidRow(
                                                  column(6,box(width= 12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/mH43Zwk/Screenshot-172.png"))),
                                                  column(6,box(width= 12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/n8KK20z/Screenshot-173.png")))
                                                ),
                                                fluidRow(
                                                  column(6,box(width= 12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab01"))),
                                                         box(width= 12,status = "success", solidHeader = T,withSpinner(verbatimTextOutput(outputId = "tab02")))),
                                                  column(6,box(width=12, status = "success", solidHeader = T,withSpinner(plotOutput(outputId = "bicns", width = 500))))
                                                )),
                                       tabPanel(title = "Uncertainty NS",icon = icon("menu-down", lib = "glyphicon"), value = "nsun",
                                                fluidRow(
                                                  column(12, tags$img(src="https://i.ibb.co/5FMcBpb/Immagine-2022-11-14-164214.png",width=800, align = "center"))
                                                )),
                                       tabPanel(title = "Classification NS",icon = icon("menu-down", lib = "glyphicon"), value = "nscl",
                                                fluidRow(
                                                  column(12,tags$img(src="https://i.ibb.co/G09hFTy/Immagine-2022-11-14-163438.png", width = 800, align = "center"))
                                                ))
                                )),
                        tabItem(tabName = "modelli8",
                                tabBox(id = "t8", width = 12,
                                       tabPanel(title = "Teigen",icon = icon("menu-down", lib = "glyphicon"), value = "teigen1",
                                                fluidRow(
                                                  column(7,verbatimTextOutput(outputId = "print"),plotOutput(outputId = "tab6")),
                                                  column(5,plotOutput(outputId = "tab5", height = 475))
                                                )),
                                       tabPanel(title = "Teigen Plot",icon = icon("menu-down", lib = "glyphicon"), value = "teigen2",
                                                fluidRow(
                                                  column(6,tags$img(src="https://i.ibb.co/M2kZpwy/Immagine-2022-11-13-231731.png", width = 580)),
                                                  column(6,tags$img(src="https://i.ibb.co/K22qsbL/Immagine-2022-11-13-231915.png", width = 580))
                                                )),
                                       tabPanel(title = "Teigen NS",icon = icon("menu-down", lib = "glyphicon"), value = "teigen1ns",
                                                fluidRow(
                                                  column(7,verbatimTextOutput(outputId = "printns"),plotOutput(outputId = "tab6ns")),
                                                  column(5,plotOutput(outputId = "tab5ns", height = 475))
                                                )),
                                       tabPanel(title = "Teigen Plot NS",icon = icon("menu-down", lib = "glyphicon"), value = "teigen2ns",
                                                fluidRow(
                                                  column(6,tags$img(src="https://i.ibb.co/7gMF2XT/Immagine-2022-11-14-174008.png", width = 580)),
                                                  column(6,tags$img(src="https://i.ibb.co/JnJ09Yz/Immagine-2022-11-14-174030.png", width = 580))
                                                )),
                                       tabPanel(title = "Contaminated Summary",icon = icon("menu-down", lib = "glyphicon"), value = "contamin",
                                                fluidRow(
                                                  column(8,verbatimTextOutput(outputId = "sum")),
                                                  column(4,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab7")),box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab8")))
                                                )),
                                       tabPanel(title = "Contaminated Plot",icon = icon("menu-down", lib = "glyphicon"), value = "conta",
                                                fluidRow(
                                                  column(12,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tampon")))
                                                )),
                                       tabPanel(title = "Contaminated Summary NS",icon = icon("menu-down", lib = "glyphicon"), value = "contaminns",
                                                fluidRow(
                                                  column(8,verbatimTextOutput(outputId = "sumns")),
                                                  column(4,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab7ns")),box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab8ns")))
                                                )),
                                       tabPanel(title = "Sparse",icon = icon("menu-down", lib = "glyphicon"), value = "sparse",
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab9",height = 610, width = 500)),
                                                  column(6,plotOutput(outputId = "tab10",height = 610, width = 500))
                                                )),
                                       tabPanel(title = "Dim Reduction",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(6,box(width=12,status = "success", solidHeader = T,verbatimTextOutput(outputId = "sumdi1")),box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "plotdim1"))),
                                                  column(6,box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "tabdim1")),box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "tabdim2")))
                                                )),
                                       tabPanel(title = "Dim Reduction NS",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(6,box(width=12,status = "success", solidHeader = T,verbatimTextOutput(outputId = "sumdi2")),box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "plotdim2"))),
                                                  column(6,box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "tabdim3")),box(width=12,status = "success", solidHeader = T,plotOutput(outputId = "tabdim4")))
                                                ))
                                )),
                        tabItem(tabName = "modelli7",
                                tabBox(id = "t9", width = 12,
                                       tabPanel(title = "Teigen",icon = icon("menu-down", lib = "glyphicon"), value = "teigen17",
                                                fluidRow(
                                                  column(7,verbatimTextOutput(outputId = "print7"),plotOutput(outputId = "tab67")),
                                                  column(5,plotOutput(outputId = "tab57", height = 475))
                                                )),
                                       tabPanel(title = "Teigen Plot",icon = icon("menu-down", lib = "glyphicon"), value = "teigen27",
                                                fluidRow(
                                                  column(6,tags$img(src="https://i.ibb.co/YBtCqSs/Immagine-2022-11-15-160638.png", width = 580)),
                                                  column(6,tags$img(src="https://i.ibb.co/4j0Kvgz/Immagine-2022-11-15-160710.png", width = 580))
                                                )),
                                       tabPanel(title = "Contaminated Summary",icon = icon("menu-down", lib = "glyphicon"), value = "contamin7",
                                                fluidRow(
                                                  column(8,verbatimTextOutput(outputId = "sum7")),
                                                  column(4,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab77")),box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab87")))
                                                )),
                                       tabPanel(title = "Contaminated Plot",icon = icon("menu-down", lib = "glyphicon"), value = "conta7",
                                                fluidRow(
                                                  column(12,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tampon7")))
                                                ))
                                )),
                        tabItem(tabName = "modelli6",
                                tabBox(id = "t10", width = 12,
                                       tabPanel(title = "Teigen",icon = icon("menu-down", lib = "glyphicon"), value = "teigen16",
                                                fluidRow(
                                                  column(7,verbatimTextOutput(outputId = "print6"),plotOutput(outputId = "tab66")),
                                                  column(5,plotOutput(outputId = "tab56", height = 475))
                                                )),
                                       tabPanel(title = "Teigen Plot",icon = icon("menu-down", lib = "glyphicon"), value = "teigen26",
                                                fluidRow(
                                                  column(6,tags$img(src="https://i.ibb.co/829vMvP/Immagine-2022-11-15-181354.png", width = 580)),
                                                  column(6,tags$img(src="https://i.ibb.co/k6RRZct/Immagine-2022-11-15-181556.png", width = 580))
                                                )),
                                       tabPanel(title = "Contaminated Summary",icon = icon("menu-down", lib = "glyphicon"), value = "contamin6",
                                                fluidRow(
                                                  column(8,verbatimTextOutput(outputId = "sum6")),
                                                  column(4,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab76")),box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tab86")))
                                                )),
                                       tabPanel(title = "Contaminated Plot",icon = icon("menu-down", lib = "glyphicon"), value = "conta6",
                                                fluidRow(
                                                  column(12,box(width= 12,status = "success", solidHeader = T,plotOutput(outputId = "tampon6")))
                                                ))
                                )),
                        tabItem(tabName = "selection",
                                tabBox(id = "t11", width = 12,
                                       tabPanel(title = "VSCC",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(12,verbatimTextOutput(outputId="mus"))),
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab11")),
                                                  column(6,plotOutput(outputId = "tab12"))
                                                )),
                                       tabPanel(title = "VSCC NS",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(12,verbatimTextOutput(outputId="musns"))),
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab11ns")),
                                                  column(6,plotOutput(outputId = "tab12ns"))
                                                )),
                                       tabPanel(title = "Lasso",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(6,verbatimTextOutput(outputId="lasum"),plotOutput(outputId = "tab13")),
                                                  column(6,plotOutput(outputId = "tab14", height = 600))
                                                )),
                                       tabPanel(title = "Lasso NS",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(6,verbatimTextOutput(outputId="lasumns"),plotOutput(outputId = "tab13ns")),
                                                  column(6,plotOutput(outputId = "tab14ns", height = 600))
                                                )),
                                       tabPanel(title = "Clustvarsel",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(12,verbatimTextOutput(outputId="summa"))),
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab15")),
                                                  column(6,plotOutput(outputId = "tab16"))
                                                )),
                                       tabPanel(title = "Clustvarsel 6 NS",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(12,verbatimTextOutput(outputId="summans"))),
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab15ns")),
                                                  column(6,plotOutput(outputId = "tab16ns"))
                                                )),
                                       tabPanel(title = "Clustvarsel 7 NS",icon = icon("menu-down", lib = "glyphicon"), value = "",
                                                fluidRow(
                                                  column(12,verbatimTextOutput(outputId="summans7"))),
                                                fluidRow(
                                                  column(6,plotOutput(outputId = "tab15ns7")),
                                                  column(6,plotOutput(outputId = "tab16ns7"))
                                                ))
                                )),
                        tabItem(tabName = "conclusions",
                                tabBox(id = "t12", width = 12,
                                       fluidRow(
                                         column(6,h2("DATI SCALATI"),align="center",box(width=12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/zNhm0Pm/concccc1.png"))),
                                         column(6,h2("DATI NON SCALATI"),align="center",box(width=12,status = "success", solidHeader = T,tags$img(src="https://i.ibb.co/mGB23pD/conccc2.png")))
                                       ),
                                       fluidRow(h4("L'Adjusted Random Index, indicante la somiglianza della classificazione tra i nostri dati e i valori ottenuti tramite clustering, ci permette di valutare quale tra i modelli di clustering applicati sia il più preciso. Per il dataset con dati scalati confrontiamo sia i modelli di clustering model-based che quelli di clustering gerarchico. Per quanto riguarda l'area, otteniamo un risultato migliore dell'indice ARI con il modello Teigen; mentre, rispetto alle regioni,
                                                   il modello che clusterizza più similmente è il Kmeans-sparse seguito dal modello Teigen. Per il dataset con i dati non scalati abbiamo confrontato i modelli di model-based clustering al variare del numero di variabili selezionate. Per Region il risultato migliore lo otteniamo con il modello Contaminated sul dataset contenente 7 variabili ( escluso Oleic ); mentre, utilizzando ancora il dataset da 7 variabili, per Area il valore di somiglianza migliore lo riscontriamo con il modello Mbclust."), align = "center")
                                ))
                      )
                    )
)

# SERVER ----------------------------------------------------------------------
server <- function(input, output) {
  
  # DataTable
  output$dataT <- renderDataTable(
    olive %>%
      select(-c(11,12,13,14))
  )
  
  # Structure
  output$structure <- renderPrint({
    olive %>%
      select(-c(11,12,13,14)) %>%
      str()
  })
  
  # Summary
  output$summary <- renderPrint(
    olive %>%
      select(-c(1,2,11,12,13,14)) %>%
      summary()
  )
  
  # Map
  output$mappa <- renderLeaflet({
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
  })
  
  #Correlogramma
  output$correlogramma <- renderPlot(
    ggcorrplot(corr, hc.order = TRUE, 
               type = "lower", 
               lab = TRUE, 
               lab_size = 3, 
               method="square", 
               colors = c("yellow", "white", "green"), 
               title="Correlogramma di olive", 
               ggtheme=theme_bw)
  )
  
  output$dendogramma1 <- renderPlot(
    plot(hc.eu_single, main = "Single Method")
  )
  
  output$dendogramma2 <- renderPlot(
    plot(hc.eu_com, main = "Complete Method")
  )
  
  output$dendogramma3 <- renderPlot(
    plot(hc.eu_avg, main = "Average Method")
  )
  
  output$dendogramma4 <- renderPlot(
    plot(hc.eu_ward, main = "Ward Method")
  )
  
  output$dendogramma5 <- renderPlot(
    plot(hc.man_single, main = "Single Method")
  )
  
  output$dendogramma6 <- renderPlot(
    plot(hc.man_com, main = "Complete Method")
  )
  
  output$dendogramma7 <- renderPlot(
    plot(hc.man_avg, main = "Average Method")
  )
  
  output$dendogramma8 <- renderPlot(
    plot(hc.man_ward, main = "Ward Method")
  )
  
  output$dendogramma9 <- renderPlot(
    plot(hc.min_single, main = "Single Method")
  )
  
  output$dendogramma10 <- renderPlot(
    plot(hc.min_com, main = "Complete Method")
  )
  
  output$dendogramma11 <- renderPlot(
    plot(hc.min_avg, main = "Average Method")
  )
  
  output$dendogramma12 <- renderPlot(
    plot(hc.min_ward, main = "Ward Method")
  )
  
  output$ch <- renderPrint({
    clus$All.index[-c(5,8,11,12),][,2]
  })
  
  output$sil <- renderPlot(
    fviz_nbclust(olive.scaled,kmeans,method="silhouette")
  )
  
  output$wss <- renderPlot(
    fviz_nbclust(olive.scaled,kmeans,method="wss")
  )
  
  output$quod <- renderPlot(
    plot(olive.scaled, col=clus$Best.partition,main="Ward's minimum variance - Euclidean distance")
  )
  
  output$dati2 <- DT::renderDataTable(
    DT::datatable(kmeans_basic_df, options = list(scrollX = T))
  )
  
  output$starmean <- renderPrint(
    stargazer(k.olive$centers,type="text")
  )
  
  output$matrix1 <- renderPlot(
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
  )
  
  output$matrix2 <- renderPlot(
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
  )
  
  output$bar1 <- renderPlot(
    ggplot(data=kmeans_basic_df, aes(y=Cluster))+
      geom_bar(aes(fill=Region))+
      ggtitle("Count of clusters by Region ")+
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$bar2 <- renderPlot(
    ggplot(data=kmeans_basic_df, aes(y=Cluster))+
      geom_bar(aes(fill=Area))+
      ggtitle("Count of clusters by Area ")+
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  #
  output$dati3 <- DT::renderDataTable(
    DT::datatable(pam_basic_df, options = list(scrollX = T))
  )
  
  output$starpam <- renderPrint(
    stargazer(p.olive$medoids,type="text")
  )
  
  output$matrix3 <- renderPlot(
    table(pam_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "PAM Clusters", 
        y = "Region", 
        title = "Confusion matrix of PAM clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$matrix4 <- renderPlot(
    table(pam_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "PAM Clusters", 
        y = "Area", 
        title = "Confusion matrix of PAM clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$bar3 <- renderPlot(
    ggplot(data=pam_basic_df, aes(y=Cluster))+
      geom_bar(aes(fill=Region))+
      ggtitle("Count of clusters by Region ")+
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$bar4 <- renderPlot(
    ggplot(data=pam_basic_df, aes(y=Cluster))+
      geom_bar(aes(fill=Area))+
      ggtitle("Count of clusters by Area ")+
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  output$pca_summary <- renderPrint(
    summary.PCA(res.pca)
  )
  
  output$pca_graph <- renderPlot(
    PCA(olive.scaled, ncp = 5, graph = T)
  )
  
  output$dati4 <- DT::renderDataTable(
    DT::datatable(pca_basic_df, options = list(scrollX = T))
  )
  
  output$matrix5 <- renderPlot(
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
  )
  
  output$matrix6 <- renderPlot(
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
  )
  
  output$clus_plot <- renderPlot(
    fviz_cluster(res.hcpc,
                 repel = TRUE,            
                 show.clust.cent = TRUE,
                 palette = "jco",         
                 ggtheme = theme_minimal(),
                 main = "Factor map"
    )
  )
  
  output$bic5 <- renderPlot(
    fviz_mclust(clustering_5, "BIC", palette = "jco")
  )
  
  output$tab1 <- renderPrint(
    table(olive[,1],clustering_5$classification)
  )
  
  output$tab2 <- renderPrint(
    table(olive[,2],clustering_5$classification)
  )
  
  output$bic9 <- renderPlot(
    fviz_mclust(clustering_9, "BIC", palette = "jco")
  )
  
  output$tab3 <- renderPrint(
    table(olive[,1],clustering_9$classification)
  )
  
  output$tab4 <- renderPrint(
    table(olive[,2],clustering_9$classification)
  )
  
  output$print <- renderPrint(
    print(teign)
  )
  
  output$tab5 <- renderPlot(
    table(teigen_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Region", 
        title = "Confusion matrix of Teigen clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab6 <- renderPlot(
    table(teigen_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Area", 
        title = "Confusion matrix of Teigen clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$print7 <- renderPrint(
    print(teign7)
  )
  
  output$tab57 <- renderPlot(
    table(teigen7_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Region", 
        title = "Confusion matrix of Teigen clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab67 <- renderPlot(
    table(teigen7_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Area", 
        title = "Confusion matrix of Teigen clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$print6 <- renderPrint(
    print(teign7)
  )
  
  output$tab56 <- renderPlot(
    table(teigen6_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Region", 
        title = "Confusion matrix of Teigen clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab66 <- renderPlot(
    table(teigen6_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Teigen Clusters", 
        y = "Area", 
        title = "Confusion matrix of Teigen clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$printns <- renderPrint(
    print(teign_notsc)
  )
  
  output$tab5ns <- renderPlot(
    table(teigenns_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Teigen NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of Teigen NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab6ns <- renderPlot(
    table(teigenns_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Teigen NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of Teigen NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$sum <- renderPrint(
    summary(contam)
  )
  
  output$tab7 <- renderPlot(
    table(contam_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Region", 
        title = "Confusion matrix of Contaminated clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab8 <- renderPlot(
    table(contam_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Area", 
        title = "Confusion matrix of Contaminated clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$sum7 <- renderPrint(
    summary(contam7)
  )
  
  output$tab77 <- renderPlot(
    table(contam7_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Region", 
        title = "Confusion matrix of Contaminated clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab87 <- renderPlot(
    table(contam7_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Area", 
        title = "Confusion matrix of Contaminated clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$sum6 <- renderPrint(
    summary(contam6)
  )
  
  output$tab76 <- renderPlot(
    table(contam6_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Region", 
        title = "Confusion matrix of Contaminated clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab86 <- renderPlot(
    table(contam6_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Contaminated Clusters", 
        y = "Area", 
        title = "Confusion matrix of Contaminated clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$sumns <- renderPrint(
    summary(contam_notsc)
  )
  
  output$tab7ns <- renderPlot(
    table(contamns_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "Contaminated NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of Contaminated NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab8ns <- renderPlot(
    table(contamns_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Contaminated NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of Contaminated NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$tampon <- renderPlot(
    plot(contam,criterion = "BIC", contours = T, xmarg = 1, ymarg = 2,  
         res = 200, levels = seq(.0001,1,by=0.01))
  )
  
  output$tampon7 <- renderPlot(
    plot(contam7,criterion = "BIC", contours = T, xmarg = 1, ymarg = 2,  
         res = 200, levels = seq(.0001,1,by=0.01))
  )
  
  output$tampon6 <- renderPlot(
    plot(contam6,criterion = "BIC", contours = T, xmarg = 1, ymarg = 2,  
         res = 200, levels = seq(.0001,1,by=0.01))
  )
  
  output$tamponns <- renderPlot(
    plot(contam_notsc,criterion = "BIC", contours = T, xmarg = 1, ymarg = 2,  
         res = 200, levels = seq(.0001,1,by=0.01))
  )
  
  output$tab9 <- renderPlot(
    table(ksparse_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = "K-means sparse Clusters", 
        y = "Region", 
        title = "Confusion matrix of k-means sparse clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab10 <- renderPlot(
    table(ksparse_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "K-means Sparse Clusters", 
        y = "Area", 
        title = "Confusion matrix of k-means sparse clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$mus <- renderPrint(
    head(clust.vscc$topselected)
  )
  
  output$tab11 <- renderPlot(
    table(vscc_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " VSCC Clusters", 
        y = "Region", 
        title = "Confusion matrix of vscc clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab12 <- renderPlot(
    table(vscc_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "vscc Clusters", 
        y = "Area", 
        title = "Confusion matrix of vscc clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$musns <- renderPrint(
    head(clust.vsccns$topselected)
  )
  
  output$tab11ns <- renderPlot(
    table(vsccns_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " VSCC NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of vscc NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab12ns <- renderPlot(
    table(vsccns_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "vscc NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of vscc NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$lasum <- renderPrint(
    summary(lasso)
  )
  
  output$tab13 <- renderPlot(
    table(lasso_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " Lasso Clusters", 
        y = "Region", 
        title = "Confusion matrix of Lasso clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab14 <- renderPlot(
    table(lasso_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Lasso Clusters", 
        y = "Area", 
        title = "Confusion matrix of Lasso clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$lasumns <- renderPrint(
    summary(lassons)
  )
  
  output$tab13ns <- renderPlot(
    table(lassons_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " Lasso NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of Lasso NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab14ns <- renderPlot(
    table(lassons_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Lasso NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of Lasso NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$summa <- renderPrint(
    clust$subset
  )
  
  output$tab15 <- renderPlot(
    table(varsel_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " Clustvarsel Clusters", 
        y = "Region", 
        title = "Confusion matrix of Clustvarsel clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab16 <- renderPlot(
    table(varsel_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Clustvarsel Clusters", 
        y = "Area", 
        title = "Confusion matrix of Clustvarsel clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$summans <- renderPrint(
    clustns$subset
  )
  
  output$tab15ns <- renderPlot(
    table(varselns_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " Clustvarsel NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of Clustvarsel NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab16ns <- renderPlot(
    table(varselns_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Clustvarsel NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of Clustvarsel NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$summans7 <- renderPrint(
    clustns_$subset
  )
  
  output$tab15ns7 <- renderPlot(
    table(varselns7_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " Clustvarsel NS Clusters", 
        y = "Region", 
        title = "Confusion matrix of Clustvarsel NS clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tab16ns7 <- renderPlot(
    table(varselns7_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "Clustvarsel NS Clusters", 
        y = "Area", 
        title = "Confusion matrix of Clustvarsel NS clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$bicns <- renderPlot(
    fviz_mclust(clustering_5_notsc, "BIC", palette = "jco")
  )
  
  output$tab01 <- renderPrint(
    table(olive[,1],clustering_5_notsc$classification)
  )
  
  output$tab02 <- renderPrint(
    table(olive[,2],clustering_5_notsc$classification)
  )
  
  output$sumdi1 <- renderPrint(
    summary(drmod)
  )
  
  output$sumdi2 <- renderPrint(
    summary(drmod_notsc)
  )
  
  output$plotdim1 <- renderPlot(
    plot(drmod,what="contour")
  )
  
  output$plotdim2 <- renderPlot(
    plot(drmod_notsc,what="contour")
  )
  
  output$tabdim1 <- renderPlot(
    table(drmod_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " DimR Clusters", 
        y = "Region", 
        title = "Confusion matrix of Dim Reduct clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tabdim2 <- renderPlot(
    table(drmod_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "DimR Clusters", 
        y = "Area", 
        title = "Confusion matrix of Dim Reduct clusters and Area of olive oil samples") +
      theme_light()
  )
  
  output$tabdim3 <- renderPlot(
    table(drmodnsc_basic_df %>% dplyr::select(Region,Cluster)) %>% 
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
        x = " DimR Clusters", 
        y = "Region", 
        title = "Confusion matrix of Dim Reduct clusters and Region of olive oil samples") +
      theme_light()
  )
  
  output$tabdim4 <- renderPlot(
    table(drmodnsc_basic_df %>% dplyr::select(Area,Cluster)) %>% 
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
        x = "DimR Clusters", 
        y = "Area", 
        title = "Confusion matrix of Dim Reduct clusters and Area of olive oil samples") +
      theme_light()
  )
  
}

shinyApp(ui = ui, server = server)
