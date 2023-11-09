########### CODICE TESINA ###########
str(bfi)
summary(bfi)

########### PULIZIA DATASET #########
library(plotly)

sum(is.na(bfi))
bfi.cleaned <- na.omit(bfi)

bfi.cleaned.s1 <- bfi.cleaned %>% filter(bfi.cleaned$education == 1 & bfi.cleaned$age > 14)
bfi.cleaned.s2 <- bfi.cleaned %>% filter(bfi.cleaned$education == 2 & bfi.cleaned$age > 17)
bfi.cleaned.s3 <- bfi.cleaned %>% filter(bfi.cleaned$education == 3 & bfi.cleaned$age > 17)
bfi.cleaned.s4 <- bfi.cleaned %>% filter(bfi.cleaned$education == 4 & bfi.cleaned$age > 21)
bfi.cleaned.s5 <- bfi.cleaned %>% filter(bfi.cleaned$education == 5 & bfi.cleaned$age > 23)

df_list <- list(bfi.cleaned.s1, bfi.cleaned.s2, bfi.cleaned.s3, bfi.cleaned.s4, bfi.cleaned.s5)      

bfi.cleaned <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  

########### ISTOGRAMMI ##############
library(tidyr)

bfi.cleaned %>%
  select(A1, A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3, N4, N5, O1, O2, O3, O4, O5) %>% 
  gather(key = variable, value = response) %>% 
  ggplot(mapping = aes(x = response)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(facets = ~ variable)

########### COPPIE DI GRAFICI A BARRE BASATI SU DUE GRUPPI ##########
library(psych)

par(mfrow=c(1,2))
bi.bars(bfi.cleaned,"age","gender" ,xlab="Gender",ylab="Age",main="Age by males and females")
bi.bars(bfi.cleaned,"education","gender",xlab="Education",ylab="Gender",
        main="Education by gender",horiz=FALSE)

########### CORRELOGRAMMA #################################
par(mfrow=c(1,1))

bfi.cleaned %>%
  select(A1, A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3, N4, N5, O1, O2, O3, O4, O5) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  round(1) %>% 
  as.data.frame() %>% 
  mutate(item1 = rownames(.)) %>% 
  gather(key = item2, value = r, -item1) %>% 
  ggplot(mapping = aes(x = item1, y = item2, fill = r, label = r)) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient2(low='#B3000C', mid='white', high='#00B32C') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#library(ggcorrplot)
#corr <- round(cor(bfi.cleaned[1:25]), 1)
#ggcorrplot(corr, hc.order = TRUE, 
#           type = "full", 
#           lab = TRUE, 
#           lab_size = 3, 
#           method="square", 
#           colors = c("#B3000C", "white", "#00B32C"), 
#           title="Correlogram of bfi", 
#           ggtheme=theme_bw)

############ NETWORK ################################################
library("qgraph")

CorMat <- cor_auto(bfi.cleaned[,1:25])
Names <- c( "Indifferent to the feelings of others.", "Inquire about others' well-being", "Know how to comfort others", "Love children", "Make people feel at ease",
            "Exacting in my work", "Continue until everything is perfect", "Do things according to a plan", "Do things in a half-way manner", "Waste my time",
            "Don't talk a lot", "Find it difficult to approach others", "Know how to captivate people", "Make friends easily", "Take charge",
            "Get angry easily", "Get irritated easily", "Have frequent mood swings", "Often feel blue", "Panic easily",
            "Full of ideas", "Avoid difficult reading materials", "Carry the conversation to a higher level", "Spend time reflecting on things", "Will not probe deeply into a subject" )   

Groups <- rep(c(
  'Agreeableness',
  'Conscientiousness',
  'Extraversion',
  'Neuroticism',
  'Opennness'),each=5)

qgraph(CorMat, graph = "glasso", sampleSize = nrow(bfi), groups = Groups,
       nodeNames = Names, layout = "spring", legend.mode = "style1",
       legend.cex = 0.35)

########### PARALLEL ANALYSIS #######################################
bfi.cleaned %>%
  select(A1, A2, A3, A4, A5, C1, C2, C3, C4, C5, E1, E2, E3, E4, E5, N1, N2, N3, N4, N5, O1, O2, O3, O4, O5) %>% 
  fa.parallel(fm = "minres", fa = "both")

########### PCA #####################################################
library(FactoMineR)
library(factoextra)
library(plyr)

bfi.cleaned.new <- bfi.cleaned

bfi.cleaned.new$gender <- revalue(x = as.factor(bfi.cleaned$gender),
                              replace = c("1" = "Males",
                                          "2" = "Females"))

bfi.cleaned.new$education <- revalue(x = as.factor(bfi.cleaned$education),
                                 replace = c("1" = "HS", 
                                             "2" = "finished HS", 
                                             "3" = "some college",
                                             "4" = "college graduate",
                                             "5" = "graduate degree"))

bfi.cleaned.new$age_range <- bfi.cleaned$age
bfi.cleaned.new.s1 <- bfi.cleaned.new %>%
  filter(bfi.cleaned.new$age >= 15 & bfi.cleaned.new$age <= 25) %>%
  mutate(age_range = "[15-25]")
bfi.cleaned.new.s2 <- bfi.cleaned.new %>%
  filter(bfi.cleaned.new$age > 25 & bfi.cleaned.new$age <= 35) %>%
  mutate(age_range = "(25-35]")
bfi.cleaned.new.s3 <- bfi.cleaned.new %>%
  filter(bfi.cleaned.new$age > 35 & bfi.cleaned.new$age <= 45) %>%
  mutate(age_range = "(35-45]")
bfi.cleaned.new.s4 <- bfi.cleaned.new %>%
  filter(bfi.cleaned.new$age > 45 & bfi.cleaned.new$age <= 55) %>%
  mutate(age_range = "(45-55]")
bfi.cleaned.new.s5 <- bfi.cleaned.new %>%
  filter(bfi.cleaned.new$age > 55 & bfi.cleaned.new$age <= 90) %>%
  mutate(age_range = "(55-90]")

df_list.new <- list(bfi.cleaned.new.s1, bfi.cleaned.new.s2, bfi.cleaned.new.s3, bfi.cleaned.new.s4, bfi.cleaned.new.s5)      

bfi.cleaned.new <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list.new)  

str(bfi.cleaned.new)

res.pca <- PCA(bfi.cleaned.new, ncp = 5, graph = T, quali.sup = c(26:27,29), quanti.sup = 28)
plot(res.pca, cex=0.8, invisible="ind",title="Individuals PCA graph")
summary(res.pca, nbelements=Inf)

########### CLUSTERING ####################################################
res.HCPC<-HCPC(res.pca,nb.clust = -1,consol=FALSE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')

set.seed(1234)
esempio <- kmeans(res.pca$var$coord, centers = 3)
gruppi <- as.factor(esempio$cluster)
fviz_pca_var(res.pca, col.var = gruppi)

bfi.scaled <- scale(bfi.cleaned[-c(26,27)])

library(NbClust)
clus <- NbClust(bfi.scaled, method = "kmeans", index = "all")
clus$All.index

esempio2 <- kmeans(res.pca$var$coord, centers = 2)
gruppi2 <- as.factor(esempio2$cluster)
fviz_pca_var(res.pca, col.var = gruppi2)

#fviz_nbclust(bfi.scaled,kmeans,method="silhouette")
#fviz_nbclust(bfi.scaled,kmeans,method="wss")

########### FACTOR ANALYSIS ##########################################
KMO(bfi.cleaned[1:25])

##Confirmatory
#library('lavaan')

#model <- '
#  O =~ O1 + O2 + O3 + O4 + O5
#  C =~ C1 + C2 + C3 + C4 + C5
#  E =~ E1 + E2 + E3 + E4 + E5
#  A =~ A1 + A2 + A3 + A4 + A5
#  N =~ N1 + N2 + N3 + N4 + N5
#' 

#fit <- cfa(model, bfi.cleaned)
#summary(fit, fit.measures = TRUE) #CFI vicino a 0.8

##Exploratory
factordata6 <- fa(bfi.cleaned[1:25], 6)
print(factordata6$loadings, cutoff=0.3)
fa.diagram(factordata6) 

factordata5 <- fa(bfi.cleaned[1:25], 5)
print(factordata5$loadings, cutoff=0.3)
fa.diagram(factordata5) 

#library(parameters)
#fa(bfi.cleaned[1:25], nfactors = 5) %>% 
#  model_parameters(sort = TRUE, threshold = "max")
