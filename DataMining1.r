########################## LIBRERIE UTILIZZATE #################################

library(lme4)
library(lcsm)
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse) 
library(RColorBrewer) 
library(lmerTest)
library(ggplot2)
library(stargazer)
library(gamlss)
library(Hmisc)
library(ggcorrplot)
library(plm)
library(plotly)

############################ DESCRIZIONE #######################################

# carichiamo i dataset proposti
load("RegressionData.RData")
# scegliamo il dataset "wagepan"
View(wagepan)
# attach consente di accedere alle variabili di un data.frame senza richiamare  il data.frame
attach(wagepan)
#detach(wagepan)
# head ritorna le prime sei righe del data.frame
head(wagepan)
# summary() può essere utilizzata per riassumere rapidamente i valori del data.frame 
summary(wagepan)
# str mostra la struttura interna del data.frame
str(wagepan)
# creiamo un sottoinsieme del data.frame selezionando le variabili d'interesse
selection <- wagepan %>% dplyr::select(lwage,educ,exper,expersq)
# vedi riga 27
head(selection)
# vedi riga 29
summary(selection)
# descrivere viene utilizzata per ottenere un riepilogo statistico descrittivo di 
# un determinato data.frame in questo caso un riepilogo delle colonne "educ" ed "exper"
describe(educ)
describe(exper)
# table() viene utilizzata per creare una rappresentazione categoriale dei dati 
# con il nome della variabile e la frequenza sotto forma di tabella
table(black)
table(hisp)
table(union)

############################ BOXPLOTS ###########################################

hisp_box <- plot_ly(x = ~hisp, y = ~lwage, type = "box", name = 'hisp_box') %>%
  layout(title = 'Logarithmic wage for Hispanic(1) and non-Hispanic(0) people')

black_box <- plot_ly(x = ~black, y = ~lwage, type = "box", name = 'black_box') %>%
  layout(title = 'Logarithmic wage for black(1) and non-black(0) people')

subplot(black_box, hisp_box) %>% 
  layout(title = 'Compare logarithmic wage for black(1) and non-black(0) people
         & for Hispanic(1) and non-Hispanic(0) people')

#union_box
plot_ly(x = ~union, y = ~lwage, type = "box", boxpoints = "all",  jitter = 0.2) %>% 
  layout(title = 'Logarithmic wage for people belonging to a union(1) and not(0)')

#married_box
plot_ly(x = ~married, y = ~lwage, type = "box", boxpoints = "all",  jitter = 0.2) %>% 
layout(title = 'Logarithmic wage for married(1) people and not(0)')

#rur_box <- plot_ly(x = ~rur, y = ~lwage, type = "box", boxpoints = "all",  jitter = 0.2) %>% 
#  layout(title = 'Logarithmic wage for people living in rural area and not')

############################ STACKED BAR CHARTS ################################

new_wagepan <- wagepan[!duplicated(wagepan$nr),]
selection2 <- new_wagepan %>% dplyr::select(nr,union,black,hisp,married,poorhlth,south,rur)
black2 <- selection2$black
union2 <- selection2$union
hisp2 <- selection2$hisp
married2 <- selection2$married
poorhlth2 <- selection2$poorhlth
south2 <- selection2$south
rur2 <- selection2$rur

ggplot(selection2, aes(black2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack", color="black") +
  ggtitle("Stacked Bar Chart about black(1) and non-black(0) people 
          who are(1) and are not(0) members of a union") +
  xlab("black") + scale_fill_discrete(name = "union")

ggplot(selection2, aes(hisp2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack", color="orange") +
  ggtitle("Stacked Bar Chart about Hispanic(1) and non-Hispanic(0) people 
          who are(1) and are not(0) members of a union") +
  xlab("hisp") + scale_fill_discrete(name = "union")

ggplot(selection2, aes(married2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack", color = "green") +
  ggtitle("Stacked Bar Chart about married(1) and not married(0) people 
          who are(1) and are not(0) members of a union") +
  xlab("married") + scale_fill_discrete(name = "union")

ggplot(selection2, aes(poorhlth2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack") +
  ggtitle("Stacked Bar Chart about people in poor health(1) and not(0) 
          who are(1) and are not(0) members of a union") +
  xlab("poorhlth") + scale_fill_discrete(name = "union")

ggplot(selection2, aes(south2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack", color = "red") +
  ggtitle("Stacked Bar Chart about southern people(1) and not(0) 
          who are(1) and are not(0) members of a union") +
  xlab("south") + scale_fill_discrete(name = "union")

ggplot(selection2, aes(rur2, ..count..)) + geom_bar(aes(fill = as.factor(union2)), position = "stack", color="purple") +
  ggtitle("Stacked Bar Chart about poeple living in a rural area(1) and not(0) 
          who are(1) and are not(0) members of a union") +
  xlab("rur") + scale_fill_discrete(name = "union")

table(union2&black2)
table(black2)
table(poorhlth2&union2)

######################## SCATTERPLOT ###########################################

# aggiungiamo linee di regressione a degli scatterplot
# fitted è una funzione generica che estrae i valori adattati(fitted) 
# dagli oggetti restituiti dalle funzioni di modellazione
wagepan %>% 
  plot_ly(x = ~educ) %>% 
  add_markers(y = ~lwage) %>% 
  add_lines(x = ~educ, y = fitted(lm(lwage ~ educ, data = wagepan))) %>%
  layout(title = "Logarithmic wage and education regression model")
# Controllare ###
wagepan %>% 
  plot_ly(x = ~factor(year)) %>% 
  add_markers(y = ~lwage) %>% 
  add_lines(x = ~factor(year), y = fitted(lm(lwage ~ factor(year), data = wagepan))) %>%
  layout(title = "Logarithmic wage and years regression model")
###############
wagepan %>% 
  plot_ly(x = ~exper) %>% 
  add_markers(y = ~lwage) %>% 
  add_lines(x = ~exper, y = fitted(lm(lwage ~ exper, data = wagepan))) %>%
  layout(title = "Logarithmic wage and experience regression model")

wagepan %>% 
  plot_ly(x = ~hours) %>% 
  add_markers(y = ~lwage) %>% 
  add_lines(x = ~hours, y = fitted(lm(lwage ~ hours, data = wagepan))) %>%
  layout(title = "Logarithmic wage and hours regression model")

##################################

ggplot(data=wagepan, aes(x=educ, y=lwage, col=as.factor(year)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Education and Logarithmic Wage over the 7 Years", col= "year")

ggplot(data=wagepan, aes(x=exper, y=lwage, col=as.factor(year)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Experience and Logarithmic Wage over the 7 Years", col= "year")

ggplot(data=wagepan, aes(x=exper, y=lwage, col=as.factor(educ)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Experience and Logarithmic Wage for the different levels/years of education", col= "educ")

ggplot(data=wagepan, aes(x=educ, y=lwage, col=as.factor(exper)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Experience and Logarithmic Wage for the different levels/years of experience", col= "exper")

ggplot(data=wagepan, aes(x=exper, y=lwage, col=as.factor(union)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Experience and Logarithmic Wage for Union members and not", col= "union")

ggplot(data=wagepan, aes(x=educ, y=lwage, col=as.factor(union)))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size=.7, alpha=.8, position = "jitter")+
  geom_smooth(method="lm",se=FALSE, size=2, alpha=.8)+theme_minimal()+
  labs(title="Linear Relationship Between Experience and Logarithmic Wage for Union members and not", col= "union")

###################### CORRELAZIONE ############################################

# creiamo un correlogramma tra le variabili d'interesse
corr <- round(cor(selection), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of selection (from wagepan)", 
           ggtheme=theme_bw)

# test di correlazione
cor.test(lwage, exper)
cor.test(lwage, educ)

cor.test(wagepan$lwage,wagepan$year)

######################## DISTRIBUZIONI SEMPLICI ################################

# modello lineare a effetti misti non generalizzato, utilizzando come variabile di raggruppamento nr 
# per prima cosa stimiamo i coefficienti in caso di intercetta con effetti casuali dovuta ai vari indivdui
mod_year <- lmer(lwage~1+educ+exper+expersq+union+black+hisp+married+(1|nr),data = wagepan,REML = FALSE)
summary(mod_year)

# valutiamo quali variabili comportano effetti random sul coefficiente angolare della retta di fit non solo 
# dovuti al variare di nr
mod_exper <- lmer(lwage~1+educ+exper+expersq+union+black+hisp+married+(1+exper|nr),data = wagepan,REML = FALSE)
summary(mod_exper)
# con il Ranova testa controlliamo se l'effetto della variabile exper è significativo sul coeff.angolare 
# della retta
ranova(mod_exper)
mod_ed_exp <- lmer(lwage~1+educ+exper+expersq+union+black+hisp+married+(1+educ+exper|nr),data = wagepan,REML = FALSE)
summary(mod_ed_exp)
ranova(mod_ed_exp)

histDist(wagepan$lwage, family = ST1)

# facciamo uno Shapiro-Test per verificare che la nostra distribuzione non sia una normale 
shapiro.test(wagepan$lwage)

t.test(wagepan$lwage)

# La distribuzione che meglio approssima i nostri dati Ã¨ una distribuzione di tipo Skew t si può verificare
# analizzando i valori di skewness(prossimo a 0) e kurtosis(prossimo a 3). 
# mentre la distribuzione dati con variabile dipendente union(binaria) è di tipo binomiale

mod_0 <- gamlss(lwage ~ factor(year)+black+union+educ+exper+married+expersq,data=wagepan,family = ST5)
summary(mod_0)
# si può vedere attraverso il qqplot e il density plot che la famiglia della distribuzione è corretta.  
plot(mod_0)

################################################################################

# modello lineare generalizzato per verificare l'andamento del lwage in funzione del tempo
# andamento che rispetta il grafico fatto precedentemente
mod_year <- gamlss(lwage ~ d81+d82+d83+d84+d85+d86+d87,data=wagepan,family=ST1)
summary(mod_year)
plot(mod_year)

# modello per verificare le differenze nel lwage dovute all'impiego svolto e in base all'anno
mod_lm_j <- gamlss(lwage ~ fin+ent+construc+agric+bus+factor(year)+hisp+black+union+exper+expersq+educ+married,data=wagepan,family = ST1)
summary(mod_lm_j)
plot(mod_lm_j)

# Modello lineare che utilizzeremo poi per confrontare i risultati con i modelli plm, le variabili "pub" e "hisp"
# non sono significative come si evince dal summary.
mod_lm <- lm(lwage ~ exper+expersq+educ+black+hisp+union+married+pub,data=wagepan)
summary(mod_lm)
plot(mod_lm)

############################# PLM ##############################################

# Creo un nuovo dataset indicando quali sono le due variabili che rendono il dataset wagepan un dataset panel
# ovvero la variabile di raggruppamento "nr" e quella temporale "year", in modo da poter usare il dataset 
# nelle funzioni "plm".
wagepan.p <- pdata.frame(wagepan,index=c("nr","year"))
head(wagepan.p)
summary(wagepan.p)

# Effetti Fixed
# utilizza la varianza delle variabili che cambiano nel tempo per stimare i coefficienti
model_fix <- plm(lwage ~ hisp+black+union+exper+expersq+educ+married+pub,data=wagepan.p, model="within")
stargazer(model_fix,type="text")
# summary(model_fix)

# Effetti Random
# stima i coefficienti del modello di regressione tenendo conto degli effetti dovuti alle altre variabili
# come "exper" e "expersq"
model_rand <- plm(lwage ~ hisp+black+union+exper+expersq+educ+married+pub,data=wagepan.p, model="random")
stargazer(model_rand,type="text")
# summary(model_rand)

# Effetti Between
# Utilizza le medie delle variabili per stimare i coefficienti, si vede l'utilizzo delle medie anche nel 
# summary infatti a numero di osservazioni compare 545 invece di 4360.
model_bet <- plm(lwage ~ hisp+black+union+exper+expersq+educ+married+pub,data=wagepan.p, model="between")
stargazer(model_bet,type="text")
# summary(model_bet)

# First Difference 
# stima i coefficienti del modello di regressione sottraendo alle variabili che cambiano nel tempo il loro 
# valore antecedente.
model_fd <- plm(lwage ~ hisp+black+union+exper+expersq+educ+married+pub,data=wagepan.p, model="fd")
stargazer(model_fd,type="text")
# summary(model_fd)

# hausman test: facciamo un test per verificare se il modello da considerare è quello RANDOM o WITHIN,
# in caso di un p-value < 0.05 il modello migliore è quello ad effetti fissi , se >= 0.05 quello Random.
# In questo caso il valore è minore di 0.05 perciò si preferisce il modello a effetti fissi.
phtest(model_fix,model_rand)

# Utilizzando la libreria stargazer stampiamo a schermo i risultati ottenuti utilizzando i diversi modelli,
# "between","within", "Rand" e "fd", il valore del coefficiente di autodeterminazione R^2 ci dice che il 
# modello migliore per i nostri dati è quello Between 
stargazer(model_fix,model_rand,model_bet,mod_lm,type="text")

#results <- round(
#  data.frame("Between Effects" = model_bet$coefficients[2:8],
#             "Random Effects" = model_rand$coeff[2:8],
#             "Fixed Effects" = c(NA, NA, NA, NA, model_fix$coeff[2:4]),
#             "FD" = model_fd$coeff[2:8]             
#             ),3)
#results

#plmtest(model_bet, type = "bp")
#qqnorm(resid(model_bet))
#qqline(resid(model_bet))

########################### ODDS RATIO #########################################

#logre<-gamlss(union ~ black+hisp+married+poorhlth+south+rur, family =BI, data = wagepan)
#summary(logre)
#plot(logre)
#library(oddsratio)
#coef(logre)
#exp(coef(logre))

# Utilizziamo un modello generalizzato ad effetti misti sulla variabile dipendente union per determinare 
# di quanto alcune caratteristiche dell'individuo influenzano la sua presenza o meno all'interno di una union

loBi<-glmer(union ~ 1+black+hisp+married+poorhlth+south+rur+(1|nr), family=binomial, data = wagepan,nAGQ = 11)
summary(loBi)

se <- sqrt(diag(vcov(loBi)))
# stimiamo le ODDS Ratio per le variabili del modello utilizzando la funzione esponenziale
# e le inseriamo in una tabella con i limiti di confidenza del 95%

tab <- cbind(Est = fixef(loBi), LL = fixef(loBi) - 1.96 * se, UL = fixef(loBi) + 1.96 * se)
print(exp(tab), digits=3)

# I risultati ottenuti stimano di quanto la probabilità di essere o meno all'interno di una union varia a 
# seconda delle caratteristiche di ogni individuo ad esempio l'essere Black comporta un notevole aumento 
# della probabilità pari circa 6.7 volte 

logre<-lm(union ~ d81+d82+d83+d84+d85+d86+d87, data = wagepan)
summary(logre)
plot(logre)
