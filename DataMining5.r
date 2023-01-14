# Load packages
library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(plotly)
library(xts)
require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(tsibble)  #tsibble for time series based on tidy principles
require(fable)  #for forecasting based on tidy principles
require(ggfortify)  #for plotting timeseries
require(forecast)  #for forecast function
require(tseries)
require(chron)
require(lubridate)
require(directlabels)
require(zoo)
require(lmtest)
require(TTR)  #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)
library(ggplot2)
library(gamlss)
library(FinTS)
library(plyr)
library(mhsmm)
library(hrbrthemes)
library(tsapp)


StockReturns$Date <- format(as.POSIXct(StockReturns$Date,
                                       format = '%d/%m/%Y'),
                            format = '%Y-%m-%d')

Stock <- xts(StockReturns[,-1], order.by=as.Date(as.character(StockReturns$Date), format='%Y-%m-%d'))
date <- as.Date(StockReturns[,1])
year <- as.numeric(format(date,"%Y"))
StockReturns1<- cbind(StockReturns,year)

stock_means<- ddply(StockReturns, .(year), summarize,  SP500=mean(SP500), NASDAQ=mean(NASDAQ), FTSE=mean(FTSE),DAC=mean(DAC),CAC=mean(CAC),ESTX50=mean(ESTX50))

modStock <- VAR(Stock,p=5,type="both")

modStockUsa <- VAR(Stock[,1:2],p=8,type="both")

modStockEU <- VAR(Stock[,c(-1,-2)],p=5,type="both")

serSto <- serial.test(modStock, lags.pt = 16, type = "PT.asymptotic") 

normSto <- normality.test(modStock)

archSto <- arch.test(modStock, lags.multi = 5)

serStoUsa <- serial.test(modStockUsa, lags.pt = 16, type = "PT.asymptotic") 

normStoUsa <- normality.test(modStockUsa)

archStoUsa <- arch.test(modStockUsa, lags.multi = 5)

serStoEu <- serial.test(modStockEU, lags.pt = 16, type = "PT.asymptotic") 

normStoEu <- normality.test(modStockEU)

archStoEu <- arch.test(modStockEU, lags.multi = 5)

Stock_ret <- StockReturns[,c(-1,-8)]
kmeans_mvt <- kmeans(StockReturns[,c(-1,-8)],centers = 2)
kmeans_mvt3 <- kmeans(StockReturns[,c(-1,-8)],centers = 3)

kmeans_2sp <- kmeans(StockReturns[,2],centers = 2)
var_sp2 <- cbind(var(StockReturns[kmeans_2sp$cluster==1,2]),var(StockReturns[kmeans_2sp$cluster==2,2]))
kmeans_2nas <- kmeans(StockReturns[,3],centers = 2)
var_2nas <- cbind(var(StockReturns[kmeans_2nas$cluster==1,3]),var(StockReturns[kmeans_2nas$cluster==2,3]))
kmeans_2fts <- kmeans(StockReturns[,4],centers = 2)
var_2fts<- cbind(var(StockReturns[kmeans_2fts$cluster==1,4]),var(StockReturns[kmeans_2fts$cluster==2,4]))
kmeans_2dac<- kmeans(StockReturns[,5],centers = 2)
var_2dac <- cbind(var(StockReturns[kmeans_2dac$cluster==1,5]),var(StockReturns[kmeans_2dac$cluster==2,5]))
kmeans_2cac <- kmeans(StockReturns[,6],centers = 2)
var_2cac <- cbind(var(StockReturns[kmeans_2cac$cluster==1,6]),var(StockReturns[kmeans_2cac$cluster==2,6]))
kmeans_2est <- kmeans(StockReturns[,7],centers = 2)
var_2est <- cbind(var(StockReturns[kmeans_2est$cluster==1,7]),var(StockReturns[kmeans_2est$cluster==2,7]))


kmeans_3sp <- kmeans(StockReturns[,2],centers = 3)
var_sp3 <- cbind(var(StockReturns[kmeans_3sp$cluster==1,2]),var(StockReturns[kmeans_3sp$cluster==2,2]),var(StockReturns[kmeans_3sp$cluster==3,2]))
kmeans_3nas <- kmeans(StockReturns[,3],centers = 3)
var_nas3 <- cbind(var(StockReturns[kmeans_3nas$cluster==1,3]),var(StockReturns[kmeans_3nas$cluster==2,3]),var(StockReturns[kmeans_3nas$cluster==3,3]))
kmeans_3fts <- kmeans(StockReturns[,4],centers = 3)
var_fts3 <- cbind(var(StockReturns[kmeans_3fts$cluster==1,4]),var(StockReturns[kmeans_3fts$cluster==2,4]),var(StockReturns[kmeans_3fts$cluster==3,4]))
kmeans_3dac<- kmeans(StockReturns[,5],centers = 3)
var_dac3 <- cbind(var(StockReturns[kmeans_3dac$cluster==1,5]),var(StockReturns[kmeans_3dac$cluster==2,5]),var(StockReturns[kmeans_3dac$cluster==3,5]))
kmeans_3cac <- kmeans(StockReturns[,6],centers = 3)
var_cac3 <- cbind(var(StockReturns[kmeans_3cac$cluster==1,6]),var(StockReturns[kmeans_3cac$cluster==2,6]),var(StockReturns[kmeans_3cac$cluster==3,6]))
kmeans_3est <- kmeans(StockReturns[,7],centers = 3)
var_est3 <- cbind(var(StockReturns[kmeans_3est$cluster==1,7]),var(StockReturns[kmeans_3est$cluster==2,7]),var(StockReturns[kmeans_3est$cluster==3,7]))


kmeans4_sp <- kmeans(StockReturns[,2],centers = 4)
var_sp4<- cbind(var(StockReturns[kmeans4_sp$cluster==1,2]),var(StockReturns[kmeans4_sp$cluster==2,2]),var(StockReturns[kmeans4_sp$cluster==3,2]),var(StockReturns[kmeans4_sp$cluster==4,2]))
kmeans4_fts <- kmeans(StockReturns[,4],centers = 4)
var_fts4<- cbind(var(StockReturns[kmeans4_fts$cluster==1,3]),var(StockReturns[kmeans4_fts$cluster==2,3]),var(StockReturns[kmeans4_fts$cluster==3,3]),var(StockReturns[kmeans4_fts$cluster==4,3]))
kmeans4_nas <- kmeans(StockReturns[,3],centers = 4)
var_nas4<- cbind(var(StockReturns[kmeans4_nas$cluster==1,4]),var(StockReturns[kmeans4_nas$cluster==2,4]),var(StockReturns[kmeans4_nas$cluster==3,4]),var(StockReturns[kmeans4_nas$cluster==4,4]))
kmeans4_dac <- kmeans(StockReturns[,5],centers = 4)
var_dac4<- cbind(var(StockReturns[kmeans4_dac$cluster==1,5]),var(StockReturns[kmeans4_dac$cluster==2,5]),var(StockReturns[kmeans4_dac$cluster==3,5]),var(StockReturns[kmeans4_dac$cluster==4,5]))
kmeans4_cac <- kmeans(StockReturns[,6],centers = 4)
var_cac4<- cbind(var(StockReturns[kmeans4_cac$cluster==1,6]),var(StockReturns[kmeans4_cac$cluster==2,6]),var(StockReturns[kmeans4_cac$cluster==3,6]),var(StockReturns[kmeans4_cac$cluster==4,6]))
kmeans4_est <- kmeans(StockReturns[,7],centers = 4)
var_est4<- cbind(var(StockReturns[kmeans4_est$cluster==1,7]),var(StockReturns[kmeans4_est$cluster==2,7]),var(StockReturns[kmeans4_est$cluster==3,7]),var(StockReturns[kmeans4_est$cluster==4,7]))


kmeans_mvU2 <- kmeans(StockReturns[,2:3],centers = 2)
var_mvU2<-var(StockReturns[kmeans_mvU2$cluster==1,2:3])
var_mvU22<-var(StockReturns[kmeans_mvU2$cluster==2,2:3])

kmeans_mvU3 <- kmeans(StockReturns[,2:3],centers = 3)
var_mvU3<-var(StockReturns[kmeans_mvU3$cluster==1,2:3])
var_mvU32<-var(StockReturns[kmeans_mvU3$cluster==2,2:3])
var_mvU33<-var(StockReturns[kmeans_mvU3$cluster==3,2:3])

kmeans_mvU4 <- kmeans(StockReturns[,2:3],centers = 4)
var_mvU4<-var(StockReturns[kmeans_mvU4$cluster==1,2:3])
var_mvU42<-var(StockReturns[kmeans_mvU4$cluster==2,2:3])
var_mvU43<-var(StockReturns[kmeans_mvU4$cluster==3,2:3])
var_mvU44<-var(StockReturns[kmeans_mvU4$cluster==4,2:3])

kmeans_mvt <- kmeans(StockReturns[,c(-1,-8)],centers = 2)
var_mvt2<-var(StockReturns[kmeans_mvt$cluster==1,c(-1,-8)])
var_mvt22<-var(StockReturns[kmeans_mvt$cluster==2,c(-1,-8)])

kmeans_mvt3 <- kmeans(StockReturns[,c(-1,-8)],centers = 3)
var_mvt31<-var(StockReturns[kmeans_mvt3$cluster==1,c(-1,-8)])
var_mvt32<-var(StockReturns[kmeans_mvt3$cluster==2,c(-1,-8)])
var_mvt33<-var(StockReturns[kmeans_mvt3$cluster==3,c(-1,-8)])

kmeans_mvt4 <- kmeans(StockReturns[,c(-1,-8)],centers = 4)
kmeans_mvt4$centers
var_mvt41<-var(StockReturns[kmeans_mvt4$cluster==1,c(-1,-8)])
var_mvt42<-var(StockReturns[kmeans_mvt4$cluster==2,c(-1,-8)])
var_mvt43<-var(StockReturns[kmeans_mvt4$cluster==3,c(-1,-8)])
var_mvt44<-var(StockReturns[kmeans_mvt4$cluster==4,c(-1,-8)])

### MARKOV ###

K2 <- 2
start.val2_sp <- hmmspec(init = rep(1/K2, K2),
                      trans = matrix(1/K2, nrow = K2, ncol = K2),
                      parms.emis = list(mu = kmeans_2sp$centers,sigma=var_sp2),
                      dens.emis = dnorm.hsmm)
mod.hmm.k2_sp <- hmmfit(StockReturns[,2],start.val2_sp, mstep = mstep.norm)
start.val2_nas <- hmmspec(init = rep(1/K2, K2),
                         trans = matrix(1/K2, nrow = K2, ncol = K2),
                         parms.emis = list(mu = kmeans_2nas$centers,sigma=var_2nas),
                         dens.emis = dnorm.hsmm)
mod.hmm.k2_nas <- hmmfit(StockReturns[,3],start.val2_nas, mstep = mstep.norm)
start.val2_fts <- hmmspec(init = rep(1/K2, K2),
                         trans = matrix(1/K2, nrow = K2, ncol = K2),
                         parms.emis = list(mu = kmeans_2fts$centers,sigma=var_2fts),
                         dens.emis = dnorm.hsmm)
mod.hmm.k2_fts <- hmmfit(StockReturns[,4],start.val2_fts, mstep = mstep.norm)
start.val2_dac <- hmmspec(init = rep(1/K2, K2),
                         trans = matrix(1/K2, nrow = K2, ncol = K2),
                         parms.emis = list(mu = kmeans_2dac$centers,sigma=var_2dac),
                         dens.emis = dnorm.hsmm)
mod.hmm.k2_dac <- hmmfit(StockReturns[,5],start.val2_dac, mstep = mstep.norm)
start.val2_cac <- hmmspec(init = rep(1/K2, K2),
                         trans = matrix(1/K2, nrow = K2, ncol = K2),
                         parms.emis = list(mu = kmeans_2cac$centers,sigma=var_2cac),
                         dens.emis = dnorm.hsmm)
mod.hmm.k2_cac <- hmmfit(StockReturns[,6],start.val2_cac, mstep = mstep.norm)
start.val2_est <- hmmspec(init = rep(1/K2, K2),
                         trans = matrix(1/K2, nrow = K2, ncol = K2),
                         parms.emis = list(mu = kmeans_2est$centers,sigma=var_2est),
                         dens.emis = dnorm.hsmm)
mod.hmm.k2_est <- hmmfit(StockReturns[,7],start.val2_est, mstep = mstep.norm)
start.val2_mvU2 <- hmmspec(init = c(1,0),
                           trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K2, ncol = K2),
                           parms.emis = list(mu = list(kmeans_mvU2$centers[1,],kmeans_mvU2$centers[2,]) , 
                                             sigma=list(var_mvU2,var_mvU22)) ,
                           dens.emis = dmvnorm.hsmm)
mod.hmm.k2_mvU2 <- hmmfit(matrix(unlist(Stock_ret[,1:2]),ncol=2), 
                          start.val2_mvU2, mstep = mstep.mvnorm)
start.val2_mvt <- hmmspec(init = c(1,0),
                          trans = matrix(c(.9,.1,.1,.9),byrow=T, nrow = K2, ncol = K2),
                          parms.emis = list(mu = list(kmeans_mvt$centers[1,],kmeans_mvt$centers[2,]) , 
                                            sigma=list(var_mvt2,var_mvt22)) ,
                          dens.emis = dmvnorm.hsmm)
mod.hmm.k2_mvt <- hmmfit(matrix(unlist(Stock_ret),ncol=6), 
                         start.val2_mvt, mstep = mstep.mvnorm)

K3 <- 3 
start.val3_sp <- hmmspec(init = rep(1/K3, K3),
                      trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                      parms.emis = list(mu = kmeans_3sp$centers,sigma=var_sp3),
                      dens.emis = dnorm.hsmm)
mod.hmm.k3_sp <- hmmfit(StockReturns[,2], start.val3_sp, mstep = mstep.norm)
start.val3_nas <- hmmspec(init = rep(1/K3, K3),
                         trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                         parms.emis = list(mu = kmeans_3nas$centers,sigma=var_nas3),
                         dens.emis = dnorm.hsmm)
mod.hmm.k3_nas <- hmmfit(StockReturns[,3],start.val3_nas, mstep = mstep.norm)
start.val3_fts <- hmmspec(init = rep(1/K3, K3),
                         trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                         parms.emis = list(mu = kmeans_3fts$centers,sigma=var_fts3),
                         dens.emis = dnorm.hsmm)
mod.hmm.k3_fts <- hmmfit(StockReturns[,4],start.val3_fts, mstep = mstep.norm)
start.val3_dac <- hmmspec(init = rep(1/K3, K3),
                         trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                         parms.emis = list(mu = kmeans_3dac$centers,sigma=var_dac3),
                         dens.emis = dnorm.hsmm)
mod.hmm.k3_dac <- hmmfit(StockReturns[,5],start.val3_dac, mstep = mstep.norm)
start.val3_cac <- hmmspec(init = rep(1/K3, K3),
                         trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                         parms.emis = list(mu = kmeans_3cac$centers,sigma=var_cac3),
                         dens.emis = dnorm.hsmm)
mod.hmm.k3_cac <- hmmfit(StockReturns[,6],start.val3_cac, mstep = mstep.norm)
start.val3_est <- hmmspec(init = rep(1/K3, K3),
                         trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),                     
                         parms.emis = list(mu = kmeans_3est$centers,sigma=var_est3),
                         dens.emis = dnorm.hsmm)
mod.hmm.k3_est <- hmmfit(StockReturns[,7],start.val3_est, mstep = mstep.norm)
start.val2_mvU3 <- hmmspec(init = rep(1/K3,K3),
                           trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),
                           parms.emis = list(mu = list(kmeans_mvU3$centers[1,],kmeans_mvU3$centers[2,],kmeans_mvU3$centers[3,]) , 
                                             sigma=list(var_mvU3,var_mvU32,var_mvU33)) ,
                           dens.emis = dmvnorm.hsmm)
mod.hmm.k2_mvU3 <- hmmfit(matrix(unlist(Stock_ret[,1:2]),ncol=2), 
                          start.val2_mvU3, mstep = mstep.mvnorm)
start.val3_mvt <- hmmspec(init = rep(1/K3,K3),
                          trans = matrix(c(.8,.1,.1,.1,.8,.1,.1,.1,.8),byrow=T, nrow = K3, ncol = K3),
                          parms.emis = list(mu = list(kmeans_mvt3$centers[1,],kmeans_mvt3$centers[2,],kmeans_mvt3$centers[3,]), 
                                            sigma=list(var_mvt31,var_mvt32,var_mvt33)),
                          dens.emis = dmvnorm.hsmm) 
mod.hmm.k3_mvt <- hmmfit(matrix(unlist(Stock_ret),ncol=6), 
                         start.val3_mvt, mstep = mstep.mvnorm)
K4 <- 4 

start.val4 <- hmmspec(init = rep(1/K4, K4),
                      trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                      parms.emis = list(mu = kmeans4_sp$centers,sigma=var_sp4),
                      dens.emis = dnorm.hsmm)
mod.hmm.k4_sp <- hmmfit(StockReturns[,2],start.val4, mstep = mstep.norm)

start.val4_nas <- hmmspec(init = rep(1/K4, K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                          parms.emis = list(mu = kmeans4_nas$centers,sigma=var_nas4),
                          dens.emis = dnorm.hsmm)
mod.hmm.k4_nas <- hmmfit(StockReturns[,3],start.val4_nas, mstep = mstep.norm)

start.val4_fts <- hmmspec(init = rep(1/K4, K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                          parms.emis = list(mu = kmeans4_fts$centers,sigma=var_fts4),
                          dens.emis = dnorm.hsmm)
mod.hmm.k4_fts <- hmmfit(StockReturns[,4],start.val4_fts, mstep = mstep.norm)

start.val4_dac <- hmmspec(init = rep(1/K4, K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                          parms.emis = list(mu = kmeans4_dac$centers,sigma=var_dac4),
                          dens.emis = dnorm.hsmm)
mod.hmm.k4_dac <- hmmfit(StockReturns[,5],start.val4_dac, mstep = mstep.norm)

start.val4_cac <- hmmspec(init = rep(1/K4, K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                          parms.emis = list(mu = kmeans4_cac$centers,sigma=var_cac4),
                          dens.emis = dnorm.hsmm)
mod.hmm.k4_cac <- hmmfit(StockReturns[,6],start.val4_cac, mstep = mstep.norm)

start.val4_est <- hmmspec(init = rep(1/K4, K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7), nrow = K4, ncol = K4),
                          parms.emis = list(mu = kmeans4_est$centers,sigma=var_est4),
                          dens.emis = dnorm.hsmm)
mod.hmm.k4_est <- hmmfit(StockReturns[,7],start.val4_est, mstep = mstep.norm)

start.val2_mvU4 <- hmmspec(init = rep(1/K4,K4),
                           trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7),byrow=T, nrow = K4, ncol = K4),
                           parms.emis = list(mu = list(kmeans_mvU4$centers[1,],kmeans_mvU4$centers[2,],kmeans_mvU4$centers[3,],kmeans_mvU4$centers[4,]) , 
                                             sigma=list(var_mvU4,var_mvU42,var_mvU43,var_mvU44)),
                           dens.emis = dmvnorm.hsmm)
mod.hmm.k2_mvU4 <- hmmfit(matrix(unlist(Stock_ret[,1:2]),ncol=2), 
                          start.val2_mvU4, mstep = mstep.mvnorm)

start.val4_mvt <- hmmspec(init = rep(1/K4,K4),
                          trans = matrix(c(.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7,.1,.1,.1,.1,.7),byrow=T, nrow = K4, ncol = K4),
                          parms.emis = list(mu = list(kmeans_mvt4$centers[1,],kmeans_mvt4$centers[2,],kmeans_mvt4$centers[3,],kmeans_mvt4$centers[4,]), 
                                            sigma=list(var_mvt41,var_mvt42,var_mvt43,var_mvt44)),
                          dens.emis = dmvnorm.hsmm) 
mod.hmm.k4_mvt <- hmmfit(matrix(unlist(Stock_ret),ncol=6), 
                         start.val4_mvt, mstep = mstep.mvnorm)

states_sp3 <- mod.hmm.k3_sp$yhat
states_nas3 <- mod.hmm.k3_nas$yhat
states_fts3 <- mod.hmm.k3_fts$yhat
states_dac3 <- mod.hmm.k3_dac$yhat
states_cac3 <- mod.hmm.k3_cac$yhat
states_est3 <- mod.hmm.k3_est$yhat

states_sp4 <- mod.hmm.k4_sp$yhat
states_nas4 <- mod.hmm.k4_nas$yhat
states_fts4 <- mod.hmm.k4_fts$yhat
states_dac4 <- mod.hmm.k4_dac$yhat
states_cac4 <- mod.hmm.k4_cac$yhat
states_est4 <- mod.hmm.k4_est$yhat
states_mvU4 <- mod.hmm.k2_mvU4$yhat

bic2_sp <- -2*mod.hmm.k2_sp$loglik[length(mod.hmm.k2_sp$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_sp <- round(bic2_sp, 2)
bic2_nas <- -2*mod.hmm.k2_nas$loglik[length(mod.hmm.k2_nas$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_nas <- round(bic2_nas, 2)
bic2_fts <- -2*mod.hmm.k2_fts$loglik[length(mod.hmm.k2_fts$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_fts <- round(bic2_fts, 2)
bic2_dac <- -2*mod.hmm.k2_dac$loglik[length(mod.hmm.k2_dac$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_dac <- round(bic2_dac, 2)
bic2_cac <- -2*mod.hmm.k2_cac$loglik[length(mod.hmm.k2_cac$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_cac <- round(bic2_cac, 2)
bic2_est <- -2*mod.hmm.k2_est$loglik[length(mod.hmm.k2_est$loglik)]+log(6444)*((2-1)+(2*(2-1))+4)
bic2_est <- round(bic2_est, 2)
bic2_mv <- -2*mod.hmm.k2_mvU2$loglik[length(mod.hmm.k2_mvU2$loglik)]+log(6444)*((2-1)+(2*(2-1))+12)
bic2_mv <- round(bic2_mv, 2)
bic2_mvt <- -2*mod.hmm.k2_mvt$loglik[length(mod.hmm.k2_mvt$loglik)]+log(6444)*((2-1)+(2*(2-1))+24)
bic2_mvt <- round(bic2_mvt, 2)

bic3_sp <- -2*mod.hmm.k3_sp$loglik[length(mod.hmm.k3_sp$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_sp <- round(bic3_sp, 2)
bic3_nas <- -2*mod.hmm.k3_nas$loglik[length(mod.hmm.k3_nas$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_nas <- round(bic3_nas, 2)
bic3_fts <- -2*mod.hmm.k3_fts$loglik[length(mod.hmm.k3_fts$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_fts <- round(bic3_fts, 2)
bic3_dac <- -2*mod.hmm.k3_dac$loglik[length(mod.hmm.k3_dac$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_dac <- round(bic3_dac, 2)
bic3_cac <- -2*mod.hmm.k3_cac$loglik[length(mod.hmm.k3_cac$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_cac <- round(bic3_cac, 2)
bic3_est <- -2*mod.hmm.k3_est$loglik[length(mod.hmm.k3_est$loglik)]+log(6444)*((3-1)+(3*(3-1))+6) 
bic3_est <- round(bic3_est, 2)
bic3_mv <- -2*mod.hmm.k2_mvU3$loglik[length(mod.hmm.k2_mvU3$loglik)]+log(6444)*((3-1)+(3*(3-1))+18)
bic3_mv <- round(bic3_mv, 2)
bic3_mvt <- -2*mod.hmm.k3_mvt$loglik[length(mod.hmm.k3_mvt$loglik)]+log(6444)*((3-1)+(3*(3-1))+36)
bic3_mvt <- round(bic3_mvt, 2)

bic4_sp <- -2*mod.hmm.k4_sp$loglik[length(mod.hmm.k4_sp$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_sp <- round(bic4_sp, 2)
bic4_nas <- -2*mod.hmm.k4_nas$loglik[length(mod.hmm.k4_nas$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_nas <- round(bic4_nas, 2)
bic4_fts <- -2*mod.hmm.k4_fts$loglik[length(mod.hmm.k4_fts$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_fts <- round(bic4_fts, 2)
bic4_dac <- -2*mod.hmm.k4_dac$loglik[length(mod.hmm.k4_dac$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_dac <- round(bic4_dac, 2)
bic4_cac <- -2*mod.hmm.k4_cac$loglik[length(mod.hmm.k4_cac$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_cac <- round(bic4_cac, 2)
bic4_est <- -2*mod.hmm.k4_est$loglik[length(mod.hmm.k4_est$loglik)]+log(6444)*((4-1)+(4*(4-1))+8)
bic4_est <- round(bic4_est, 2)
bic4_mv <- -2*mod.hmm.k2_mvU4$loglik[length(mod.hmm.k2_mvU4$loglik)]+log(6444)*((4-1)+(4*(4-1))+24)
bic4_mv <- round(bic4_mv, 2)
bic4_mvt <- -2*mod.hmm.k4_mvt$loglik[length(mod.hmm.k4_mvt$loglik)]+log(6444)*((4-1)+(4*(4-1))+48)
bic4_mvt <- round(bic4_mvt, 2)

model <- auto.arima(diff(log(REDWINE)),ic="bic")

WINEforecasts <- forecast(model, h=12)

thematic_shiny()

# color statuses
statusColors <- c(
  "gray-dark",
  "gray",
  "secondary",
  "navy",
  "indigo",
  "purple",
  "primary",
  "lightblue",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "white"
)

#' basic_cards_tab ----
basic_cards_tab <- tabItem(
  tabName = "cards",
  fluidRow(
    column(
      width = 12,
      tabBox(
        ribbon(
          text = NULL,
          color = "success"
        ),
        title = NULL,
        elevation = 2,
        id = "tabcard1",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        type = "tabs",
        status = "success",
        solidHeader = TRUE,
        selected = "Dati",
        tabPanel(
          "Dati",
          DT::dataTableOutput("dataT")
        ),
        tabPanel(
          "Struttura",
          verbatimTextOutput("structure")
        ),
        tabPanel(
          "Statistiche descrittive",
          verbatimTextOutput("summary")
        )
      )
    )
  )
)

#' social_cards_tab ----
social_cards_tab <- tabItem(
  tabName = "socialcards",
  fluidRow(
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/61ry3k5/profile-pic.png",
        title = "Matteo Gurrieri"
      ),
      collapsible = FALSE,
      status = "success",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/matteogurrieri/", icon("linkedin"), "LinkedIn "),
      tags$a(href="https://github.com/IguanMat", icon("github"), "GitHub")
    ),
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/wpq09Vc/IMG-20221214-232033.jpg",
        title = "Riccardo Bianchi"
      ),
      collapsible = FALSE,
      status = "danger",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/riccardo-bianchi-4928b0251/", icon("linkedin"), "LinkedIn ")
    )
  ),
  fluidRow(
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/942B7rS/IMG-20221214-232055.jpg",
        title = "Edoardo Mercuri"
      ),
      collapsible = FALSE,
      status = "orange",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/edoardo-mercuri-b50a04256/", icon("linkedin"), "LinkedIn ")
    ),
    userBox(
      title = userDescription(
        image = "https://i.ibb.co/HKTKVKJ/IMG-20221214-232816-1.jpg",
        title = "Paolo Losacco"
      ),
      collapsible = FALSE,
      status = "navy",
      elevation = 4,
      tags$a(href="https://www.linkedin.com/in/paolo-losacco-888278239/", icon("linkedin"), "LinkedIn ")
    )
  )
)

# timeseries_tab ----
timeseries_tab <- tabItem(
  tabName = "timeseries",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
        title = NULL,
        elevation = 2,
        id = "tabcard2",
        width = 12,
        collapsible = FALSE, 
        closable = FALSE,
        type = "tabs",
        status = "success",
        solidHeader = TRUE,
        selected = "Autoplot",
        tabPanel(
          "Autoplot",
          column(width = 8, offset = 2,
            plotOutput("autoplot", height = 500)
          )
        ),
        tabPanel(
          "SP500",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel1")),
            column(width = 6,
                   plotOutput("carousel2"))
          )
        ),
        tabPanel(
          "NASDAQ",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel3")),
            column(width = 6,
                   plotOutput("carousel4"))
          )
        ),
        tabPanel(
          "FTSE",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel5")),
            column(width = 6,
                   plotOutput("carousel6"))
          )
        ),
        tabPanel(
          "DAC",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel7")),
            column(width = 6,
                   plotOutput("carousel8"))
          )
        ),
        tabPanel(
          "CAC",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel9")),
            column(width = 6,
                   plotOutput("carousel10"))
          )
        ),
        tabPanel(
          "ESTX50",
          fluidRow(
            column(width = 6,
                   plotOutput("carousel11")),
            column(width = 6,
                   plotOutput("carousel12"))
          )
        )
      )
    )
  )
)

total_tab <- tabItem(
  tabName = "total",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard3",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "Summary",
             tabPanel(
               "Summary",
               fluidRow(
                 verbatimTextOutput("presum1")
               ),
               fluidRow(
                 verbatimTextOutput("sum1")
               )
             ),
             tabPanel(
               "SP500",
               fluidRow(
                 plotOutput("plot_tot1", height = 500)
               )
             ),
             tabPanel(
               "NASDAQ",
               fluidRow(
                 plotOutput("plot_tot2", height = 500)
               )
             ),
             tabPanel(
               "FTSE",
               fluidRow(
                 plotOutput("plot_tot3", height = 500)
               )
             ),
             tabPanel(
               "DAC",
               fluidRow(
                 plotOutput("plot_tot4", height = 500)
               )
             ),
             tabPanel(
               "CAC",
               fluidRow(
                 plotOutput("plot_tot5", height = 500)
               )
             ),
             tabPanel(
               "ESTX50",
               fluidRow(
                 plotOutput("plot_tot6", height = 500)
               )
             ),
             tabPanel(
               "Tests",
               fluidRow(
                 column(width = 5, offset = 1,
                 verbatimTextOutput("test_total1"),
                 verbatimTextOutput("test_total3")),
                 column(width = 5, offset = 1,
                 verbatimTextOutput("test_total2"),
                 verbatimTextOutput("test_total4"),
                 verbatimTextOutput("test_total5"))
               )
             )
      )
    )
  )
)

usa_tab <- tabItem(
  tabName = "usa",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard4",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "Summary",
             tabPanel(
               "Summary",
               fluidRow(
                 verbatimTextOutput("presum2")
               ),
               fluidRow(
                 verbatimTextOutput("sum2")
               )
             ),
             tabPanel(
               "SP500",
               fluidRow(
                 plotOutput("plot_usa1", height = 500)
               )
             ),
             tabPanel(
               "NASDAQ",
               fluidRow(
                 plotOutput("plot_usa2", height = 500)
               )
             ),
             tabPanel(
               "Tests",
               fluidRow(
                 column(width = 5, offset = 1,
                        verbatimTextOutput("test_usa1"),
                        verbatimTextOutput("test_usa3")),
                 column(width = 5, offset = 1,
                        verbatimTextOutput("test_usa2"),
                        verbatimTextOutput("test_usa4"),
                        verbatimTextOutput("test_usa5"))
               )
             )
      )
    )
  )
)

eu_tab <- tabItem(
  tabName = "eu",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard5",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "Summary",
             tabPanel(
               "Summary",
               fluidRow(
                 verbatimTextOutput("presum3")
               ),
               fluidRow(
                 verbatimTextOutput("sum3")
               )
             ),
             tabPanel(
               "FTSE",
               fluidRow(
                 plotOutput("plot_eu1", height = 500)
               )
             ),
             tabPanel(
               "DAC",
               fluidRow(
                 plotOutput("plot_eu2", height = 500)
               )
             ),
             tabPanel(
               "CAC",
               fluidRow(
                 plotOutput("plot_eu3", height = 500)
               )
             ),
             tabPanel(
               "ESTX50",
               fluidRow(
                 plotOutput("plot_eu4", height = 500)
               )
             ),
             tabPanel(
               "Tests",
               fluidRow(
                 column(width = 5, offset = 1,
                        verbatimTextOutput("test_eu1"),
                        verbatimTextOutput("test_eu3")),
                 column(width = 5, offset = 1,
                        verbatimTextOutput("test_eu2"),
                        verbatimTextOutput("test_eu4"),
                        verbatimTextOutput("test_eu5"))
               )
             )
      )
    )
  )
)

hmm_tab <- tabItem(
  tabName = "hmm",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard6",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "SP500",
             tabPanel(
               "SP500",
               fluidRow(
                 column(width = 5, offset = 1,
                   accordion( id = "acc1",
                              accordionItem(
                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                     verbatimTextOutput("bic1"),
                     verbatimTextOutput("bic2"),
                     verbatimTextOutput("bic3")
                   )),
                   verbatimTextOutput("model1")
                 ),
                 column(width = 6,
                        plotOutput("plotz1", height = 250),
                        plotOutput("plotz2", height = 300))
               )
             ),
             tabPanel(
               "NASDAQ",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc2",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic4"),
                                     verbatimTextOutput("bic5"),
                                     verbatimTextOutput("bic6")
                                   )),
                        verbatimTextOutput("model2")
                 ),
                 column(width = 6,
                        plotOutput("plotz3", height = 250),
                        plotOutput("plotz4", height = 300))
               )
             ),
             tabPanel(
               "FTSE",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc3",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic7"),
                                     verbatimTextOutput("bic8"),
                                     verbatimTextOutput("bic9")
                                   )),
                        verbatimTextOutput("model3")
                 ),
                 column(width = 6,
                        plotOutput("plotz5", height = 250),
                        plotOutput("plotz6", height = 300))
               )
             ),
             tabPanel(
               "DAC",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc4",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic10"),
                                     verbatimTextOutput("bic11"),
                                     verbatimTextOutput("bic12")
                                   )),
                        verbatimTextOutput("model4")
                 ),
                 column(width = 6,
                        plotOutput("plotz7", height = 250),
                        plotOutput("plotz8", height = 300))
               )
             ),
             tabPanel(
               "CAC",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc5",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic13"),
                                     verbatimTextOutput("bic14"),
                                     verbatimTextOutput("bic15")
                                   )),
                        verbatimTextOutput("model5")
                 ),
                 column(width = 6,
                        plotOutput("plotz9", height = 250),
                        plotOutput("plotz10", height = 300))
               )
             ),
             tabPanel(
               "ESTX50",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc6",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic16"),
                                     verbatimTextOutput("bic17"),
                                     verbatimTextOutput("bic18")
                                   )),
                        verbatimTextOutput("model6")
                 ),
                 column(width = 6,
                        plotOutput("plotz11", height = 250),
                        plotOutput("plotz12", height = 300))
               )
             ),
             tabPanel(
               "MULTIVAR USA",
               fluidRow(
                 column(width = 5, offset = 1,
                        accordion( id = "acc7",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic19"),
                                     verbatimTextOutput("bic20"),
                                     verbatimTextOutput("bic21")
                                   )),
                        verbatimTextOutput("model7")
                 ),
                 column(width = 6,
                        plotOutput("plotz13", height = 250),
                        plotOutput("plotz14", height = 300))
               )
             ),
             tabPanel(
               "MULTIVAR Total",
               fluidRow(
                 column(width = 6,
                        accordion( id = "acc8",
                                   accordionItem(
                                     title = "BIC 2 Vs BIC 3 Vs BIC 4",
                                     verbatimTextOutput("bic22"),
                                     verbatimTextOutput("bic23"),
                                     verbatimTextOutput("bic24")
                                   )),
                        verbatimTextOutput("model8")
                 ),
                 column(width = 6,
                        plotOutput("plotz15", height = 250),
                        box(width = 12, title = "Multivar Total",maximizable = TRUE,
                        plotOutput("plotz16", height = 600)))
               )
             )
      )
    )
  )
)

dataset2_tab <- tabItem(
  tabName = "dataset2",
  fluidRow(#https://i.ibb.co/SVxYTGJ/Glass-of-Red-Wine-PNG-Clipart-Image.png
    column(width = 5,
           tags$img(src="https://i.ibb.co/1zJxFM8/midevosbrigarevino.jpg", width = 500, height = 500)
    ),
    column(
      width = 7,
      timelineBlock(
        width = 12,
        timelineEnd(color = "danger"),
        timelineLabel("1980", color = "success"),
        timelineItem(
          title = "REDWINE - Monthly sales of Australian red wine (1000 l)",
          verbatimTextOutput("frequency"),
          verbatimTextOutput("structuretime"),
          verbatimTextOutput("summarytime")
        ),
        timelineLabel("1996", color = "success"),
        timelineStart(color = "danger")
      )
    )
  )
)

plots_tab <- tabItem(
  tabName = "plots",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard7",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "Tab 1",
             tabPanel(
               "Tab 1",
               fluidRow(
                 column(6,
                 plotOutput("plots1", height = 250)),
                 column(6,
                 plotOutput("plots2", height = 250))),
               fluidRow(
                 column(6,
                 plotOutput("plots3", height = 250)),
                 column(6,
                 plotOutput("plots4", height = 250))
               )
             ),
             tabPanel(
               "Tab 2",
               plotOutput("plots5")
             ),
             tabPanel(
               "Tab 3",
               fluidRow(
                 column(6, offset = 4,
                        verbatimTextOutput("plots_test1"))),
               fluidRow(
                 column(6,
                        plotOutput("plots6")),
                 column(6,
                        plotOutput("plots7"))
               )
             ),
             tabPanel(
               "Tab 4",
               fluidRow(
                 column(5, offset = 1,
                        verbatimTextOutput("plots_test2")),
                 column(6,
                        plotOutput("plots8", height = 250))),
               fluidRow(
                 column(6,
                        plotOutput("plots9", height = 250)),
                 column(6,
                        plotOutput("plots10", height = 250))
               )
             )
      )
    )
  )
)

last_tab <- tabItem(
  tabName = "last",
  fluidRow(
    column(
      width = 12,
      tabBox(maximizable = TRUE,
             title = NULL,
             elevation = 2,
             id = "tabcard6",
             width = 12,
             collapsible = FALSE, 
             closable = FALSE,
             type = "tabs",
             status = "success",
             solidHeader = TRUE,
             selected = "ARIMA",
             tabPanel(
               "ARIMA",
               fluidRow(
                 column(5, offset = 1,
                        verbatimTextOutput("plots_test3")),
                 column(6,
                        plotOutput("plots11", height = 250)))
             ),
             tabPanel(
               "Residuals",
              fluidRow(
                column(width = 8, offset = 4,
                       verbatimTextOutput("last1"))
              ),
              fluidRow(
                column(width = 6,
                       plotOutput("last2")),
                column(width = 6,
                       plotOutput("last3"))
              )
             )
      ) 
    )
  )
)

shinyApp(
  ui = dashboardPage(
    preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
    dark = TRUE,
    help = FALSE,
    fullscreen = TRUE,
    scrollToTop = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "LUMSA Project",
        color = "success",
        href = "https://www.lumsa.it/didattica/corsi-di-laurea/roma/triennale/tecniche-informatiche-gestione-dati",
        image = "https://i.ibb.co/dL93G7N/Logo-Lumsa.jpg",
        opacity = 0.8
      ),
      fixed = TRUE,
      rightUi = tagList(
        userOutput("user")
      ),
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "success",
          type = "notifications",
          notificationItem(
            inputId = "triggerAction2",
            text = "Clicca sull'icona in basso a sinistra!",
            icon = icon("search", lib = "glyphicon")
          )
        )
      )
    ),
    sidebar = dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "success",
      id = "sidebar",
      customArea = fluidRow(
        actionButton(
          inputId = "myAppButton",
          label = NULL,
          icon = icon("education", lib = "glyphicon"),
          width = NULL,
          status = "success",
          style = "margin: auto"#,
          #dashboardBadge(textOutput("btnVal"), color = "danger")
        )
      ),
      sidebarUserPanel(
        image = "http://rstudio.github.io/shiny/reference/figures/logo.png",
        name = "Developed with Shiny!"
      ),
      sidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        sidebarHeader("Part 1"),
        menuItem(
          "Dataset",
          tabName = "cards",
          icon = icon("database")
        ),
        menuItem(
          "Time Series",
          #badgeLabel = "New",
          #badgeColor = "success",
          tabName = "timeseries",
          icon = icon("time", lib = "glyphicon")
        ),
        menuItem(
          text = "VAR",
          icon = icon("mouse-pointer"),
          startExpanded = FALSE,
          menuSubItem(
            text = HTML(
              paste(
                "Total"
              )
            ),
            tabName = "total",
            icon = icon("globe", lib = "glyphicon")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "USA"
              )
            ),
            tabName = "usa",
            icon = icon("dollar")
          ),
          menuSubItem(
            text = HTML(
              paste(
                "EU"
              )
            ),
            tabName = "eu",
            icon = icon("euro")
          )
        ),
        menuItem(
          "HMM",
          tabName = "hmm",
          icon = icon("eye-close", lib = "glyphicon")
        ),
        sidebarHeader("Part 2"),
        menuItem(
          "Dataset",
          tabName = "dataset2",
          icon = icon("database")
        ),
        menuItem(
          "Plots",
          tabName = "plots",
          icon = icon("line-chart")
        ),
        menuItem(
          "Models",
          tabName = "last",
          icon = icon("time", lib = "glyphicon")
        ),
        sidebarHeader("About Us"),
        menuItem(
          "Our Group",
          tabName = "socialcards",
          icon = icon("users")
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        basic_cards_tab,
        timeseries_tab,
        total_tab,
        usa_tab,
        eu_tab,
        hmm_tab,
        dataset2_tab,
        plots_tab,
        last_tab,
        social_cards_tab
      )
    ),
    controlbar = dashboardControlbar(
      id = "controlbar",
      skin = "light",
      pinned = TRUE,
      overlay = FALSE,
      controlbarMenu(
        id = "controlbarMenu",
        type = "pills",
        controlbarItem(
          "Skin",
          skinSelector()
        )
      )
    ),
    footer = dashboardFooter(
      fixed = FALSE,
      left = a(
        href = "https://www.lumsa.it/",
        target = "_blank", "@LUMSA"
      ),
      right = "2022"
    ),
    title = "Data Mining 5"
  ),
  server = function(input, output, session) {
    useAutoColor()
    
    #output$btnVal <- renderText(input$myAppButton)
    observeEvent(input$myAppButton, {
      showModal(modalDialog("Clicca sul logo LUMSA e dai un'occhiata al nostro corso!", easyClose = TRUE))
    })
    
    # DataTable
    output$dataT <- DT::renderDataTable(
      DT::datatable(StockReturns, options=list(scrollX = T)) 
    )
    
    # Structure
    output$structure <- renderPrint({
      StockReturns %>%
        str()
    })
    
    # Summary
    output$summary <- renderPrint(
      StockReturns %>%
        summary()
    )
    
    output$autoplot <- renderPlot(
      autoplot(Stock)
    )
    
    output$carousel1 <- renderPlot(
      plot(stock_means$year,stock_means$SP500,type="l",ylab="SP500",xlab="Year")
    )
    
    output$carousel2 <- renderPlot(
      hist(StockReturns$SP500,main = "Histogram of SP500",xlab="SP500",breaks = 30)
    )
    
    output$carousel3 <- renderPlot(
      plot(stock_means$year,stock_means$NASDAQ,type="l",ylab="NASDAQ",xlab="Year")
    )
    
    output$carousel4 <- renderPlot(
      hist(StockReturns$NASDAQ,main = "Histogram of NASDAQ",xlab="NASDAQ",breaks = 30)
    )
    
    output$carousel5 <- renderPlot(
      plot(stock_means$year,stock_means$FTSE,type="l",ylab="FTSE",xlab="Year")
    )
    
    output$carousel6 <- renderPlot(
      hist(StockReturns$FTSE,main = "Histogram of FTSE",xlab="FTSE",breaks = 30)
    )
    
    output$carousel7 <- renderPlot(
      plot(stock_means$year,stock_means$DAC,type="l",ylab="DAC",xlab="Year")
    )
    
    output$carousel8 <- renderPlot(
      hist(StockReturns$DAC,main = "Histogram of DAC",xlab="DAC",breaks = 30)
    )
    
    output$carousel9 <- renderPlot(
      plot(stock_means$year,stock_means$CAC,type="l",ylab="CAC",xlab="Year")
    )
    
    output$carousel10 <- renderPlot(
      hist(StockReturns$CAC,main = "Histogram of CAC",xlab="CAC",breaks = 30)
    )
    
    output$carousel11 <- renderPlot(
      plot(stock_means$year,stock_means$ESTX50,type="l",ylab="ESTX50",xlab="Year")
    )
    
    output$carousel12 <- renderPlot(
      hist(StockReturns$ESTX50,main = "Histogram of ESTX50",xlab="ESTX50",breaks = 30)
    )
    
    output$presum1 <- renderPrint(
      VARselect(Stock, lag.max=8,  type="both")[["selection"]]
    )
    
    output$presum2 <- renderPrint(
      VARselect(Stock[,1:2], lag.max=10,  type="both")[["selection"]]
    )
    
    output$presum3 <- renderPrint(
      VARselect(Stock[,c(-1,-2)], lag.max=10,  type="both")[["selection"]]
    )
    
    output$sum1 <- renderPrint(
      summary(modStock)
    )
    
    output$sum2 <- renderPrint(
      summary(modStockUsa)
    )
    
    output$sum3 <- renderPrint(
      summary(modStockEU)
    )
    
    output$plot_tot1 <- renderPlot(
      plot(modStock,names = "SP500")
    )
    
    output$plot_usa1 <- renderPlot(
      plot(modStockUsa,names = "SP500")
    )
    
    output$plot_tot2 <- renderPlot(
      plot(modStock,names = "NASDAQ")
    )
    
    output$plot_usa2 <- renderPlot(
      plot(modStockUsa,names = "NASDAQ")
    )
    
    output$plot_tot3 <- renderPlot(
      plot(modStock,names = "FTSE")
    )
    
    output$plot_eu1 <- renderPlot(
      plot(modStockEU,names = "FTSE")
    )
    
    output$plot_tot4 <- renderPlot(
      plot(modStock,names = "DAC") 
    )
    
    output$plot_eu2 <- renderPlot(
      plot(modStockEU,names = "DAC") 
    )
    
    output$plot_tot5 <- renderPlot(
      plot(modStock,names = "CAC")
    )
    
    output$plot_eu3 <- renderPlot(
      plot(modStockEU,names = "CAC")
    )
    
    output$plot_tot6 <- renderPlot(
      plot(modStock,names = "ESTX50")
    )
    
    output$plot_eu4 <- renderPlot(
      plot(modStockEU,names = "ESTX50")
    )
    
    output$test_total1 <- renderPrint(
      serSto$serial
    )
    
    output$test_total2 <- renderPrint(
      normSto$jb.mul$JB
    )
    
    output$test_total4 <- renderPrint(
      normSto$jb.mul$Skewness
    )
    
    output$test_total5 <- renderPrint(
      normSto$jb.mul$Kurtosis
    )
    
    
    output$test_total3 <- renderPrint(
      archSto$arch.mul
    )
    
    output$test_usa1 <- renderPrint(
      serStoUsa$serial
    )
    
    output$test_usa2 <- renderPrint(
      normStoUsa$jb.mul$JB
    )
    
    output$test_usa4 <- renderPrint(
      normStoUsa$jb.mul$Skewness
    )
    
    output$test_usa5 <- renderPrint(
      normStoUsa$jb.mul$Kurtosis
    )
    
    
    output$test_usa3 <- renderPrint(
      archStoUsa$arch.mul
    )
    
    output$test_eu1 <- renderPrint(
      serStoEu$serial
    )
    
    output$test_eu2 <- renderPrint(
      normStoEu$jb.mul$JB
    )
    
    output$test_eu4 <- renderPrint(
      normStoEu$jb.mul$Skewness
    )
    
    output$test_eu5 <- renderPrint(
      normStoEu$jb.mul$Kurtosis
    )
    
    
    output$test_eu3 <- renderPrint(
      archStoEu$arch.mul
    )
    
    output$bic1 <- renderPrint(
      paste("BIC2 =", bic2_sp)
    )
    
    output$bic2 <- renderPrint(
      paste("BIC3 =", bic3_sp)
    )
    
    output$bic3 <- renderPrint(
      paste("BIC4 =", bic4_sp)
    )
    
    output$bic4 <- renderPrint(
      paste("BIC2 =", bic2_nas)
    )
    
    output$bic5 <- renderPrint(
      paste("BIC3 =", bic3_nas)
    )
    
    output$bic6 <- renderPrint(
      paste("BIC4 =", bic4_nas)
    )
    
    output$bic7 <- renderPrint(
      paste("BIC2 =", bic2_fts)
    )
    
    output$bic8 <- renderPrint(
      paste("BIC3 =", bic3_fts)
    )
    
    output$bic9 <- renderPrint(
      paste("BIC4 =", bic4_fts)
    )
    
    output$bic10 <- renderPrint(
      paste("BIC2 =", bic2_dac)
    )
    
    output$bic11 <- renderPrint(
      paste("BIC3 =", bic3_dac)
    )
    
    output$bic12 <- renderPrint(
      paste("BIC4 =", bic4_dac)
    )
    output$bic13 <- renderPrint(
      paste("BIC2 =", bic2_cac)
    )
    output$bic14 <- renderPrint(
      paste("BIC3 =", bic3_cac)
    )
    output$bic15 <- renderPrint(
      paste("BIC4 =", bic4_cac)
    )
    output$bic16 <- renderPrint(
      paste("BIC2 =", bic2_est)
    )
    output$bic17 <- renderPrint(
      paste("BIC3 =", bic3_est)
    )
    output$bic18 <- renderPrint(
      paste("BIC4 =", bic4_est)
    )
    output$bic19 <- renderPrint(
      paste("BIC2 =", bic2_mv)
    )
    
    output$bic20 <- renderPrint(
      paste("BIC3 =", bic3_mv)
    )
    
    output$bic21 <- renderPrint(
      paste("BIC4 =", bic4_mv)
    )
    
    output$bic22 <- renderPrint(
      paste("BIC2 =", bic2_mvt)
    )
    
    output$bic23 <- renderPrint(
      paste("BIC3 =", bic3_mvt)
    )
    
    output$bic24 <- renderPrint(
      paste("BIC4 =", bic4_mvt)
    )
    
    output$model1 <- renderPrint(
      mod.hmm.k4_sp$model
    )
    
    output$model2 <- renderPrint(
      mod.hmm.k4_nas$model
    )
    
    output$model3 <- renderPrint(
      mod.hmm.k4_fts$model
    )
    
    output$model4 <- renderPrint(
      mod.hmm.k3_dac$model
    )
    
    output$model5 <- renderPrint(
      mod.hmm.k4_cac$model
    )
    
    output$model6 <- renderPrint(
      mod.hmm.k4_est$model
    )
    
    output$model7 <- renderPrint(
      mod.hmm.k2_mvU4$model
    )
    
    output$model8 <- renderPrint(
      mod.hmm.k4_mvt$model
    )
    
    output$plotz1 <- renderPlot(
      plot(mod.hmm.k4_sp$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz2 <- renderPlot(
      plot(date,StockReturns[,2],col=states_sp4,main = "SP500",ylab="SP500") %>%
      abline(h=mod.hmm.k4_sp$model$parms.emission$mu[1]) %>%
      abline(h=mod.hmm.k4_sp$model$parms.emission$mu[2],col=2) %>%
      abline(h=mod.hmm.k4_sp$model$parms.emission$mu[3],col=3) %>%
      abline(h=mod.hmm.k4_sp$model$parms.emission$mu[4],col=4)
    )
    
    output$plotz3 <- renderPlot(
      plot(mod.hmm.k4_nas$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz4 <- renderPlot(
      plot(date,StockReturns[,3],col=states_nas4,main = "NAS",ylab="NAS") %>%
        abline(h=mod.hmm.k4_nas$model$parms.emission$mu[1]) %>%
        abline(h=mod.hmm.k4_nas$model$parms.emission$mu[2],col=2) %>%
        abline(h=mod.hmm.k4_nas$model$parms.emission$mu[3],col=3) %>%
        abline(h=mod.hmm.k4_nas$model$parms.emission$mu[4],col=4)
    )
    
    output$plotz5 <- renderPlot(
      plot(mod.hmm.k4_fts$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz6 <- renderPlot(
      plot(date,StockReturns[,4],col=states_fts4,main = "FTSE",ylab="FTSE") %>%
        abline(h=mod.hmm.k4_fts$model$parms.emission$mu[1]) %>%
        abline(h=mod.hmm.k4_fts$model$parms.emission$mu[2],col=2) %>%
        abline(h=mod.hmm.k4_fts$model$parms.emission$mu[3],col=3) %>%
        abline(h=mod.hmm.k4_fts$model$parms.emission$mu[4],col=4)
    )
    
    output$plotz7 <- renderPlot(
      plot(mod.hmm.k3_dac$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz8 <- renderPlot(
      plot(date,StockReturns[,5],col=states_dac3,main = "DAC",ylab="DAC") %>%
        abline(h=mod.hmm.k3_dac$model$parms.emission$mu[1]) %>%
        abline(h=mod.hmm.k3_dac$model$parms.emission$mu[2],col=2) %>%
        abline(h=mod.hmm.k3_dac$model$parms.emission$mu[3],col=3)
    )
    
    output$plotz9 <- renderPlot(
      plot(mod.hmm.k4_cac$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz10 <- renderPlot(
      plot(date,StockReturns[,6],col=states_cac4,main = "CAC",ylab="CAC") %>%
        abline(h=mod.hmm.k4_cac$model$parms.emission$mu[1]) %>%
        abline(h=mod.hmm.k4_cac$model$parms.emission$mu[2],col=2) %>%
        abline(h=mod.hmm.k4_cac$model$parms.emission$mu[3],col=3) %>%
        abline(h=mod.hmm.k4_cac$model$parms.emission$mu[4],col=4)
    )
    
    output$plotz11 <- renderPlot(
      plot(mod.hmm.k4_est$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz12 <- renderPlot(
      plot(date,StockReturns[,7],col=states_est4,main = "ESTX50",ylab="ESTX50") %>%
        abline(h=mod.hmm.k4_est$model$parms.emission$mu[1]) %>%
        abline(h=mod.hmm.k4_est$model$parms.emission$mu[2],col=2) %>%
        abline(h=mod.hmm.k4_est$model$parms.emission$mu[3],col=3) %>%
        abline(h=mod.hmm.k4_est$model$parms.emission$mu[4],col=4)
    )
    
    output$plotz13 <- renderPlot(
      plot(mod.hmm.k2_mvU4$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz14 <- renderPlot(
      plot(StockReturns[,2:3],col=mod.hmm.k2_mvU4$yhat, main = "MULTIVAR USA")
    )
    
    output$plotz15 <- renderPlot(
      plot(mod.hmm.k4_mvt$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    )
    
    output$plotz16 <- renderPlot(
      plot(StockReturns[,2:7],col=mod.hmm.k4_mvt$yhat)
    )
    
    output$frequency <- renderPrint(
      paste("The frequency is:", frequency(REDWINE))
    )
    
    output$structuretime <- renderPrint(
      str(REDWINE)
    )
    
    output$summarytime <- renderPrint(
      summary(REDWINE)
    )
    
    output$plots1 <- renderPlot(
      plot(REDWINE) %>%
        abline(reg=lm(REDWINE~time(REDWINE)))
    )
    
    output$plots2 <- renderPlot(
      ggseasonplot(REDWINE)
    )
    
    output$plots3 <- renderPlot(
      plot(aggregate(REDWINE,FUN=mean),main="Year Trend",ylab="REDWINE mean")
    )
    
    output$plots4 <- renderPlot(
      boxplot(REDWINE~cycle(REDWINE),xlab="Month")
    )
    
    output$plots5 <- renderPlot(
      REDWINE %>%
        decompose() %>%
        autoplot()
    )
    
    output$plots6 <- renderPlot(
      acf(REDWINE)
    )
    
    output$plots7 <- renderPlot(
      pacf(REDWINE)
    )
    
    output$plots8 <- renderPlot(
      plot(diff(log(REDWINE)))
    )
    
    output$plots9 <- renderPlot(
      acf(diff(log(REDWINE)))
    )
    
    output$plots10 <- renderPlot(
      pacf(diff(log(REDWINE)))
    )
    
    output$plots11 <- renderPlot(
      plot(forecast(model,h=24))
    )
    
    output$plots_test1 <- renderPrint(
      adf.test(REDWINE, alternative="stationary", k=0)
    )
    
    output$plots_test2 <- renderPrint(
      adf.test(diff(log(REDWINE)), alternative="stationary", k=0)
    )
    
    output$plots_test3 <- renderPrint(
      auto.arima(diff(log(REDWINE)),ic="bic")
    )
    
    output$last1 <- renderPrint(
      Box.test(WINEforecasts$residuals, lag=20, type="Ljung-Box")
    )
    
    output$last2 <- renderPlot(
      acf(WINEforecasts$residuals, lag.max=20,main="REDWINE Forecast Residuals series")
    )
    
    output$last3 <- renderPlot(
      plot.ts(WINEforecasts$residuals,ylab="REDWINE Forecast Residuals")
    )
    
    observeEvent(input$current_tab, {
      if (input$current_tab == "cards") {
        showModal(modalDialog(
          title = "L'evento si attiva solo per la prima scheda!",
          "Questo progetto è diviso in 2 parti. Nella prima parte abbiamo svolto analisi con VAR e HMM sul dataset StockReturns. Nella seconda parte abbiamo svolto un analisi della serie temporale di REDWINE.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$dark_mode, {
      toast(
        title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
        options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
      )
    })
    
    output$user <- renderUser({
      dashboardUser(
        #name = "You",
        image = "https://cdn-icons-png.flaticon.com/512/21/21104.png",
        title = "You"
      )
    })
  }
)