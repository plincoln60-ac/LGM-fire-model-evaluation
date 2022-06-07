library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tidync)
library(tidyr)
library(easyNCDF)
library(maps)
library(maptools)
library(rasterVis)
library(sp)
library(lattice)
library(RColorBrewer)
library(geosphere)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Rfast)
library(zoo)
library(sf)
library(readr)
library(plyr)
library(reshape2)
library(xlsx)
library(tidyverse)
library(ggpubr)
rm(list = ls())

LCFfunction <- function(df,model) {
  df <-  df %>% hyper_tibble(force = T) 
  df <- df[, c("TotVegfrac", "Grassfrac","Treesfrac", "lon","lat")]
  df$Grass_Tree_ratio <- df$Grassfrac / df$Treesfrac
  df$model <- model
  return(df)
}
BA_function<-function(df,model) {
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('BA', 'lon', 'lat', 'model')
  return(df)
}
GPP_function <- function(df, model){
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('gpp', 'lon', 'lat', 'model')
  return(df)
}
mFuel_function<-function(df,model) {
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('mFuel', 'lon', 'lat', 'model')
  return(df)
}

ORCLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/ORCHIDEE/ORCHIDEE.nc")
ORCGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_gpp_anomaly_baseline.nc")
ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
ORCmFUEL <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/mFuel/ORCHIDEE/ORCHIDEE_1951_1970_mFuel_diff.nc", var = 'mFuel')
ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
ORCGPP<- GPP_function(ORCGPP, 'ORCHIDEE') 
ORCmFUEL <- mFuel_function(ORCmFUEL, 'ORCHIDEE') 


ORCHIDEE <- merge(ORCBA,ORCLCF, by = c('lat','lon','model'))
ORCHIDEE <- merge(ORCHIDEE,ORCGPP, by = c('lat','lon','model'))
ORCHIDEE <- merge(ORCHIDEE,ORCmFUEL, by = c('lat','lon','model'))

ORCHIDEE_BA <- pivot_longer(ORCHIDEE, cols = c(5:10), names_to = 'Variable', values_to = 'Value')
ORCHIDEE_mFuel <- pivot_longer(ORCHIDEE, cols = c(4:9), names_to = 'Variable', values_to = 'Value')

ORCHIDEE_BA<- ORCHIDEE_BA %>% 
  filter_all(all_vars(!is.infinite(.)))

ORCHIDEE_mFuel<- ORCHIDEE_mFuel %>% 
  filter_all(all_vars(!is.infinite(.)))
#2.2. Create function.
BA_regression <-  function(df){
  #setting the regression function. 
  reg_fun<-lm(formula=df$BA~df$Value) #regression function
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}
mFuel_regression <-  function(df){
  #setting the regression function. 
  reg_fun<-lm(formula=df$mFuel~df$Value) #regression function
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}

regressions_data_ORCHIDEE_BA <-ddply(ORCHIDEE_BA,"Variable",BA_regression)
colnames(regressions_data_ORCHIDEE_BA)<-c ("Variable","slope","intercept","R2","R2.Adj")

regressions_data_ORCHIDEE_mFuel <-ddply(ORCHIDEE_mFuel,"Variable",mFuel_regression)
colnames(regressions_data_ORCHIDEE_mFuel)<-c ("Variable","slope","intercept","R2","R2.Adj")


ggplot(ORCHIDEE_BA, aes(x = Value, y = BA, color = Variable)) +
  geom_point(size = 0.2) + facet_wrap(~Variable,scales="free") + geom_smooth(method="lm", color = 'black')+ ggtitle("SPITFIRE Equatorial Africa")+
  geom_text(data=regressions_data_ORCHIDEE_BA, inherit.aes=FALSE, aes(x = 0, y = 55,
                                                                      label=paste("R^2=",R2))) + theme_bw()




ggplot(ORCHIDEE_mFuel, aes(x = Value, y = mFuel, color = Variable)) +
  geom_point(size = 0.2) + facet_wrap(~Variable, scales="free") + geom_smooth(method="lm", color = 'black')+ scale_
  geom_text(data=regressions_data_ORCHIDEE_mFuel, inherit.aes=FALSE, aes(x = 0, y = 55,
                                                                      label=paste("R^2=",R2))) + theme_bw()






