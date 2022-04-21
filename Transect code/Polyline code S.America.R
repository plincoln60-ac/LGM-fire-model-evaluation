rm(list = ls())
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sp)
library(rgdal)
library(tidyr)
library(geosphere)
library(ggpubr)
library(sf)
library(Rcpp)

setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/")

BAm <- raster("1951_1970_mean_BA.nc", var = 'BA')
SPITBA <- raster("SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
SIMBA <- raster("SIMFIRE_1951_1970_BA_diff1.nc", var = 'burntArea.monthly')
LPJLMBA <- raster("LPJLM_1951_1970_BA_diff1.nc", var = 'burnedf')
ORCBA <- raster("ORCHIDEE_1951_1970_BA_diff1.nc", var = 'BA')
models <-  stack(BAm, SPITBA, SIMBA, LPJLMBA,ORCBA)
## S4 method for signature 'Raster,SpatialLines'



#####entity data

detach("package:dplyr", unload=TRUE)
library(plyr)
ent_wd <- ('/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/0_4kv2/')
setwd('/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/0_4kv2/')
LGM_dataset <- ldply(list.files(), read.csv, header=TRUE) #files with base at -71 to 4k 
setwd('~/Dropbox/2021/Research/RPD LGM for model comparison/RPD data/October 21/0_4k')
setwd("/Users/paullincoln/Dropbox/2022/Research/LGM Fire figures/XY plots")
LGM_datasetSAM <-LGM_dataset %>% filter(ID_ENTITY = c(173)) 


detach("package:plyr", unload=TRUE)
library(dplyr)

library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
dbListTables(mydb)

ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 

ent20 <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 20000 and 22000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 

#Subset data frame to polygons
#LGM_dataset <-merge(LGM_dataset, ent, by ='ID_ENTITY')
#LGM_dataset17 <-merge(LGM_dataset, ent17, by ='ID_ENTITY')
LGM_dataset <-merge(LGM_dataset, ent, by ='ID_ENTITY')
LGM_dataset <-LGM_dataset %>% filter(EST_AGE >17000 & EST_AGE <24000)




LGM_datasetsam <-LGM_dataset %>% filter(lat < 12 & lat >-32, lon <-34 &lon > -83)
LGM_datasetsam <-LGM_datasetsam %>% filter(ID_ENTITY != 2205) ##ANggi has really low values



LGM_datasetsam$lat <-as.numeric(LGM_datasetsam$lat)
LGM_datasetsam$longitude <-as.numeric(LGM_datasetsam$longitude)

LGM_datasetsam$ID_ENTITY <-as.factor(LGM_datasetsam$ID_ENTITY)

entboxplot<-ggplot(LGM_datasetsam, aes(x=lon, y= zt, group = ID_ENTITY)) + geom_boxplot(width = 2) +theme_bw()
entboxplot


########code to write 

#create lines

line1 <-gcIntermediate(c(-80,5), c(-40,-15), n= 45,sp=F)
maxline <-gcIntermediate(c(-75,15), c(-35,-5), n= 45,sp=F)
minline <-gcIntermediate(c(-85,-5), c(-45,-25), n= 45,sp=F)

line2 <-gcIntermediate(c(-80,10), c(-44,-20), n= 45,sp=F)
maxline2 <-gcIntermediate(c(-75,15), c(-39,-15), n= 45,sp=F)
minline2 <-gcIntermediate(c(-85,5), c(-49,-25), n= 45,sp=F)

lats <-as.vector(line2[,2])
lons <- as.vector(line2[,1])
#lonsmin <- lons - 15
#lonsmax <- lons +15
#minline <- data.frame(lonsmin,lats)
#maxline <- data.frame(lonsmax, lats)

#produce base dataframes

af_SPITFIRE <-data.frame(lats,lons)
af_SPITFIRE[,3:8] <- NA
colnames(af_SPITFIRE) <-c('lat', 'lon', 'Min', '1st Qu', 'Median', 'Mean', '3rd Qu', 'Max')
af_mean <-data.frame(lats,lons)
af_mean[,3:8] <- NA
colnames(af_mean) <-c('lat', 'lon', 'Min', '1st Qu', 'Median', 'Mean', '3rd Qu', 'Max')
af_SIMFIRE <- data.frame(lats,lons)
af_SIMFIRE[,3:8] <- NA
colnames(af_SIMFIRE) <-c('lat', 'lon', 'Min', '1st Qu', 'Median', 'Mean', '3rd Qu', 'Max')
af_LPJLM  <- data.frame(lats,lons)
af_LPJLM[,3:8] <- NA
colnames(af_LPJLM) <-c('lat', 'lon', 'Min', '1st Qu', 'Median', 'Mean', '3rd Qu', 'Max')
af_ORCHIDEE <- data.frame(lats,lons)
af_ORCHIDEE[,3:8] <- NA
colnames(af_ORCHIDEE) <-c('lat', 'lon', 'Min', '1st Qu', 'Median', 'Mean', '3rd Qu', 'Max')
#stage 1, write min and max lines to polyline
str(af_ORCHIDEE)

d <- 1:45
for (i in d) {
  gc <-gcIntermediate(c(minline2[i,1],minline2[i,2]), c(maxline2[i,1],maxline2[i,2]), n= 14,sp=F)
  result2 <- data.frame(raster::extract(models, gc, cellnumbers = T))
  colnames(result2) <- c('cell number','mean_model', 'SPITFIRE', 'SIMFIRE', 'LPJLM','ORCHIDEE' ) 
  af_mean[i,3:8] <-summary(result2[,2])
  af_SPITFIRE[i,3:8] <- summary(result2[,3])
  af_SIMFIRE[i,3:8] <- summary(result2[,4])
  af_LPJLM[i,3:8] <- summary(result2[,5])
  af_ORCHIDEE[i,3:8] <- summary(result2[,6])
  }
coeff <- 0.1
LGM_datasetsam$ztscaled <- LGM_datasetsam$zt *10
mean <-ggplot(af_mean,aes(x = lat)) + geom_ribbon(aes(ymin= Min, ymax = Max, alpha =0.5))  + geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= zt, group = ID_ENTITY),  width =1, alpha = 0.5) + geom_line(aes(y=Mean, color = 'red')) + theme_bw() + scale_y_continuous(
  "BA anomaly", limits = c(-20,20),
  sec.axis = sec_axis(~.*coeff))


####Plotting

##to make entity z scores plot along transect, they need to be transformed to lie on the line...----
line1 <-gcIntermediate(c(-80,5), c(-40,-15), n= 45,sp=F)
xy <-ent[c('lon', 'lat')]
data(ent)
class(ent)
data(ent) # reload data.frame
coordinates(ent) = c("x", "y") # specify column names
class(ent)
data(ent) # reload data.frame
coordinates(ent) = c(1, 2) # specify column names
class(ent)
data(ent) # reload data.frame
coordinates(ent) = ~x+y # formula
class(ent)
data(ent) # reload data.frame
coordinates(ent) = xy   # as data frame
class(ent)
data(ent) # reload data.frame
coordinates(ent) = as.matrix(xy)   # as matrix
ent$log.zn = log(ent$zinc)
class(ent)
dim(ent)
plot(ent)
line.sp <- st_transform(line1)
class(line1)
points <- st_line_sample(ent, density = 1/1) 
site.sp <- as_Spatial(st_geometry(line1))
line1 <- as_Spatial(line1)



#calculate mean zt to colour scale
means <- aggregate(LGM_datasetsam$zt, list(LGM_datasetsam$ID_ENTITY), FUN=mean) 
colnames(means) <- c('ID_ENTITY', 'meanzt')
LGM_datasetsam <- merge(LGM_datasetsam, means, by= 'ID_ENTITY')
LGM_datasetsam$fill <- LGM_datasetsam$meanzt <0
LGM_dat <- LGM_datasetsam[c(1,11,12)]

  LGM_dat <- distinct(LGM_dat, ID_ENTITY, .keep_all = TRUE)
  LGM_dat <- means %>% merge(LGM_dat, by= 'ID_ENTITY', na.rm= T)
  

#Plotting----

af_SPITFIRE$fill <- af_SPITFIRE$Mean <0
SPITFIRE <- ggplot(af_SPITFIRE,aes(x = lon)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill), color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lon, y= ztscaled, group = ID_ENTITY, fill = fill),  width =0.5, alpha = 0.9) + geom_line(data= subset(LGM_dat, !is.na(meanzt)), aes(x= lon, y = meanzt *10),  color =  'green', linetype = 'dashed', size = 1)+
  scale_y_continuous("BA anomaly", limits= c(-100,100), sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(-80,-40))+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("SPITFIRE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))


af_ORCHIDEE$fill <- af_ORCHIDEE$Mean <0
ORCHIDEE <-ggplot(af_ORCHIDEE,aes(x = lon)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill),color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lon, y= ztscaled, group = ID_ENTITY, fill = fill),  width =0.5, alpha = 0.9)  + geom_line(data= subset(LGM_dat, !is.na(meanzt)), aes(x= lon, y = meanzt *10),  color =  'green', linetype = 'dashed', size = 1)+
  scale_y_continuous("BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(-80,-40)) +geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("ORCHIDEE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

af_LPJLM$fill <- af_LPJLM$Mean <0
LPJLM <- ggplot(af_LPJLM,aes(x = lon)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill), color = NA,width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lon, y= ztscaled, group = ID_ENTITY, fill = fill),  width =0.5, alpha = 0.9)  + geom_line(data= subset(LGM_dat, !is.na(meanzt)), aes(x= lon, y = meanzt *10),  color =  'green', linetype = 'dashed', size = 1)+ scale_y_continuous(
  "BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt'))  +scale_x_continuous(limits = c(-80,-40)) + geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("LPJLM")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

af_SIMFIRE$fill <- af_SIMFIRE$Mean <0
SIMFIRE <- ggplot(af_SIMFIRE,aes(x = lon)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill),color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lon, y= ztscaled, group = ID_ENTITY, fill = fill),  width =0.5, alpha = 0.9) + geom_line(data= subset(LGM_dat, !is.na(meanzt)), aes(x= lon, y = meanzt *10),  color =  'green', linetype = 'dashed', size = 1) + scale_y_continuous(
  "BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(-80,-40))  + geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("SIMFIRE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

transect <- ggarrange(SPITFIRE,SIMFIRE,LPJLM,ORCHIDEE,
         align = 'hv',
          ncol = 2, nrow = 2)

transect

#plot
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")
world
##Mapplot
line2 <-as.data.frame(gcIntermediate(c(-80,10), c(-44,-20), n= 45,sp=F))
maxline2 <-as.data.frame(gcIntermediate(c(-75,15), c(-39,-15), n= 45,sp=F))
minline2 <-as.data.frame(gcIntermediate(c(-85,5), c(-49,-25), n= 45,sp=F))


ggplot(data = world) + geom_map(data = world, map = world, aes(long, lat, map_id = region), color = 'dark grey', fill = 'grey') + 
  geom_point(data = LGM_datasetsam, aes(x = lon, y = lat), color = 'red') +
  coord_quickmap()+geom_line(data = line2, aes(x=lon, y= lat), color = 'blue') +geom_line(data = maxline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed') + geom_line(data = minline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed')  + theme_classic() +  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))



mapplot <-ggplot(data = world) + geom_map(data = world, map = world, aes(long, lat, map_id = region), color = 'dark grey', fill = 'grey') + 
  geom_point(data = LGM_datasetsam, aes(x = lon, y = lat), color = 'red') +
  coord_quickmap()+geom_line(data = line2, aes(x=lon, y= lat), color = 'blue') +geom_line(data = maxline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed') + geom_line(data = minline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed')  + theme_classic() +  scale_x_continuous(limits = c(-90, -30),breaks = seq(-180, 180, 20)) + scale_y_continuous(limits = c(-30, 20), breaks = seq(-60, 90, 20))




ggarrange(mapplot,transect,
          labels = c("A", "B"), widths = c(1,4),legend= F,
          ncol = 2, nrow = 1)



