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

#set horizontal line coordinates, transect width and sampling resolution (hcount), enter values for points as lon then lat
l2a <- c(25.5,10)
l2b<- c(25.5,-20)
lmin2a <- c(l2a[1]-14.5,l2a[2])
lmin2b <- c(l2b[1]-14.5,l2b[2])
lmax2a <- c(l2a[1]+14.5,l2a[2])
lmax2b <-c(l2b[1]+14.5,l2b[2])

hcount <- l2a[2] - l2b[2] 


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
#LGM_datasetSAM <-LGM_dataset %>% filter(ID_ENTITY = c(173)) 


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

#ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
   #                     left join sample s on s.ID_ENTITY = e.ID_ENTITY
    #                    left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
     #                   where am.mean between 20000 and 22000
      #                  AND e.TYPE != 'other'
       #                 group by e.ID_ENTITY;") 

#Subset data frame to polygons
LGM_dataset <-merge(LGM_dataset, ent, by ='ID_ENTITY')
LGM_dataset17 <-merge(LGM_dataset, ent17, by ='ID_ENTITY')




LGM_datasetsam <-LGM_dataset %>% filter(lat < l2a[2] & lat >l2b[2], lon <lmax2a[1] & lon >lmin2b[1])
LGM_datasetsam <-LGM_datasetsam %>% filter(EST_AGE <= 24000 & EST_AGE >=17000)




LGM_datasetsam$lat <-as.numeric(LGM_datasetsam$lat)
LGM_datasetsam$longitude <-as.numeric(LGM_datasetsam$longitude)

LGM_datasetsam$ID_ENTITY <-as.factor(LGM_datasetsam$ID_ENTITY)

entboxplot<-ggplot(LGM_datasetsam, aes(x=lon, y= zt, group = ID_ENTITY)) + geom_boxplot(width = 2) +theme_bw()
entboxplot


########code to write 

#create lines

line2 <-gcIntermediate(p1 = l2a, p2=l2b, n= hcount,sp=F)
maxline2 <-gcIntermediate(p1 = lmax2a, p2=lmax2b, n= hcount,sp=F)
minline2 <-gcIntermediate(p1 = lmin2a, p2=lmin2b, n= hcount,sp=F)

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

d <- 1:32
for (i in d) {
  gc <-gcIntermediate(c(minline2[i,1],minline2[i,2]), c(maxline2[i,1],maxline2[i,2]), n= 10,sp=F)
  result2 <- data.frame(raster::extract(models, gc, cellnumbers = T))
  colnames(result2) <- c('cell number','mean_model', 'SPITFIRE', 'SIMFIRE', 'LPJLM','ORCHIDEE' ) 
  af_mean[i,3:8] <-summary(result2[,2])
  af_SPITFIRE[i,3:8] <- summary(result2[,3])
  af_SIMFIRE[i,3:8] <- summary(result2[,4])
  af_LPJLM[i,3:8] <- summary(result2[,5])
  af_ORCHIDEE[i,3:8] <- summary(result2[,6])
  }
coeff <- 0.1

#scale zscores for comparison
LGM_datasetsam$ztscaled <- LGM_datasetsam$zt *10
mean <-ggplot(af_mean,aes(x = lat)) + geom_ribbon(aes(ymin= Min, ymax = Max, alpha =0.5))  + geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= zt, group = ID_ENTITY),  width =1, alpha = 0.5) + geom_line(aes(y=Mean, color = 'red')) + theme_bw() + scale_y_continuous(
  "BA anomaly", limits = c(-20,20),
  sec.axis = sec_axis(~.*coeff))


#calculate mean zt to colour scale
means <- aggregate(LGM_datasetsam$zt, list(LGM_datasetsam$ID_ENTITY), FUN=mean) 
colnames(means) <- c('ID_ENTITY', 'meanzt')
LGM_datasetsam <- merge(LGM_datasetsam, means, by= 'ID_ENTITY')
LGM_datasetsam$fill <- LGM_datasetsam$meanzt <0
#Plotting----

af_SPITFIRE$fill <- af_SPITFIRE$Mean <0
SPITFIRE <- ggplot(af_SPITFIRE,aes(x = lat)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill), color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= ztscaled, group = ID_ENTITY, fill = fill),  width =2, alpha = 0.9) + 
  scale_y_continuous("BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(l2b[2],l2a[2]))+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("SPITFIRE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))


af_ORCHIDEE$fill <- af_ORCHIDEE$Mean <0
ORCHIDEE <-ggplot(af_ORCHIDEE,aes(x = lat)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill),color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= ztscaled, group = ID_ENTITY),  width =2, alpha = 0.9) + scale_y_continuous(
  "BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(l2b[2],l2a[2])) +geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("ORCHIDEE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

af_LPJLM$fill <- af_LPJLM$Mean <0
LPJLM <- ggplot(af_LPJLM,aes(x = lat)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill), color = NA,width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= ztscaled, group = ID_ENTITY),  width =2, alpha = 0.9) + scale_y_continuous(
  "BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt'))  +scale_x_continuous(limits = c(l2b[2],l2a[2])) + geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("LPJLM")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

af_SIMFIRE$fill <- af_SIMFIRE$Mean <0
SIMFIRE <- ggplot(af_SIMFIRE,aes(x = lat)) + geom_crossbar(aes(y= Mean,ymin=Min, ymax=Max, fill = fill),color = NA, width = 0.5, alpha = 0.5) + geom_line(aes(y=Mean), color = 'purple', size = 1)+ geom_boxplot(data = LGM_datasetsam, aes(x=lat, y= ztscaled, group = ID_ENTITY),  width =2, alpha = 0.9) + scale_y_continuous(
  "BA anomaly", sec.axis = sec_axis(~.*coeff, name = 'zt')) +scale_x_continuous(limits = c(l2b[2],l2a[2]))+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) + theme_pubr() +theme(legend.position = "none") + ggtitle("SIMFIRE")+ scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))

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

line2 <-as.data.frame(gcIntermediate(p1 = l2a, p2=l2b, n= hcount,sp=F))
maxline2 <-as.data.frame(gcIntermediate(p1 = lmax2a, p2=lmax2b, n= hcount,sp=F))
minline2 <-as.data.frame(gcIntermediate(p1 = lmin2a, p2=lmin2b, n= hcount,sp=F))


ggplot(data = world) + geom_map(data = world, map = world, aes(long, lat, map_id = region), color = 'dark grey', fill = 'grey') + 
  geom_point(data = LGM_datasetsam, aes(x = lon, y = lat), color = 'red') +
  coord_quickmap()+geom_line(data = line2, aes(x=lon, y= lat), color = 'blue') +geom_line(data = maxline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed') + geom_line(data = minline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed')  + theme_classic() +  scale_x_continuous(limits = c(85, 125),breaks = seq(-180, 180, 20)) + scale_y_continuous(limits = c(20, 40), breaks = seq(-60, 90, 20))



mapplot <-ggplot(data = world) + geom_map(data = world, map = world, aes(long, lat, map_id = region), color = 'dark grey', fill = 'grey') + 
  geom_point(data = LGM_datasetsam, aes(x = lon, y = lat), color = 'red') +
  coord_quickmap()+geom_line(data = line2, aes(x=lon, y= lat), color = 'blue') +geom_line(data = maxline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed') + geom_line(data = minline2, aes(x=lon, y= lat), color = 'blue', linetype = 'dashed')  + theme_classic() +  scale_x_continuous(limits = c(l2a[1]-30, l2b[1]+30),breaks = seq(-180, 180, 20)) + scale_y_continuous(limits = c(l2b[2]-2, l2a[2]+2), breaks = seq(-60, 90, 20))


ggarrange(mapplot,transect,
          labels = c("A", "B"), widths = c(1,4),legend= F,
          ncol = 2, nrow = 1)


