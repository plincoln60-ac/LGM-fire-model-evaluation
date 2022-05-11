rm(list = ls())
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(tidync)
library(tidyr)
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
library(ggpubr)
library(purrr)
library(maps)
library(splancs)
library(RMySQL)
library("rnaturalearth")
library("rnaturalearthdata")
#import polygons
wd<-getwd()

SAm_hpol <- readOGR(dsn = "/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Regional shapefiles/SAm_h.shp")
SAm_lpol <- readOGR(dsn = "/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Regional shapefiles/SAm_l.shp")
Europe <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Europe.shp")
Australia <-readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/SE_Australia.shp")
#pick selected polygon to run
pol <- Europe

#set regions

#####Entity map----
##polygons for regions

#####Boxplots----

#upload data to dfs

meanBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/1951_1970_mean_BA.nc", var = 'BA')
SPITBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
SIMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc", var = 'BA')
ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
LMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/LPJLM_1951_1970_BA_diff1.nc", var = 'BA')
SPITLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SPITFIRE/SPITFIRE.nc")
SIMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SIMFIRE/SIMFIRE.nc")
ORCLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/ORCHIDEE/ORCHIDEE.nc")
LMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/LPJLM/LPJLM.nc")

LCFfunction <- function(df,model) {
df <-  df %>% hyper_tibble(force = T) 
df <- df[, c("TotVegfrac", "Grassfrac","Treesfrac", "lon","lat")]
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
#write data to dfs
meanBA <- BA_function(meanBA, 'mean') 
SPITBA <- BA_function(SPITBA, 'SPITFIRE') 
SPITLCF <- LCFfunction(SPITLCF, 'SPITFIRE') 
SIMBA <- BA_function(SIMBA, 'SIMFIRE') 
SIMLCF <- LCFfunction(SIMLCF, 'SIMFIRE') 
ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
LMBA <- BA_function(LMBA, 'LPJLM') 
LMLCF <- LCFfunction(LMLCF, 'LPJLM') 
#data_wide <- data.frame(spread(data = LMBA, key = time, value = Burnt Area Fraction))#reformat data

alldat <- data.frame(rbind(SPITBA, SIMBA, ORCBA, LMBA))
LCFdat <- data.frame(rbind(SPITLCF,SIMLCF,ORCLCF,LMLCF))
rm(meanBA,SPITBA,SIMBA,ORCBA,LMBA)

#work out which grid cells are in the polygons----
sp_function <- function(df, sp, str){
  pts <- as(sp, "SpatialLinesDataFrame")  
  pts <- as.data.frame(as(pts, "SpatialPointsDataFrame"))
  pts <- pts[,(5:6)]
  colnames(pts) <- c('x','y')
  t<-matrix(pts)
  alldatxx <- df[,2:3]
  colnames(alldatxx) <- c('x', 'y')
  alldatxx$in.shape <-  1:nrow(df) %in% inpip(pts = alldatxx, t)
  df$output  <- alldatxx$in.shape
  colnames(df) <- c('BA','lon','lat','model',str)
  df <- df %>% filter(df[5] == 'TRUE')
  return(df)
}
LCFsp_function <- function(df, sp, str){
  pts <- as(pol, "SpatialLinesDataFrame")  
  pts <- as.data.frame(as(pts, "SpatialPointsDataFrame"))
  pts <- pts[,(5:6)]
  colnames(pts) <- c('x','y')
  t<-matrix(pts)
  alldatxx <- df[,4:5]
  colnames(alldatxx) <- c('x', 'y')
  alldatxx$in.shape <-  1:nrow(alldatxx) %in% inpip(pts = alldatxx, t)
  df$output  <- alldatxx$in.shape
  df <- df %>% filter(df[7] == 'TRUE')
  df <- pivot_longer(df,cols =1:3, names_to = "Model", values_to = "Value")
  return(df)
}
#output dfs containing all cell values within the polygon
BA_model <- sp_function(alldat, pol, 'Europe')
LCF_model <- LCFsp_function(LCFdat,pol,'Europe')

#RPD entities----
#compile data from entities
###gather entity data from RPD
mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
dbListTables(mydb)

ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
###make spatial point file

xy <- ent[,c(4,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#filter which entities lie within the polygon
spdf <- data.frame(spdf[pol,])
spdf <- spdf[1:4]
#####upload RPD z scores
detach("package:dplyr", unload=TRUE)
library(plyr)
ent_wd <- ('/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/0_4kv2/')
setwd('/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/0_4kv2/')
LGM_dataset <- ldply(list.files(), read.csv, header=TRUE) #files with base at -71 to 4k 
setwd('~/Dropbox/2021/Research/RPD LGM for model comparison/RPD data/October 21/0_4k')
setwd(wd)


detach("package:plyr", unload=TRUE)
library(dplyr)

#Subset data frame ----
LGM_dataset <-merge(spdf, LGM_dataset, by =c('ID_ENTITY'))
LGM_dataset <- LGM_dataset %>% filter(EST_AGE <24000 & EST_AGE >17000)
LGM_dataset <- LGM_dataset %>% filter(zt <10 & zt >-10)


#multiplot----
#LGM ice raster file
p <- raster('/Volumes/PL SSD/Shapefiles/LGM mask/LGM mask2.tif')
p[p==1] <- 'ice'

dfp <- as.data.frame(p, xy= T)
dfp2 <- dfp %>% filter(LGM_mask2 == 'ice')
colnames(dfp2) <- c('lon','lat','ice')
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",  destfile = 'coastlines.zip')
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
coastlines <- SpatialLinesDataFrame(coastlines,
                                   coastlines@data)

#work out bounding box coordinates of polygon
boun <- data.frame(bbox(pol))

pcoords<- as.data.frame(pol@polygons[[1]]@Polygons[[1]]@coords)
colnames(pcoords) <- c('lon','lat')
ggplot(pol) +geom_polygon()
#plot

mod <- ggplot(LGM_dataset, aes(x=zt, fill = ID_ENTITY)) + 
  geom_boxplot()+ ggpubr:: theme_pubr() +   
  ggpubr::labs_pubr(base_size = 12) + geom_vline(xintercept = 0, color = 'red')+ 
  labs(title='RPD entity z-scores') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank()) #remove x axis ticks
        
RPD <- ggplot(BA_model, aes(x=BA, y=model, fill = model)) + 
  geom_boxplot() + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+
  geom_vline(xintercept = 0, color = 'red') +labs(title='Model BA anomaly') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(),
        legend.position="none") #remove x axis ticks

LCF <- ggplot(LCF_model, aes(x=Value, y=model, fill = Model)) + geom_boxplot() + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+
  geom_vline(xintercept = 0, color = 'red') +labs(title='Model Land cover fraction anomaly') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank()) #remove x axis ticks


map <- ggplot(data=BA_model, aes(x=lon, y=lat)) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  geom_point(data = spdf, aes(x = lon, y = lat), color = 'red') +
  geom_polygon(data = pcoords, aes(x=lon,y=lat), color = 'purple', fill = NA) +
  scale_x_continuous(limits = c(boun[1,1]-4.75, boun[1,2]+4.75),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(boun[2,1]-4.75, boun[2,2]+4.75), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())+labs(title='Map polygon')





model_stat <- list(tapply(Europe_model$BA, Europe_model$model, summary))
ggarrange(map, mod, RPD, LCF,
          labels = c("A", "B", "C", 'D'),
          ncol = 2, nrow = 2, 
          widths = c(1, 1, 1,1),
          legend = NULL)






region_plot_function<- function(){
  
  
  
}



