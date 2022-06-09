rm(list = ls())
library(dplyr)
library(raster)
library(tidync)
library(ncdf4) # package for netcdf manipulation
library(sp)
library(maps)
library(maptools)
library(raster) # package for raster manipulation
library(rasterVis)
library(ggplot2)
library(rgdal) # package for geospatial analysis
library(reshape2)
library(ggpattern)
library(BSDA)
library(ggpattern)

netcdf_function <-function(ad){
  BA <- tidync(ad)
  BA <- BA %>% hyper_tibble(force = T)
  return(BA)
}

BASEBA <-netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/baseline_BA_mean.nc")
SIMBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/baseline_BA_mean.nc")
ORCBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_BA_1951_1970_baseline.nc")
LPJBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_1951_1970_baseline.nc")
LGMBA <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LGM_BA_mean.nc")
SIMLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LGM_BA_mean.nc")
ORCLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/LGM_BA_mean.nc")
LPJLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LGM_BA_mean.nc")
Modras <- raster('~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/baseline_BA_mean.nc', var = 'BA')
SIMras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LGM_BA_mean.nc")
ORCras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/LGM_BA_mean.nc")
LPJras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_1951_1970_baseline.nc")

MHa_function <- function(BASEBA, LGMBA, Modras) {
  
Modras<- terra::rast(Modras)
Ras_area <-terra::cellSize(Modras,  unit = 'ha')
Ras_area<- as.data.frame(Ras_area, xy = TRUE)

names(LGMBA) <- c('LGMBA','lon','lat','time')
names(BASEBA) <- c('BA','lon','lat','time')
names(Ras_area) <- c('lon','lat','hectares')

Model <- merge(BASEBA, Ras_area, by = c('lat','lon'))
Model <- merge(Model, LGMBA, by =c('lat','lon'))
Model$MHa <- Model$hectares / 1000000  #calculate millions of hectares
Model$mha_burnt <- (Model$BA/100) * Model$MHa   #calculate millions of hectares burnt
Model$LGM_mha_burnt <- (Model$LGMBA/100) * Model$MHa   #calculate millions of hectares burnt

baseline<-sum(Model$mha_burnt)
LGM<- sum(Model$LGM_mha_burnt)
anomaly<- LGM - baseline
return(c(LGM, baseline,anomaly))
}


SPITFIRE <- MHa_function(BASEBA, LGMBA, Modras)
SIMFIRE <- MHa_function(SIMBASE,SIMLGM,SIMras)
ORCHIDEE <- MHa_function(ORCBASE,ORCLGM,ORCras)
LPJLM <- MHa_function(LPJBASE,LPJLGM,LPJras)


#LGM ice raster file
p <- raster('/Volumes/PL SSD/Shapefiles/LGM mask/LGM mask2.tif')
p[p==1] <- 'ice'

dfp <- as.data.frame(p, xy= T)
dfp2 <- dfp %>% filter(LGM_mask2 == 'ice')
colnames(dfp2) <- c('lon','lat','ice')
#####get simple global coastline shapefile####
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",  destfile = 'coastlines.zip')
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
coastlines <- SpatialLinesDataFrame(coastlines,
                                    coastlines@data)


ggplot(data=SPITFIRE, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = mha_burnt))  + scale_fill_continuous(type = "viridis") +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Baseline BA (Mha)')
