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
library(ggpubr)
library(RColorBrewer)
############## Load up mean BA models & convert to dfs -----
PRECIP <- tidync("/Volumes/PL SSD/Fire models Jan 2021/LGM climate input/LGManomalies/Anomalies/PRECIP_mean.nc")
PRECIP <- PRECIP %>% tidync::hyper_tibble(force = T)


PRECIP <- PRECIP %>%
  dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon))


TEMP <- tidync("/Volumes/PL SSD/Fire models Jan 2021/LGM climate input/LGManomalies/Anomalies/Temp_mean.nc")
TEMP <- TEMP %>% tidync::hyper_tibble(force = T)
TEMP <- TEMP %>%
  dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon))


WIND <- tidync("/Volumes/PL SSD/Fire models Jan 2021/LGM climate input/LGManomalies/Anomalies/Wind_speed_mean.nc")
WIND <- WIND %>% tidync::hyper_tibble(force = T)
WIND <- WIND %>%
  dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon))



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

cutpts<- seq(-200,200, by=  25)
cutpts <- rev(cutpts)

zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 9)
revred <- rev(brewer.pal('YlOrRd', n = 9))
blues <- rev(brewer.pal('Blues', n = 9))
revblue <- brewer.pal('Blues', n = 9)

myTheme <- rasterTheme(region = c(blues, zeroCol, reds))

PRECIP_plot <- ggplot(data=PRECIP, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = pre_ltm)) + scale_fill_binned(low = revred ,high = revblue, breaks = c(cutpts), limits= c(-200,200)) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Precipitation anomaly') 
PRECIP_plot <- PRECIP_plot + guides(fill=guide_legend(title="Precip anomaly (mm mon-1)"))
PRECIP_plot<-ggarrange(PRECIP_plot,
          ncol = 1, nrow = 1, common.legend = T, legend = 'right')

PRECIP_plot
cutptst<- seq(-40,40, by=  5)

Temp_plot <- ggplot(data=TEMP, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = tas_ltm)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutptst), limits= c(-40,40)) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Temperature anomaly') 
Temp_plot <- Temp_plot + guides(fill=guide_legend(title="Temp anomaly (deg C)"))

Temp_plot<- ggarrange(Temp_plot,
          ncol = 1, nrow = 1, common.legend = T, legend = 'right')



cutptw<- rev(seq(-10,10, by=  1))


Wind_plot <- ggplot(data=WIND, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = wspd_ltm)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutptw), limits= c(-10,10)) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Wind speed anomaly') 
Wind_plot <- Wind_plot + guides(fill=guide_legend(title="Wind anomaly (m s-1)"))

Wind_plot<- ggarrange(Wind_plot,
          ncol = 1, nrow = 1, common.legend = T, legend = 'right')


ggpubr::ggarrange(PRECIP_plot,Temp_plot,Wind_plot, legend = 'right')




