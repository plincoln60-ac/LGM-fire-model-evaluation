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
############## Load up mean BA models & convert to dfs -----
meanBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/1951_1970_mean_BA.nc")
SPITBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc")
SIMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc")
ORCBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/ORCHIDEE_1951_1970_BA_diff1.nc")
LMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/LPJ LM/LPJLM_1951_1970_BA_diff.nc")

#SIMBA_scale <- tidync("/Volumes/PL SSD/Fire models Jan 2021/SIMFIRE-BLAZE/v2 FLa/scalev2.nc")
#SIMBA_scale <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/scale_v2trial.nc")
#SIMBA_v2 <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/v2mean.nc")
#SIMBA_scale <- SIMBA_scale %>% hyper_tibble(force = T)
#SIMBA_v2 <- SIMBA_v2 %>% hyper_tibble(force = T)
#SIM <- merge(SIMBA_v2, SIMBA_scale, by = c('lon', 'lat'))
#SIM$scaled <- SIM$BA. * SIM$scale
meanBA <- meanBA %>% hyper_tibble(force = T)
colnames(meanBA) <- c("meanBA", "lon", "lat", "time")
SPITBA <- SPITBA %>% hyper_tibble(force = T)
colnames(SPITBA) <- c("SPITBA", "lon", "lat", "time")
SPITBA <- SPITBA[,1:3]
SIMBA <- SIMBA %>% hyper_tibble(force = T)
colnames(SIMBA) <- c("SIMBA", "lon", "lat", "time")
SIMBA <- SIMBA[,1:3]
ORCBA <- ORCBA %>% hyper_tibble(force = T)
colnames(ORCBA) <- c("ORCBA", "lon", "lat", "time")
ORCBA <- ORCBA[,1:3]
LMBA <- LMBA %>% hyper_tibble(force = T)
colnames(LMBA) <- c("LMBA", "lon", "lat", "time")
LMBA <- LMBA[,1:3]
LMBA$LMBA <- LMBA$LMBA *100
###write dimensions
LGM <- nc_open("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/1951_1970_mean_BA.nc")

lon <-ncvar_get(LGM, "lon")
nlon <- dim(lon)
LGM_lon <-lon
nLGM_lon <- dim(LGM_lon)
lat <-ncvar_get(LGM, "lat")
nlat <- dim(lat)
LGM_lat <-lat
nLGM_lat <- dim(LGM_lat)
head(nLGM_lat)
nc_close(LGM)
rm(LGM)

############### merge into one data frame ----
all_BA <- meanBA[,1:3]
all_BA <- merge(x = all_BA, y = SPITBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = SIMBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = ORCBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = LMBA, by = c("lon","lat"))

################ Calculate agreement all values positive, 0 or negative
all_BA$Neg_Count <- rowSums(all_BA[,4:7]<0)
all_BA$Pos_Count <- rowSums(all_BA[,4:7]>0)
all_BA$Zero <- rowSums(all_BA[,4:7]==0)

all_BA$agreement <- pmax(all_BA$Neg_Count, all_BA$Pos_Count, all_BA$Zero)
BA_agreement <- all_BA[, c(1:2, 11)]
BA_agreement$agreement <- as.character(BA_agreement$agreement)


BA_plot <- melt(all_BA, id.vars = c('agreement', 'lat', 'lon'), measure.vars = c("Neg_Count", "Pos_Count", "Zero"))


write.csv(all_BA, '~/Dropbox/2022/LGM Fire figures/BA_agreement.csv')

######## Plot output ----
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

cutpts<- seq(-80,80, by=  10)
cutpts <- rev(cutpts)
??seq
zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 9)
blues <- rev(brewer.pal('Blues', n = 9))
myTheme <- rasterTheme(region = c(blues, zeroCol, reds))


SPIT <- ggplot(data=all_BA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = SPITBA)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutpts), limits= c(-80,80)) +
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
  ggtitle('SPITFIRE BA anomaly') 
SPIT <- SPIT + guides(fill=guide_legend(title="Burnt Area anomaly (%)"))

SIM <- ggplot(data=all_BA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = SIMBA)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutpts), limits= c(-80,80)) +
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
  ggtitle('SIMFIRE BA anomaly') 

ORC<- ggplot(data=all_BA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = ORCBA)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutpts), limits= c(-80,80)) +
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
  ggtitle('ORCHIDEE BA anomaly') 

LPJLM <- ggplot(data=all_BA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = LMBA)) + scale_fill_binned(low = blues ,high = reds, breaks = c(cutpts), limits= c(-80,80)) +
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
  ggtitle('LPJLM BA anomaly') 

all_BA$agreement <- as.factor(all_BA$agreement)
pallette <- c('2' = 'orangered2',
             '3' = 'lightgoldenrod2',
             '4' = 'chartreuse3')

AGR <-ggplot(data=all_BA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = agreement)) + 
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') + scale_fill_manual(values = pallette)+
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
  ggtitle('Model BA agreement') 
AGR <- AGR + guides(fill=guide_legend(title="Model anomaly agreement (n)"))

AGR

??scale_fill_discrete
p2 <-  ggarrange(SPIT, SIM,ORC,LPJLM,
                 ncol = 2, nrow = 2, common.legend = T, legend = 'right')



