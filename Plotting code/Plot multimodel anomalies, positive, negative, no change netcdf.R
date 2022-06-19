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
############## Load up mean BA models & convert to dfs -----
meanBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/1951_1970_mean_BA.nc")
SPITBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_BA_anomaly_baseline_mean.nc")
SIMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LPJ-GUESS-BLAZE_BA_anomaly_baseline_remap_mean.nc")
ORCBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc")
LMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/LPJ LM/LPJLM_1951_1970_BA_diff.nc")

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
all_BA <- meanBA[,2:3]
all_BA <- merge(x = all_BA, y = SPITBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = SIMBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = ORCBA, by = c("lon","lat"))
#all_BA <- merge(x = all_BA, y = LMBA, by = c("lon","lat")) #Excluded LMBA

################ Calculate agreement all values positive, 0 or negative
all_BA$Neg_Count <- rowSums(all_BA[,3:5]< 0)
all_BA$Pos_Count <- rowSums(all_BA[,3:5]>0)
all_BA$Zero <- rowSums(all_BA[,3:5]==0)

all_BA$agreement <- pmax(all_BA$Neg_Count, all_BA$Pos_Count, all_BA$Zero)

BA_agreement <- all_BA[, c(1:2, 9)]
table(BA_agreement$agreement)
BA_agreement$agreement <- as.character(BA_agreement$agreement)
all_BA <- all_BA %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(mod_agreement = dplyr::case_when(
    `agreement` == 3  & `Pos_Count` ==3 ~ "3 positive",
    `agreement` == 3  & `Neg_Count` ==3 ~ "3 negative",
    `agreement` == 3  & `Zero` ==3 ~ "3 no change",
    `agreement` == 2  & `Pos_Count` ==2 ~ "2 positive",
    `agreement` == 2  & `Neg_Count` ==2 ~ "2 negative",
    `agreement` == 2  & `Zero` ==2 ~ "2 no change",
    `agreement` == 1   ~ "no agreement",
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table
BA_agreement <- all_BA[, c(1:2, 10)]

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
zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 9)
blues <- rev(brewer.pal('Blues', n = 9))
myTheme <- rasterTheme(region = c(blues, zeroCol, reds))


all_BA$agreement <- as.factor(all_BA$agreement)
pallette <- c('2 negative' = 'skyblue1',
              '3 negative' = 'deepskyblue3',
              '2 positive' = 'tomato1',
              '3 positive' = 'tomato3',
              '2 no change' = 'gray80',
              '3 no change' = 'grey51',
             'no agreement' = 'chartreuse3')

AGR <-ggplot(data=BA_agreement, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = mod_agreement)) + 
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') + scale_fill_manual(values = pallette)+
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  ggpubr:: theme_pubr()+ ggpubr:: labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Model BA agreement') 
AGR <- AGR + guides(fill=guide_legend(title="Model anomaly agreement (n)"))

AGR

p2 <-  ggarrange(SPIT, SIM,ORC,LPJLM,
                 ncol = 2, nrow = 2, common.legend = T, legend = 'right')



