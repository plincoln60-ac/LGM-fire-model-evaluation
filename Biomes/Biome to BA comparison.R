library(tidync)
library(tidyr)
library(ggplot2)
library(dplyr)
library(geosphere)
library(ggpubr)
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(ggnewscale)
landcoverfunction <-  function(df2) {
  d <- df2
  d %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`) <0.05 ~ "bare ground",
      sum(`totveg`) >0.05 & `trees` >=0.4 ~'forest >40%',
      sum(`totveg`) >0.05 & `trees` <0.4 ~'grassland and shrubland')
    )
}
BAfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:3]
  colnames(df)<- c('BA', 'lon', 'lat')
  return(df)
}
SPITLCFfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
  df<-df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      totveg = sum(c_across(`1`:`12`)),
      bare = 1-totveg,
      tropical = sum(`8`, `9`,`10`, na.rm = TRUE),
      boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
      temperate = sum(`4`,`5`,`6`,`7`, na.rm = TRUE),
      grassland = sum(`11`,`12`, na.rm = TRUE),
      trees = sum(`tropical`,`boreal`,`temperate`, na.rm = TRUE))
  df$biomes <- 0
  df <- landcoverfunction(df)
  df <- df[c(1:2,15:22)]
  
  return(df)
}
SIMLGMLCFfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'vegtype', 'lon', 'lat')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
  df<-df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      totveg = sum(c_across(`1`:`11`)),
      bare = 1-totveg,
      tropical = sum(`7`, `8`,`9`, na.rm = TRUE),
      boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
      temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
      grassland = sum(`10`,`11`, na.rm = TRUE),
      trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE))
  df$biomes <- 0
  df <- landcoverfunction(df)
  df <- df[c(1:2,14:21)]
  return(df)
}
ORCLCFLGMfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
  df <- df[1:13]
  df<-df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      totveg = sum(c_across(`2`:`11`)),
      bare = sum(`1`),
      tropical = sum(`2`, `3`, na.rm = TRUE),
      boreal = sum(`7`, `8`,`9`, na.rm = TRUE),
      temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
      grassland = sum(`10`,`11`, na.rm = TRUE),
      trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE))
  df$biomes <- 0
  df <- landcoverfunction(df)
  df <- df[c(1:2,14:21)]
  return(df)
}
LPJLMLCFfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
  df<-df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      totveg = sum(c_across(`1`:`9`)),
      bare = 1-totveg,
      tropical = sum(`1`, `2`, na.rm = TRUE),
      boreal = sum(`6`, `7`, na.rm = TRUE),
      temperate = sum(`3`,`4`,`5`, na.rm = TRUE),
      grassland = sum(`8`,`9`, na.rm = TRUE),
      trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE))
  df$biomes <- 0
  df <- landcoverfunction(df)
  df <- df[c(1:2,12:19)]
  return(df)
}
pts2poly_centroids <- function(x, y, ...) {
  # Input checks
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    is.numeric(y)
  )
  
  if (ncol(x) == 2 & ("agr" %in% names(list(...))))
    stop("agr cannot be passed to st_sfc(), ",
         "meaning when x only has two columns")
  # Use first two (lon and lat) columns to create list of sfg objects
  x.lonlat <- x %>%
    dplyr::select(c(1, 2)) %>%
    rlang:: set_names(c("lon", "lat"))
  sfg.list <- unname(apply(x.lonlat, 1, function(i, j) {
    sf::st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    )))
  }, j = y))
  
  # Create sf or sfc object, as appropriate
  if (ncol(x) > 2) {
    x %>%
      dplyr:: select(-c(1, 2)) %>%
      sf:: st_sf(geometry = sf:: st_sfc(sfg.list), ...)
  } else {
    sf:: st_sfc(sfg.list, ...)
  }
  
} #function to produce polyons from points (x) with set buffer (y), crs = can be stated
Mode <- function(x) {
  x <- unlist(x)
  x <- as.numeric(x)
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}#function to calculate the modal value of a list
Presence <- function(x) {
  x <- unlist(x)
  x <- unique(x)
  x<- replace(x,x==1, 'forest')
  x<- replace(x,x==2, 'grassland and shrubland')
  x<- replace(x,x==3, 'bare ground')
  x <- as.list(x)
  x <- paste(shQuote(x), collapse = ', ')
}
#function to calculate the modal and presence values of a list
#####get simple global coastline shapefile####
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",  destfile = 'coastlines.zip')
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
coastlines <- SpatialLinesDataFrame(coastlines,
                                    coastlines@data)

biome6kcolquant <- c('bare ground' = 'yellow2',
                     'forest >40%' = 'chartreuse4',
                     'grassland and shrubland' = 'lightgoldenrod3',
                     'ice' = 'slategray1')


#Upload LCF and BA data and reformat
SPITFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
SPITFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
SPITFIREBAdiff <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_1951_1970_BA_diff.nc")
SPITFIRELGMLCF<-SPITLCFfunction(SPITFIRELGMLCF)
SPITFIRELCF<-SPITLCFfunction(SPITFIRELCF)
SPITFIREBAdiff <- BAfunction(SPITFIREBAdiff)
SIMFIRELGMLCF <-SIMLGMLCFfunction(SIMFIRELGMLCF)
ORCHIDEELGMLCF <- ORCLCFLGMfunction(ORCHIDEELGMLCF)
LPJLMLGMLCF <- LPJLMLCFfunction(LPJLMLGMLCF)


SPITFIRELCF <- SPITFIRELCF[,c(1,2,10)]
SPITFIRELGMLCF <- SPITFIRELGMLCF[,c(1,2,10)]
SPITFIRE <- SPITFIRELCF[,1:2]
SPITFIRE <- merge(x= SPITFIRELCF,y= SPITFIRELGMLCF, by = c('lat','lon'), all.x = TRUE)
SPITFIRE <- merge(x= SPITFIRE,y= SPITFIREBAdiff, by = c('lat','lon'), all.x = TRUE)

colnames(SPITFIRE) <- c('lat','lon','Reference','LGM','BA_anomaly')


Comp_func <- function(df){
df %>%
  dplyr::mutate(change = dplyr::case_when(
    `Reference` == `LGM` ~ "no change",
    `Reference` == 'forest >40%' & `LGM` == 'grassland and shrubland' ~ 'to grassland',
    `Reference` == 'forest >40%' & `LGM` == 'bare ground' ~ 'to bare ground',
    `Reference` == 'grassland and shrubland' & `LGM` == 'bare ground' ~ 'to bare ground',
    `Reference` == 'grassland and shrubland' & `LGM` == 'forest >40%' ~ 'to forest',
    `Reference` == 'bare ground' & `LGM` == 'grassland and shrubland' ~ 'to grassland'
    ))%>%
      dplyr::mutate(BA_change = dplyr::case_when(
        `BA_anomaly` >0 ~ "positive",
        `BA_anomaly` <0 ~'negative',
        `BA_anomaly` == 0 ~ 'no change')
  )
}

SPITFIRE <- Comp_func(SPITFIRE)



changes <- c('no change' = 'snow2',
                     'to grassland' = 'orangered2',
                     'to bare ground' = 'yellow',
                     'to forest' = 'chartreuse3',
                     'ice' = 'slategray1')


###############################################
#####global plot####################################
###############################################
###ice mask
p <- raster('/Volumes/PL SSD/Shapefiles/LGM mask/LGM mask2.tif')
p[p==1] <- 'ice'

dfp <- as.data.frame(p, xy= T)
dfp2 <- dfp %>% filter(LGM_mask2 == 'ice')
colnames(dfp2) <- c('lon','lat','ice')




Change <-ggplot(data=SPITFIRE, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = change)) + scale_fill_manual(values = changes, na.value="white") +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice))+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice)) +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Ref to LGM Change') 



LGM <-ggplot(data=SPITFIRE, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = LGM)) + scale_fill_manual(values = biome6kcolquant, na.value="white") +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice))+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice)) +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('LGM') 


REF<-ggplot(data=SPITFIRE, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = Reference)) + scale_fill_manual(values = biome6kcolquant, na.value="white") +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  theme_pubr()+ labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('Reference') 


SPITBA <- ggplot(data=SPITFIRE, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = BA_anomaly)) + scale_fill_gradient2(low = 'blue4',high = 'orangered3', mid = 'white', midpoint = 0, limits= c(-60,60)) +
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
  ggtitle('BA anomaly') 

BA_leg <- get_legend(SPITBA)
BA_leg <- as_ggplot(BA_leg)
REF_leg <- get_legend(REF) 
REF_leg <- as_ggplot(REF_leg)
CH_leg <- get_legend(Change,position = 'left') 
CH_leg <- as_ggplot(CH_leg)
cj <- ggplot(data=SPITFIRE$, aes(x=change, y= BA_change, fill = )) + geom_count()
legends <- ggarrange(BA_leg,REF_leg,CH_leg,
                     ncol = 3, nrow = 1)

plot(legends)
p2 <-  ggarrange(LGM,REF,
                   ncol = 2, nrow = 1, common.legend = T, legend = 'bottom')


p3 <-  ggarrange(Change,SPITBA,
                 ncol = 2, nrow = 1, common.legend = F, legend = 'none')

p4 <-  ggarrange(p2,p3,legends,
                 ncol = 1, nrow = 3, legend = 'none')

plot(p4)


plot(p2)
