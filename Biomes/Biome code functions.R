rm(list = ls())
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

########################################################################################################################
#######################################Upload Biome 6k data#############################################################----
########################################################################################################################

#read in biome data
BiomeLGM <-read.csv('/Volumes/PL SSD/Biome 6000/Biome 6000 LGM.csv')
BiomeLGM <- BiomeLGM[c(2:4,8)]
colnames(BiomeLGM)<- c('sitename', 'lat', 'lon', 'biome')
BiomePD <-read.csv('/Volumes/PL SSD/Biome 6000/Biome 6000 PD.csv')
BiomePD <- BiomePD[c(2:4,8)]
colnames(BiomePD)<- c('sitename', 'lat', 'lon', 'biome')

BiomeLGM$biomesimple <- 'NA'

#simplify biome categorisation
BiomeLGM <-BiomeLGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomesimple = dplyr::case_when(
    biome =='boreal forest' ~ "forest",
    biome =='temperate forest' ~ "forest",
    biome =='tropical forest' ~ "forest",
    biome =='warm-temperate forest' ~ "forest",
    biome =='grassland and dry shrubland' ~ "grassland and shrubland",
    biome =='savanna and dry woodland' ~ "grassland and shrubland",
    biome =='tundra' ~ "grassland and shrubland",
    biome =='desert' ~ "desert",
  ))
BiomePD <-BiomePD %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomesimple = dplyr::case_when(
    biome =='boreal forest' ~ "forest",
    biome =='temperate forest' ~ "forest",
    biome =='tropical forest' ~ "forest",
    biome =='warm-temperate forest' ~ "forest",
    biome =='grassland and dry shrubland' ~ "grassland and shrubland",
    biome =='savanna and dry woodland' ~ "grassland and shrubland",
    biome =='tundra' ~ "grassland and shrubland",
    biome =='desert' ~ "desert",
  ))

world <-rnaturalearth:: ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")
biome6kcol <- c("desert" = 'yellow',
                'forest' = 'green4',
                'grassland and shrubland' ='yellow4')



########################################################################################################################
####################################Upload & reformat data into biomes##################################################----
########################################################################################################################

##create upload function

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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
  df <- df[c(1:2,15:22)]
  
  return(df)
}
SIMLCFfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
  df <- df[c(1:2,14:21)]
  return(df)
}
ORCLCFfunction <- function(df){
  df <- df %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
  df <- df[c(1:2,12:19)]
  return(df)
}

#Upload LCF and BA data and reformat
SPITFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
SPITFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
SIMFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF.nc")
SIMFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")
ORCHIDEELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/SF2_LCF_1951_1970.nc")
ORCHIDEELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF.nc")
LPJLMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LCF_1951_1970.nc")
LPJLMLGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LGM_landcoverfrac_90yr.nc")

SPITFIRELCF <-SPITLCFfunction(SPITFIRELCF)
SPITFIRELGMLCF<-SPITLCFfunction(SPITFIRELGMLCF)
SIMFIRELCF <- SIMLCFfunction(SIMFIRELCF)
SIMFIRELGMLCF <-SIMLCFfunction(SIMFIRELGMLCF)
ORCHIDEELCF <- ORCLCFfunction(ORCHIDEELCF)
ORCHIDEELGMLCF <- ORCLCFfunction(ORCHIDEELGMLCF)
LPJLMLGMLCF <- LPJLMLCFfunction(LPJLMLGMLCF)


#######################


#Raster values to point
SPIT <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
SPITLGM <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
SIMLCF <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF.nc")
SIMLGM <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")
ORC <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/SF2_LCF_1951_1970.nc")
ORCLGM <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF.nc")
LPJLM <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LCF_1951_1970.nc")
LPJLMLGM <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LGM_landcoverfrac_90yr.nc")
str(ORC)
# extract circular, 20m buffer
set.seed(0)
df2 <-SPITFIRELCF[,c(2,1,10)] #subset biome
df2$biomes <- factor(df2$biomes) #make biome a factor
df2$ibiomes = as.numeric(df2$biomes) #make a numeric index column of the factor
rast <-rasterFromXYZ(df2[,c('lon', 'lat','ibiomes')])
rast <-ratify(rast)
crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs" 
crs(LGM_centroid)<- "+proj=longlat +datum=WGS84 +no_defs" 

crs(LGM_centroid)<- "+proj=utm +datum=WGS84" 
crs(rast) <- "+proj=utm +datum=WGS84" 

r <- data.frame(raster::extract(rast,             # raster layer
                                LGM_centroid,   # SPDF with centroids for buffer
                                buffer = 2,  # buffer size, units depend on CRS
                                # buffer size, units depend on CRS
                           df=T  ))
for (i in 10:15) {
  hist(r[[i]], main=(paste("plot",i)))
}

head(r)
??raster :: extract
crs(LGM_centroid)<- "+proj=longlat +datum=WGS84 +no_defs" 
crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs" 

crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

print(r)

cent_heightList <- raster::extract(rast,LGM_centroid,buffer = 20)
BiomeLGM$ID <- 1:291
LGM_centroid <- SpatialPointsDataFrame(
  BiomeLGM[,3:2], proj4string=rast@crs, BiomeLGM)

radius <- 4
yPlus <- BiomeLGM$lat+radius
xPlus <- BiomeLGM$lon+radius
yMinus <- BiomeLGM$lat-radius
xMinus <- BiomeLGM$lon-radius
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon


polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))







for (i in 1:5) {
  hist(r[[i]], main=(paste("plot",i)))
}





plot(rast)
points(BiomeLGM$lon, BiomeLGM$lat, pch=0, cex = 2)
points(BiomeLGM$lon,BiomeLGM$lat, pch=19, cex=.5, col = 2)



bi <-shapefile(BiomeLGM)
r <- data.frame(raster::extract(rast,             # raster layer
                                BiomeLGM[,3:2],   # SPDF with centroids for buffer
                                buffer = 10,  # buffer size, units depend on CRS
                                fun = mean,
                                # buffer size, units depend on CRS
                                df=TRUE) )  

# return a dataframe? 
??raster::extract
str(rast)
colnames(r) <- c('ID', 'SPIT')

BiomeLGM2<-cbind(BiomeLGM, r)
??raster::extract
BiomeLGM2 <- BiomeLGM2 %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(SPIT = dplyr::case_when(
    `SPIT`== 1  ~ "desert",
    `SPIT`==  2 ~ "forest",
    `SPIT`== 3~ "grassland and shrubland",
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table







