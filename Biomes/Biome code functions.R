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
library(ggnewscale)

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
    biome =='desert' ~ "bare ground",
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
    biome =='desert' ~ "bare ground",
  ))

world <-rnaturalearth:: ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")
biome6kcol <- c("bare ground" = 'yellow',
                'forest' = 'green4',
                'grassland and shrubland' ='yellow4',
                'TRUE' = "green", 'FALSE' = 'red', 'NEIGHBOURING' = 'orange')

########################################################################################################################
####################################Upload & reformat data into biomes##################################################----
########################################################################################################################

##create & upload functions

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
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
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
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
  df <- df[c(1:2,14:21)]
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
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
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
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
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland")
    )
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
    select(c(1, 2)) %>%
    rlang:: set_names(c("lon", "lat"))
  sfg.list <- unname(apply(x.lonlat, 1, function(i, j) {
    st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    )))
  }, j = y))
  
  # Create sf or sfc object, as appropriate
  if (ncol(x) > 2) {
    x %>%
      select(-c(1, 2)) %>%
      st_sf(geometry = st_sfc(sfg.list), ...)
  } else {
    st_sfc(sfg.list, ...)
  }
 
} #function to produce polyons from points (x) with set buffer (y), crs = can be stated
Mode <- function(x) {
  x <- unlist(x)
  x <- as.numeric(x)
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}#function to calculate the modal value of a list



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
SIMFIRELGMLCF <-SIMLGMLCFfunction(SIMFIRELGMLCF)
ORCHIDEELCF <- ORCLCFfunction(ORCHIDEELCF)
ORCHIDEELGMLCF <- ORCLCFLGMfunction(ORCHIDEELGMLCF)
LPJLMLCF <- LPJLMLCFfunction(LPJLMLCF)
LPJLMLGMLCF <- LPJLMLCFfunction(LPJLMLGMLCF)

#######################

#Raster values to point

# extract circular, buffer----
#loop vectors
#loop vectors
listLGM.dfs<- list(SPITFIRELGMLCF, SIMFIRELGMLCF, ORCHIDEELGMLCF, LPJLMLGMLCF)
list.dfs<- list(SPITFIRELCF, SIMFIRELCF, ORCHIDEELCF, LPJLMLCF)

#sp points of entities
Sras <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
#Set up spatial points of entities
centroid <- SpatialPointsDataFrame(BiomeLGM[,3:2], proj4string=Sras@crs, BiomeLGM)
centroid_PD <- SpatialPointsDataFrame(BiomePD[,3:2], proj4string=Sras@crs, BiomePD)

##loop to extract biome values to points (with buffer) 
#LGM
for(i in 1:length(listLGM.dfs)) {
  mod <- c('SPITFIRE', 'SIMFIRE', 'ORCHIDEE', 'LPJLM')
  c_centre <- paste(mod[i])#write new columns to biome data frame
  c_mode <- paste(mod[i], '_mode', sep="")
  BiomeLGM[[c_centre]] <- as.character('NA')
  BiomeLGM[[c_mode]] <- as.numeric(0)
  df2 <- as.data.frame(listLGM.dfs[i])
  df2 <-df2[,c(2,1,10)] #subset biome
  crs(rast) <- crs(Sras)
  df2<-df2 %>%                          #make biome a numeric value & convert to raster for extracting
    dplyr::rowwise() %>%
    dplyr::mutate(ibiomes = dplyr:: case_when(
      biomes == 'forest' ~1,
      biomes == 'grassland and shrubland' ~2,
      biomes == 'bare ground' ~3)
    )
  rast <-rasterFromXYZ(df2[,c('lon', 'lat','ibiomes')])
  rast <-ratify(rast)

  BiomeLGMtrial <-BiomeLGM[,3:1]       #extract lat lons for points
  pol<-pts2poly_centroids(BiomeLGMtrial, 1, crs = 4326)  #convert to polygons 1 degree from point using function
  pol<- as(pol, 'Spatial') 
  pol <- raster::extract(rast, pol)
  jlen <- length(pol)
  colu <-which(colnames(BiomeLGM)==c_centre)
  colm <-which(colnames(BiomeLGM)==c_mode)
  BiomeLGM[[c_centre]] <- cbind(pol)    #calculate mean value of polygon
  for (j in 1:jlen){
    BiomeLGM[j,colm] <- Mode(pol[j])
  }
}
   
BiomeLGM <-BiomeLGM %>%                          #make biome a numeric value & convert to raster for extracting
  dplyr::mutate(SPITFIRE_mode = dplyr:: case_when(
    SPITFIRE_mode == 1 ~'forest' ,
    SPITFIRE_mode == 2~'grassland and shrubland',
    SPITFIRE_mode == 3 ~'bare ground'))%>%
  dplyr::mutate(SIMFIRE_mode = dplyr:: case_when(
    SIMFIRE_mode == 1 ~'forest' ,
    SIMFIRE_mode == 2~'grassland and shrubland',
    SIMFIRE_mode == 3 ~'bare ground'))%>%
  dplyr::mutate(ORCHIDEE_mode = dplyr:: case_when(
    ORCHIDEE_mode == 1 ~'forest' ,
    ORCHIDEE_mode == 2~'grassland and shrubland',
    ORCHIDEE_mode == 3 ~'bare ground'))%>%
  dplyr::mutate(LPJLM_mode = dplyr:: case_when(
    LPJLM_mode == 1 ~'forest' ,
    LPJLM_mode == 2~'grassland and shrubland',
    LPJLM_mode == 3 ~'bare ground'))
  



for (j in 1:jlen){
  pol <- as.numeric(unlist(pol[j]))
  #calculate modal biome type
  BiomeLGM[j,colu] <-as.character(pol, stringAsFactor = F)  
  BiomeLGM[[c_centre]] <- cbind(pol)    #calculate mean value of polygon
}

pol<-replace(pol, pol==1, 'forest')
pol<-replace(pol, pol==2, 'grassland and shrubland')
pol<-replace(pol, pol==3, 'bare ground')

  r<-  raster::extract(rast,             # raster layer
                       centroid) #
  r<-replace(r, r==1, 'forest')
  r<-replace(r, r==2, 'grassland and shrubland')
  r<-replace(r, r==3, 'bare ground')
  BiomeLGM[[c_centre]] <- cbind(r)
  
  rbuff <- raster::extract(rast,     # raster layer
                           centroid, # SPDF with centroids for buffer
                           buffer = 1)
  jlen <- length(rbuff)
  colu <-which(colnames(BiomeLGM)==c_buffer )
  for (j in 1:jlen){
    rbuff2 <- unique(as.numeric(unlist(rbuff[j])))
    rbuff2<-replace(rbuff2, rbuff2==1, 'forest')
    rbuff2<-replace(rbuff2, rbuff2==2, 'grassland and shrubland')
    rbuff2<-replace(rbuff2, rbuff2==3, 'bare ground')
    rbuff2 <- toString(rbuff2, sep = ",") #convert different biome types to single character string
    BiomeLGM[j,colu] <-as.character(rbuff2, stringAsFactor = F)             #write values to dataframe (note this will only write values to the final column, could probably be improved)
  }
  crs(centroid)<- crs(Sras)#change crs back to lat long 
  crs(rast) <- crs(Sras) #change crs back to lat long 
}






#PD
for(i in 1:length(list.dfs)) {
  mod <- c('SPITFIRE', 'SIMFIRE', 'ORCHIDEE', 'LPJLM')
  c_centre <- paste(mod[i], '_centre', sep="")#write new columns to biome data frame
  c_buffer <- paste(mod[i], '_buffer', sep="")
  BiomePD[[c_centre]] <- as.character('NA')
  BiomePD[[c_buffer]] <- as.character('NA')
  df2 <- as.data.frame(list.dfs[i])
  df2 <-df2[,c(2,1,10)] #subset biome
  df2$biomes <- factor(df2$biomes) #make biome a factor
  df2$ibiomes = as.numeric(df2$biomes) #make a numeric index column of the factor
  rast <-rasterFromXYZ(df2[,c('lon', 'lat','ibiomes')])
  rast <-ratify(rast)
  crs(rast) <- crs(Sras)
  df2<-df2 %>%                          #make biome a numeric value & convert to raster for extracting
    dplyr::rowwise() %>%
    dplyr::mutate(ibiomes = dplyr:: case_when(
      biomes == 'forest' ~1,
      biomes == 'grassland and shrubland' ~2,
      biomes == 'bare ground' ~3)
    )
  crs(centroid_PD)<- "+proj=utm +zone=10 +datum=WGS84"  #change crs to utm (i.e. m). Suggested as a mechanism to include buffers
  crs(rast) <- "+proj=utm +zone=10 +datum=WGS84" #change crs to utm (i.e. m). Suggested as a mechanism to include buffers
  
  r<-  raster::extract(rast,             # raster layer
                       centroid_PD) #
  r<-replace(r, r==1, 'forest')
  r<-replace(r, r==2, 'grassland and shrubland')
  r<-replace(r, r==3, 'bare ground')
  BiomePD[[c_centre]] <- cbind(r)
  
  rbuff <- raster::extract(rast,     # raster layer
                           centroid_PD, # SPDF with centroids for buffer
                           buffer = 1)
  jlen <- length(rbuff)
  colu <-which(colnames(BiomePD)==c_buffer )
  for (j in 1:jlen){
    rbuff2 <- unique(as.numeric(unlist(rbuff[j])))
    rbuff2<-replace(rbuff2, rbuff2==1, 'forest')
    rbuff2<-replace(rbuff2, rbuff2==2, 'grassland and shrubland')
    rbuff2<-replace(rbuff2, rbuff2==3, 'bare ground')
    rbuff2 <- toString(rbuff2, sep = ",") #convert different biome types to single character string
    BiomePD[j,colu] <-as.character(rbuff2, stringAsFactor = F)             #write values to dataframe (note this will only write values to the final column, could probably be improved)
  }
  crs(centroid_PD)<- crs(Sras)#change crs back to lat long 
  crs(rast) <- crs(Sras) #change crs back to lat long 
}

BiomeLGM3 <- BiomeLGM[1:5]
BiomeLGM2<-BiomeLGM %>%

  dplyr::mutate(
    SPITFIRE_match = dplyr::case_when(
      SPITFIRE_mode %>% stringr::str_detect(biomesimple) ~ "TRUE",
      SPITFIRE_mode %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        SPITFIRE_mode %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)%>%
  dplyr::mutate(
    SIMFIRE_match = dplyr::case_when(
      SIMFIRE_mode %>% stringr::str_detect(biomesimple) ~ "TRUE",
      SIMFIRE_mode %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        SIMFIRE_mode %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5) %>%
  dplyr::mutate(
    ORCHIDEE_match = dplyr::case_when(
      ORCHIDEE_mode %>% stringr::str_detect(biomesimple) ~ "TRUE",
      ORCHIDEE_mode %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        ORCHIDEE_mode %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)%>%
  dplyr::mutate(
    LPJLM_match = dplyr::case_when(
      LPJLM_mode %>% stringr::str_detect(biomesimple) ~ "TRUE",
      LPJLM_mode %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        LPJLM_mode %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)

BiomeLGM2 <- BiomeLGM2[1:9]



BiomePD2<-BiomePD %>%
  dplyr::mutate(SPITFIRE_centre = SPITFIRE_centre[1, ],
                SIMFIRE_centre = SIMFIRE_centre[1, ],
                ORCHIDEE_centre = ORCHIDEE_centre[1, ],
                LPJLM_centre = LPJLM_centre[1, ]) %>%
  dplyr::mutate(
    SPITFIRE = dplyr::case_when(
      SPITFIRE_centre %>% stringr::str_detect(biomesimple) ~ "TRUE",
      SPITFIRE_centre %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        SPITFIRE_buffer %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)%>%
  dplyr::mutate(
    SIMFIRE = dplyr::case_when(
      SIMFIRE_centre %>% stringr::str_detect(biomesimple) ~ "TRUE",
      SIMFIRE_centre %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        SIMFIRE_buffer %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5) %>%
  dplyr::mutate(
    ORCHIDEE = dplyr::case_when(
      ORCHIDEE_centre %>% stringr::str_detect(biomesimple) ~ "TRUE",
      ORCHIDEE_centre %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        ORCHIDEE_buffer %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)%>%
  dplyr::mutate(
    LPJLM = dplyr::case_when(
      LPJLM_centre %>% stringr::str_detect(biomesimple) ~ "TRUE",
      LPJLM_centre %>% stringr::str_detect(biomesimple, negate = TRUE) & 
        LPJLM_buffer %>% stringr::str_detect(biomesimple) ~ "NEIGHBOURING",
      TRUE ~ "FALSE"
    ),
    .after = 5)

 ####################################
 ############South America ###########
 ####################################
 #LGM
SPITLGM <- ggplot(SPITFIRELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
   geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
   geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = SPITFIRE_match), colour="black",pch=21, alpha = 0.75) +
   geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
   theme_pubr()+
   scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
   scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
   ggtitle('SPITFIRE') 
     
   SIMLGM <- ggplot(SIMFIRELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
     geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
     geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = SIMFIRE_match), colour="black",pch=21, alpha = 0.75) +
     geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
     theme_pubr()+
     scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
     scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
   ggtitle('SIMFIRE') 
   
   ORCLGM <- ggplot(ORCHIDEELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
     geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
     geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = ORCHIDEE_match), colour="black",pch=21, alpha = 0.75) +
     geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
     theme_pubr()+
     scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
     scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
     ggtitle('ORCHIDEE') 


LPJLMLGM <- ggplot(LPJLMLGMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = LPJLM_match), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('LPJLM') 


plot2 <- ggarrange(SPITLGM,SIMLGM, ORCLGM, LPJLMLGM, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("LGM model to biome 6000 comparison", 
                                       color = "black", face = "bold", size = 18))

#PD
SPIT <- ggplot(SPITFIRELCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomePD2, aes(x= lon, y=lat,fill = SPITFIRE), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('SPITFIRE') 

SIM <- ggplot(SIMFIRELCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomePD2, aes(x= lon, y=lat,fill = SIMFIRE), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('SIMFIRE') 

ORC <- ggplot(ORCHIDEELCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomePD2, aes(x= lon, y=lat,fill = ORCHIDEE), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('ORCHIDEE') 


LPJLM <- ggplot(LPJLMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomePD2, aes(x= lon, y=lat,fill = LPJLM), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('LPJLM') 


plot2 <- ggarrange(SPIT,SIM, ORC, LPJLM, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("PD model to biome 6000 comparison", 
                                       color = "black", face = "bold", size = 18))




#####global

SPITLGM <- ggplot(SPITFIRELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = SPITFIRE_match), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('SPITFIRE') 

SIMLGM <- ggplot(SIMFIRELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = SIMFIRE_match), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('SIMFIRE') 

ORCLGM <- ggplot(ORCHIDEELGMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = ORCHIDEE_match), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('ORCHIDEE') 


LPJLMLGM <- ggplot(LPJLMLGMLCF, aes(x=lon, y=lat, fill = biomes)) +
  geom_tile(alpha = 0.5) + scale_fill_manual(values = biome6kcol) + 
  geom_point(data = BiomeLGM2, aes(x= lon, y=lat,fill = LPJLM_match), colour="black",pch=21, alpha = 0.75) +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('LPJLM') 


plot2 <- ggarrange(SPITLGM,SIMLGM, ORCLGM, LPJLMLGM, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("LGM model to biome 6000 comparison", 
                                       color = "black", face = "bold", size = 18))

