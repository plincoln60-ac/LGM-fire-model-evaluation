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
fname <- c('baseplot_quantitative.pdf')
jpname <- c('baseplot_quantitative.jpeg')
#read in biome data
BiomeLGM <-read.csv('/Volumes/PL SSD/Biome 6000/Biome 6000 LGM.csv')
BiomeLGM <- BiomeLGM[c(2:4,8)]
colnames(BiomeLGM)<- c('sitename', 'lat', 'lon', 'biome')


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

world <-rnaturalearth:: ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")
biome6kcol <- c("bare ground" = 'yellow',
                'forest' = 'green4',
                'grassland and shrubland' ='yellow4',
                'TRUE' = "green", 'FALSE' = 'red')

barfill <- c('TRUE' = "green", 'FALSE' = 'red')

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
      sum(`totveg`) < 0.02 ~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `trees` < `grassland`~ "grassland and shrubland")
    )
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
  df <- df %>%
    dplyr::mutate(biomes = dplyr::case_when(
        sum(`totveg`) < 0.02 ~ "bare ground",
        sum(`totveg`, na.rm = TRUE) >= 0.02  & `grassland` < `trees`~ "forest", 
        sum(`totveg`, na.rm = TRUE) >= 0.02  & `trees` < `grassland`~ "grassland and shrubland")
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
      sum(`totveg`) < 0.02 ~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `trees` < `grassland`~ "grassland and shrubland")
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
      sum(`totveg`) < 0.02 ~ "bare ground",
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.02  & `trees` < `grassland`~ "grassland and shrubland")
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
    sf::st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    )))
  }, j = y))
  
  
  
  
  # Create sf or sfc object, as appropriate
  if (ncol(x) > 2) {
    x %>%
      select(-c(1, 2)) %>%
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



#Upload LCF and BA data and reformat
SPITFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
SIMFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")
ORCHIDEELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF.nc")
LPJLMLGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LGM_landcoverfrac_90yr.nc")

SPITFIRELGMLCF<-SPITLCFfunction(SPITFIRELGMLCF)
SIMFIRELGMLCF <-SIMLGMLCFfunction(SIMFIRELGMLCF)
ORCHIDEELGMLCF <- ORCLCFLGMfunction(ORCHIDEELGMLCF)
LPJLMLGMLCF <- LPJLMLCFfunction(LPJLMLGMLCF)

#######################

#Raster values to point

# extract circular, buffer----
#loop vectors
#loop vectors
listLGM.dfs<- list(SPITFIRELGMLCF, SIMFIRELGMLCF, ORCHIDEELGMLCF, LPJLMLGMLCF)

#sp points of entities
Sras <- raster("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
rast <- raster()

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


#update biomes for % increases


SPITFIRELGMLCF<- SPITFIRELGMLCF %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`biome_quant` = dplyr::case_when(
    sum(`totveg`) < 0.02 ~ "bare ground",
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.7 ~'grassland and shrubland >70%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.4 & `grassland` <=0.7 ~'grassland and shrubland >40%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` <=0.1  ~'grassland and shrubland <10%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.1 & `grassland` <= 0.4 ~'grassland and shrubland >10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.7 ~'forest >70%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.4 & `trees` <=0.7 ~'forest >40%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` <=0.1  ~'forest <10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.1 & `trees` <= 0.4 ~'forest >10%'
  ))
  

SIMFIRELGMLCF<- SIMFIRELGMLCF %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`biome_quant` = dplyr::case_when(
    sum(`totveg`) < 0.02 ~ "bare ground",
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.7 ~'grassland and shrubland >70%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.4 & `grassland` <=0.7 ~'grassland and shrubland >40%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` <=0.1  ~'grassland and shrubland <10%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.1 & `grassland` <= 0.4 ~'grassland and shrubland >10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.7 ~'forest >70%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.4 & `trees` <=0.7 ~'forest >40%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` <=0.1  ~'forest <10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.1 & `trees` <= 0.4 ~'forest >10%'
  ))

ORCHIDEELGMLCF<- ORCHIDEELGMLCF %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`biome_quant` = dplyr::case_when(
    sum(`totveg`) < 0.02 ~ "bare ground",
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.7 ~'grassland and shrubland >70%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.4 & `grassland` <=0.7 ~'grassland and shrubland >40%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` <=0.1  ~'grassland and shrubland <10%',
    `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.1 & `grassland` <= 0.4 ~'grassland and shrubland >10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.7 ~'forest >70%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.4 & `trees` <=0.7 ~'forest >40%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` <=0.1  ~'forest <10%',
    `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.1 & `trees` <= 0.4 ~'forest >10%'
  ))

  LPJLMLGMLCF<- LPJLMLGMLCF %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`biome_quant` = dplyr::case_when(
      sum(`totveg`) < 0.02 ~ "bare ground",
      `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.7 ~'grassland and shrubland >70%',
      `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.4 & `grassland` <=0.7 ~'grassland and shrubland >40%',
      `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` <=0.1  ~'grassland and shrubland <10%',
      `biomes` == 'grassland and shrubland' & sum(`totveg`) >0.02 & `grassland` >0.1 & `grassland` <= 0.4 ~'grassland and shrubland >10%',
      `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.7 ~'forest >70%',
      `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.4 & `trees` <=0.7 ~'forest >40%',
      `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` <=0.1  ~'forest <10%',
      `biomes` == 'forest' & sum(`totveg`) >0.02 & `trees` >0.1 & `trees` <= 0.4 ~'forest >10%'
    ))


biome6kcolquant <- c("bare ground" = 'yellow',
                'forest <10%' = 'lightgreen',
                'forest >10%' = 'yellowgreen',
                'forest >40%' = 'chartreuse4',
                'forest >70%' = 'darkgreen',
                'grassland and shrubland <10%' ='lightyellow1',
                'grassland and shrubland >10%' ='lightgoldenrod1',
                'grassland and shrubland >40%' ='lightgoldenrod3',
                'grassland and shrubland >70%' ='lightgoldenrod4', 
                'ice' = 'slategray1')




###############################################
#####global####################################
###############################################

#LGM
Biomes <- ggplot() + geom_point(data = BiomeLGM2, aes(x=lon, y=lat, fill = biomesimple),pch=21, size = 1.5) + scale_fill_manual(values = biome6kcol) + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+ labs_pubr(base_size = 12)+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('Biome 6000 LGM') 

SPITLGM <- ggplot(data=SPITFIRELGMLCF, aes(x=lon, y=lat, fill = biome_quant)) +
  geom_tile(alpha = 0.9) + scale_fill_manual(values = biome6kcolquant) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+ labs_pubr(base_size = 12)+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('SPITFIRE') 

SIMLGM <- ggplot(SIMFIRELGMLCF, aes(x=lon, y=lat, fill = biome_quant)) +
  geom_tile(alpha = 0.9) + scale_fill_manual(values = biome6kcolquant) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+ labs_pubr(base_size = 12)+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('SIMFIRE') 

ORCLGM <- ggplot(ORCHIDEELGMLCF, aes(x=lon, y=lat, fill = biome_quant)) +
  geom_tile(alpha = 0.9) + scale_fill_manual(values = biome6kcolquant) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+ labs_pubr(base_size = 12)+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('ORCHIDEE') 

LPJLMLGM <- ggplot(LPJLMLGMLCF, aes(x=lon, y=lat, fill = biome_quant)) +
  geom_tile(alpha = 0.9) + scale_fill_manual(values = biome6kcolquant) + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+ labs_pubr(base_size = 12)+
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 80), breaks = seq(-60, 90, 20))+
  ggtitle('LPJLM') 

Biome_LGM_long <- gather(BiomeLGM2, model, Biome6k_match, LPJLM_match:SPITFIRE_match, factor_key=TRUE)
bl <- Biome_LGM_long %>% count(Biome6k_match, model)

bl <- bl %>% 
  dplyr:: mutate(
    model= dplyr:: case_when(
  model == 'SPITFIRE_match' ~'SPITFIRE',
  model == 'SIMFIRE_match' ~"SIMFIRE",
  model == 'ORCHIDEE_match' ~'ORCHIDEE',
  model == 'LPJLM_match' ~'LPJLM'
))

      

Barplot <- ggplot(bl, aes(fill=Biome6k_match, y=n, x=model)) + 
  geom_bar(position="dodge", stat="identity") + geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+ scale_fill_manual(values = barfill) +theme_pubr(border = F, margin = F) + labs_pubr(base_size = 12)+
  ggtitle('Match with Biome 6k entities') 

plotdir<- c('/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/Biome comparison figs Mar17/')

plot2 <- ggarrange(SPITLGM,SIMLGM, ORCLGM, LPJLMLGM, Biomes, Barplot,
                   labels = c("A", "B", "C", "D", "E", "F"),
                   ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")

mplot<-annotate_figure(plot2, top = text_grob("LGM model to biome 6000 comparison", 
                                       color = "black", face = "bold", size = 18))


pdf(paste(plotdir, fname, sep= ""))
plot(mplot)
dev.off()

jpeg(paste(plotdir, jpname, sep= "", quality = 1))
plot(mplot)
dev.off()







####################################
############South America ###########----
####################################
#LGM
Biomes <- ggplot() + geom_point(data = BiomeLGM2, aes(x=lon, y=lat, fill = biomesimple), colour="black",pch=21, alpha = 0.75, size = 3) + scale_fill_manual(values = biome6kcol) + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') +
  theme_pubr()+
  scale_x_continuous(limits = c(-95, -20),breaks = seq(-180, 180, 20)) + 
  scale_y_continuous(limits = c(-40, 20), breaks = seq(-60, 90, 20))+
  ggtitle('Biome 6000 LGM') 

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


