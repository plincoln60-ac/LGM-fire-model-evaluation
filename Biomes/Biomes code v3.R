rm(list = ls())
library(tidync)
library(tidyr)
library(ggplot2)
library(dplyr)
library(geosphere)
library(ggpubr)

#####Biome 6k data
BiomeLGM <-read.csv('/Volumes/PL SSD/Biome 6000/Biome 6000 LGM.csv')
BiomeLGM <- BiomeLGM[c(2:4,8)]
colnames(BiomeLGM)<- c('sitename', 'lat', 'lon', 'biome')

####SPITFIRE£#####----
SPITFIREBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/LPJ-GUESS-SPITFIRE_LGM_Reference_BA_1951_1970.nc")
SPITFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
SPITFIRELGMBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/LPJ-GUESS-SPITFIRE_LGM_BAmean.nc")
SPITFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
SPITFIRELGMTemp <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/LGMtemp.nc")
SPITFIRETemp <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/controlTemp_SPIT.nc")




SPITFIREBA <- SPITFIREBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRELCF <- SPITFIRELCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRELGMBA <- SPITFIRELGMBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRELGMLCF <- SPITFIRELGMLCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRELGMTemp <- SPITFIRELGMTemp %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRETemp <- SPITFIRETemp %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SPITFIRELGMTemp <- SPITFIRELGMTemp[c(1,3,5,7,8)]
SPITFIRETemp <- SPITFIRETemp[c(1,3,5,7,8)]
colnames(SPITFIRELGMTemp) <- c('Tann', 'Tmax', 'Tmin', 'lon', 'lat')
colnames(SPITFIRETemp) <- c('Tann', 'Tmax', 'Tmin', 'lon', 'lat')

SPITFIRELCF <- SPITFIRELCF %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
SPITFIRELGMLCF <- SPITFIRELGMLCF %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
#create reference to left join
#merge dataframes
SPITFIRELGM <- merge(SPITFIRELGMLCF, SPITFIRELGMTemp, by = c('lat', 'lon'))
SPITFIRE <- merge(SPITFIRELCF, SPITFIRETemp, by = c('lat', 'lon'))

###calculate biomes SF1----
SPITFIRE<-SPITFIRE %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`12`)),
    bare = 1-totveg,
    tropical = sum(`8`, `9`,`10`, na.rm = TRUE),
    boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`,`7`, na.rm = TRUE),
    grassland = sum(`11`,`12`, na.rm = TRUE),
    trees = sum(`1`:`10`, na.rm = TRUE))

SPITFIRE$biomes <- 0
SPITFIRE1 <- SPITFIRE %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` <= 2  & `biomes` == 0~ "polar desert/ice",
      sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` >= 2  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.5  & `trees` > `grassland`~ "forest", 
      sum(`trees`, na.rm = TRUE) <= 0.25 & sum(`totveg`, na.rm = TRUE) >= 0.2 & `biomes` == 0 ~'grassland and dry shrubland',
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#LGM-----
SPITFIRELGM <- SPITFIRELGM %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`12`)),
    bare = 1-totveg,
    tropical = sum(`8`, `9`,`10`, na.rm = TRUE),
    boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`,`7`, na.rm = TRUE),
    grassland = sum(`11`,`12`, na.rm = TRUE),
    trees = sum(`1`:`10`, na.rm = TRUE))

#calculate broad summaries----
rm(SPITFIRELGM1)
SPITFIRELGM$biomes <- 0
SPITFIRELGM1 <- SPITFIRELGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` <= 2  & `biomes` == 0~ "polar desert/ice",
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` >= 2  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.5  & `trees` > `grassland`~ "forest", 
    sum(`trees`, na.rm = TRUE) <= 0.25 & sum(`totveg`, na.rm = TRUE) >= 0.2 & `biomes` == 0 ~'grassland and dry shrubland',
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#check numbers
table(SPITFIRELGM1$biomes)
table(SPITFIRE1$biomes)

#####Plot data ----
world <-rnaturalearth:: ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")
biome6kcol <- c('polar desert/ice'='light blue',
                'boreal forest' = 'green4',
                "desert" = 'yellow',
                'forest' = 'green',
                'tropical forest' = 'palegreen4',
                'temperate forest' = 'olivedrab2',
                'grassland and dry shrubland' ='yellow4',
                'savanna and dry woodland' = 'orange3', 
                'warm-temperate forest'='pink', 
                'tundra' ='dark blue')

controlBiome <-ggplot(SPITFIRE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') +geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 
LGMBiome <-ggplot(SPITFIRELGM1, aes(x=lon, y=lat)) + geom_tile(data = SPITFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_point(data = BiomeLGM, aes(x= lon, y=lat,colour = biome)) + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)         
# BA rasters
controlBA <-ggplot(SPITFIREBA, aes(x=lon, y=lat, fill = BA)) + geom_tile(alpha = 0.5) + ggtitle('Control Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
LGMBA <-ggplot(SPITFIRELGMBA, aes(x=lon, y=lat, fill = BA)) + geom_tile(alpha = 0.5) + ggtitle('LGM Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
#multiplot ----
plot <- ggarrange(controlBiome,controlBA, LGMBiome, LGMBA, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot, top = text_grob("LPJ-GUESS-SPITFIRE", 
                                      color = "black", face = "bold", size = 18))

ggplot(SPITFIRE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') +geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 

ggplot(SPITFIRELGM1, aes(x=lon, y=lat)) + geom_tile(data = SPITFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))  + geom_point(data = BiomeLGM, aes(x = lon, y = lat, color = biome)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)       
# BA rasters
####SIMFIRE#####----

SIMFIREBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LPJ-GUESS-BLAZE_v2_scaled.nc")
SIMFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRELCF_mean.nc")
SIMFIRELGMBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LGM_burntArea.nc")
SIMFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")


SIMFIREBA <- SIMFIREBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELCF <- SIMFIRELCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMBA <- SIMFIRELGMBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMLCF <- SIMFIRELGMLCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe


ggplot(BiomeLGM, aes(x=lon, y=lat)) + geom_point() + geom_tile(alpha = 0.5) +  ggtitle('LGM Biome 6000') +geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 



















#2.2. Create function.
regression=function(df){
  #setting the regression function. 
  reg_fun<-lm(formula=df$landCoverFrac~df$BA, na.) #regression function
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}

#regression 
regression_SPITFIRELGM<-plyr::ddply(SPITFIRELGM,"vegtype",regression)
colnames(regression_SPITFIRELGM)<-c ("vegtype","slope","intercept","R2","R2.Adj")
regression_SPITFIRE<-plyr::ddply(SPITFIRE,"vegtype",regression)
colnames(regression_SPITFIRE)<-c ("vegtype","slope","intercept","R2","R2.Adj")




##Mapplot

ggplot(SPITFIRELGM, aes(x=landCoverFrac, y=BA)) + geom_point(size = 0.1) + facet_wrap(~vegtype) +geom_text(data=regression_SPITFIRELGM, inherit.aes=FALSE, aes(x = 0, y = 55,label=paste("R^2=",R2))) +theme_bw() + geom_smooth(method="lm", color = 'red')

ggplot(SPITFIRE, aes(x=landCoverFrac, y=BA)) + geom_point(size = 0.1) + facet_wrap(~vegtype) +geom_text(data=regression_SPITFIRE, inherit.aes=FALSE, aes(x = 0.5, y = 60,label=paste("R^2=",R2))) +theme_bw() +geom_smooth(method="lm", color = 'red')


####SIMFIRE#####----

SIMFIREBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LPJ-GUESS-BLAZE_v2_scaled.nc")
SIMFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF.nc")
SIMFIRELGMBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LGM_burntArea.nc")
SIMFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")
SIMFIRELGMTemp <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LGMTemp_SIM.nc")
SIMFIRETemp <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/controlTemp_SIM.nc")


SIMFIREBA <- SIMFIREBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELCF <- SIMFIRELCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMBA <- SIMFIRELGMBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMLCF <- SIMFIRELGMLCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMTemp <- SIMFIRELGMTemp %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRETemp <- SIMFIRETemp %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMTemp <- SIMFIRELGMTemp[c(1,3,5,7,8)]
SIMFIRETemp <- SIMFIRETemp[c(1,3,5,7,8)]
colnames(SIMFIRELGMTemp) <- c('Tann', 'Tmax', 'Tmin', 'lon', 'lat')
colnames(SIMFIRETemp) <- c('Tann', 'Tmax', 'Tmin', 'lon', 'lat')



SIMFIRELCF <- SIMFIRELCF %>% pivot_wider(names_from = "sfc", values_from = "scale")
SIMFIRELGMLCF <- SIMFIRELGMLCF %>% pivot_wider(names_from = "pft", values_from = "landCoverFrac")
#create reference to left join
#merge dataframes
SIMFIRELGM <- merge(SIMFIRELGMLCF, SIMFIRELGMTemp, by = c('lat', 'lon'))
SIMFIRE <- merge(SIMFIRELCF, SIMFIRETemp, by = c('lat', 'lon'))

###calculate biomes SF1----
SIMFIRE<-SIMFIRE %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`11`)),
    bare = 1-totveg,
    tropical = sum(`7`, `8`,`9`, na.rm = TRUE),
    boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
    grassland = sum(`10`,`11`, na.rm = TRUE),
    trees = sum(`1`:`9`, na.rm = TRUE),
    gdd5 = (((Tmax+Tmin)/2)-5) *365,
    gdd0 = (((Tmax+Tmin)/2)-5) *365,
  )

SIMFIRE$biomes <- 0
SIMFIRE1 <- SIMFIRE %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` <= 2  & `biomes` == 0~ "polar desert/ice",
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` >= 2  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.5 & `boreal` >= `temperate` & `boreal` >= `tropical` & `boreal` >= `grassland`~ "boreal forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.5  & sum(`tropical`, na.rm = TRUE) >= `temperate` & sum(`tropical`, na.rm = TRUE) >= `boreal` & sum(`tropical`, na.rm = TRUE) >= `grassland` ~ "tropical forest",
    sum(`totveg`, na.rm = TRUE) >= 0.5 & sum(`temperate`, na.rm = TRUE) >= `tropical` &  sum(`temperate`, na.rm = TRUE) >= `boreal` & sum(`temperate`, na.rm = TRUE) >= `grassland` & `gdd5` <= 3000 & `biomes` == 0 ~'temperate forest',
    sum(`totveg`, na.rm = TRUE) >= 0.5 &  sum(`trees`, na.rm = TRUE) >= 0.25 & sum(`temperate`, na.rm = TRUE) >= `tropical` &  sum(`temperate`, na.rm = TRUE) >= `boreal` & sum(`temperate`, na.rm = TRUE) >= `grassland` & `gdd5` > 3000 & `biomes` == 0 ~'warm-temperate forest',
    sum(`trees`, na.rm = TRUE) >= 0.25 & `Tmin` > 10 & `gdd5` > 1200 ~ 'savanna and dry woodland',
    sum(`trees`, na.rm = TRUE) <= 0.25 & sum(`totveg`, na.rm = TRUE) >= 0.2 & `gdd0` >= 800 & `biomes` == 0 ~'grassland and dry shrubland',
    sum(`trees`, na.rm = TRUE) <= 0.2 & sum(`totveg`, na.rm = TRUE) >= 0.1 & `gdd0` < 800 & `biomes` == 0 ~'tundra',
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#LGM-----
SIMFIRELGM <- SIMFIRELGM %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`11`)),
    bare = 1-totveg,
    tropical = sum(`7`, `8`,`9`, na.rm = TRUE),
    boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
    grassland = sum(`10`,`11`, na.rm = TRUE),
    trees = sum(`1`:`9`, na.rm = TRUE),
    gdd5 = (((Tmax+Tmin)/2)-5) *365,
    gdd0 = (((Tmax+Tmin)/2)-5) *365,
  )
#calculate broad summaries----
rm(SIMFIRELGM1)
SIMFIRELGM$biomes <- 0
SIMFIRELGM1 <- SIMFIRELGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` <= 2  & `biomes` == 0~ "polar desert/ice",
    sum(`totveg`, na.rm = TRUE) <= 0.2  & `Tann` >= 2  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.5 & `boreal` >= `temperate` & `boreal` >= `tropical` & `boreal` >= `grassland`~ "boreal forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.5  & sum(`tropical`, na.rm = TRUE) >= `temperate` & sum(`tropical`, na.rm = TRUE) >= `boreal` & sum(`tropical`, na.rm = TRUE) >= `grassland` ~ "tropical forest",
    sum(`totveg`, na.rm = TRUE) >= 0.5 & sum(`temperate`, na.rm = TRUE) >= `tropical` &  sum(`temperate`, na.rm = TRUE) >= `boreal` & sum(`temperate`, na.rm = TRUE) >= `grassland` & `gdd5` <= 3000 & `biomes` == 0 ~'temperate forest',
    sum(`totveg`, na.rm = TRUE) >= 0.5 &  sum(`trees`, na.rm = TRUE) >= 0.25 & sum(`temperate`, na.rm = TRUE) >= `tropical` &  sum(`temperate`, na.rm = TRUE) >= `boreal` & sum(`temperate`, na.rm = TRUE) >= `grassland` & `gdd5` > 3000 & `biomes` == 0 ~'warm-temperate forest',
    sum(`trees`, na.rm = TRUE) >= 0.25 & `Tmin` > 10 & `gdd5` > 1200 ~ 'savanna and dry woodland',
    sum(`trees`, na.rm = TRUE) <= 0.25 & sum(`totveg`, na.rm = TRUE) >= 0.2 & `gdd0` >= 800 & `biomes` == 0 ~'grassland and dry shrubland',
    sum(`trees`, na.rm = TRUE) <= 0.2 & sum(`totveg`, na.rm = TRUE) >= 0.1 & `gdd0` < 800 & `biomes` == 0 ~'tundra',
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#check numbers
table(SIMFIRELGM1$biomes)
table(SIMFIRE1$biomes)


#####Plot data ----

controlBiome <-ggplot(SIMFIRE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') +geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 
LGMBiome <-ggplot(SIMFIRELGM1, aes(x=lon, y=lat)) + geom_tile(data = SIMFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_point(data = BiomeLGM, aes(x= lon, y=lat,colour = biome)) + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)         
# BA rasters
controlBA <-ggplot(SIMFIREBA, aes(x=lon, y=lat, fill = BA.)) + geom_tile(alpha = 0.5) + ggtitle('Control Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
LGMBA <-ggplot(SIMFIRELGMBA, aes(x=lon, y=lat, fill = burntArea.monthly)) + geom_tile(alpha = 0.5) + ggtitle('LGM Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
#multiplot ----
plot2 <- ggarrange(controlBiome,controlBA, LGMBiome, LGMBA, 
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("SIMFIRE-BLAZE", 
                                      color = "black", face = "bold", size = 18))


rm(list = ls())


