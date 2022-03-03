rm(list = ls())
library(tidync)
library(tidyr)
library(ggplot2)
library(dplyr)
library(geosphere)
library(ggpubr)
########################################################################################################################
#######################################Biome 6k data####################################################################----
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

########################################################################################################################
########################################################SPITFIRE########################################################----
########################################################################################################################

#Upload LCF and BA data and reformat
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
    trees = sum(`tropical`,`boreal`,`temperate`, na.rm = TRUE))

SPITFIRE$biomes <- 0
SPITFIRE1 <- SPITFIRE %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
    dplyr::mutate(biomes = dplyr::case_when(
      sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
      sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
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
    trees = sum(`tropical`,`boreal`,`temperate`, na.rm = TRUE))

#calculate broad summaries----
rm(SPITFIRELGM1)
SPITFIRELGM$biomes <- 0
SPITFIRELGM1 <- SPITFIRELGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
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
biome6kcol <- c("desert" = 'yellow',
                'forest' = 'green4',
                'grassland and shrubland' ='yellow4')

controlBiome <-ggplot(SPITFIRE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') + geom_point(data = BiomePD, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21)+ geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 
LGMBiome <-ggplot(SPITFIRELGM1, aes(x=lon, y=lat)) + geom_tile(data = SPITFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_point(data = BiomeLGM, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21)+  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)         

# BA rasters
controlBA <-ggplot(SPITFIREBA, aes(x=lon, y=lat, fill = BA)) + geom_tile(alpha = 0.5) + ggtitle('Control Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
LGMBA <-ggplot(SPITFIRELGMBA, aes(x=lon, y=lat, fill = BA)) + geom_tile(alpha = 0.5) + ggtitle('LGM Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
#multiplot ----
plot <- ggarrange(controlBiome,controlBA, LGMBiome, LGMBA, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot, top = text_grob("LPJ-GUESS-SPITFIRE", 
                                      color = "black", face = "bold", size = 18))


########################################################################################################################
########################################################SIMFIRE########################################################----
########################################################################################################################


SIMFIREBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LPJ-GUESS-BLAZE_v2_scaled.nc")
SIMFIRELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF.nc")
SIMFIRELGMBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/LGM_burntArea.nc")
SIMFIRELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF.nc")


SIMFIREBA <- SIMFIREBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELCF <- SIMFIRELCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMBA <- SIMFIRELGMBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
SIMFIRELGMLCF <- SIMFIRELGMLCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe

SIMFIRELCF <- SIMFIRELCF %>% pivot_wider(names_from = "sfc", values_from = "scale")
SIMFIRELGMLCF <- SIMFIRELGMLCF %>% pivot_wider(names_from = "pft", values_from = "landCoverFrac")
#create reference to left join
#merge dataframes
SIMFIRELGM <- SIMFIRELGMLCF
SIMFIRE <- SIMFIRELCF

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
    trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE)
  )


SIMFIRE$biomes <- 0
SIMFIRE1 <- SIMFIRE %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
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
    trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE)
  )
#calculate broad summaries----
rm(SIMFIRELGM1)
SIMFIRELGM$biomes <- 0
SIMFIRELGM1 <- SIMFIRELGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#check numbers
table(SIMFIRELGM1$biomes)
table(SIMFIRE1$biomes)


#####Plot data ----

controlBiome <-ggplot(SIMFIRE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') + geom_point(data = BiomePD, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21, size = 0.75)+ geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 
LGMBiome <-ggplot(SIMFIRELGM1, aes(x=lon, y=lat)) + geom_tile(data = SPITFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_point(data = BiomeLGM, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21)+  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)         


# BA rasters
controlBA <-ggplot(SIMFIREBA, aes(x=lon, y=lat, fill = BA.)) + geom_tile(alpha = 0.5) + ggtitle('Control Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
LGMBA <-ggplot(SIMFIRELGMBA, aes(x=lon, y=lat, fill = burntArea.monthly)) + geom_tile(alpha = 0.5) + ggtitle('LGM Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
#multiplot ----
plot2 <- ggarrange(controlBiome,controlBA, LGMBiome, LGMBA, 
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("SIMFIRE-BLAZE", 
                                      color = "black", face = "bold", size = 18))



########################################################################################################################
########################################################ORCHIDEE########################################################----
########################################################################################################################

ORCHIDEEBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_SF2_BA.nc")
ORCHIDEELCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF_ref.nc")
ORCHIDEELGMBA <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_BA.nc")
ORCHIDEELGMLCF <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF_Nov21.nc")

ORCHIDEEBA <- ORCHIDEEBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
ORCHIDEELCF <- ORCHIDEELCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
ORCHIDEELGMBA <- ORCHIDEELGMBA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
ORCHIDEELGMLCF <- ORCHIDEELGMLCF %>% tidync::hyper_tibble(force = T) # write variable to a dataframe


ORCHIDEELCF <- ORCHIDEELCF %>% pivot_wider(names_from = "sfc", values_from = "scale")
ORCHIDEELGMLCF <- ORCHIDEELGMLCF %>% pivot_wider(names_from = "pft", values_from = "landCoverFrac")
#create reference to left join
#merge dataframes
ORCHIDEELGM <-ORCHIDEELGMLCF
ORCHIDEE <- ORCHIDEELCF

###calculate biomes SF1----
ORCHIDEE<-ORCHIDEE %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`13`)),
    bare = 1-totveg,
    tropical = sum(`2`, `3`, na.rm = TRUE),
    boreal = sum(`7`, `8`,`9`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
    grassland = sum(`10`,`11`, na.rm = TRUE),
    trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE)
  )


ORCHIDEE$biomes <- 0
ORCHIDEE1 <- ORCHIDEE %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#LGM-----
ORCHIDEELGM <- ORCHIDEELGM %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    totveg = sum(c_across(`1`:`11`)),
    bare = 1-totveg,
    tropical = sum(`7`, `8`,`9`, na.rm = TRUE),
    boreal = sum(`1`, `2`,`3`, na.rm = TRUE),
    temperate = sum(`4`,`5`,`6`, na.rm = TRUE),
    grassland = sum(`10`,`11`, na.rm = TRUE),
    trees = sum(`tropical`:`boreal`, `temperate`, na.rm = TRUE)
  )
#calculate broad summaries----
rm(ORCHIDEELGM1)
ORCHIDEELGM$biomes <- 0
ORCHIDEELGM1 <- ORCHIDEELGM %>%
  dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
  dplyr::mutate(biomes = dplyr::case_when(
    sum(`totveg`, na.rm = TRUE) <= 0.1  & `biomes` == 0~ "desert",
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `grassland` < `trees`~ "forest", 
    sum(`totveg`, na.rm = TRUE) >= 0.1  & `trees` < `grassland`~ "grassland and shrubland", 
    # CONDITION ~ VALUE, # this is how you add more categories
    TRUE ~ NA_character_ # This is the default category
  )) %>%
  dplyr::ungroup() # Removes the 'rowwise' property from the table

#check numbers
table(ORCHIDEELGM1$biomes)
table(ORCHIDEE1$biomes)


#####Plot data ----

controlBiome <-ggplot(ORCHIDEE1, aes(x=lon, y=lat, fill = biomes)) + geom_tile(alpha = 0.5) +  ggtitle('Control Biomes') + geom_point(data = BiomePD, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21, size = 0.75)+ geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_manual(values = biome6kcol) 
LGMBiome <-ggplot(ORCHIDEELGM1, aes(x=lon, y=lat)) + geom_tile(data = SPITFIRELGM1, aes(fill = biomes), alpha = 0.5) + ggtitle('LGM Biomes') + geom_point(data = BiomeLGM, aes(x= lon, y=lat,fill = biomesimple), colour="black",pch=21)+  geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80)) + scale_fill_manual(values = biome6kcol)  + scale_color_manual(values = biome6kcol, guide = FALSE)         


# BA rasters
controlBA <-ggplot(ORCHIDEEBA, aes(x=lon, y=lat, fill = BA.)) + geom_tile(alpha = 0.5) + ggtitle('Control Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
LGMBA <-ggplot(ORCHIDEELGMBA, aes(x=lon, y=lat, fill = burntArea.monthly)) + geom_tile(alpha = 0.5) + ggtitle('LGM Burnt Area (%)') + geom_map(data = world, map = world, aes(long, lat, map_id = region), size = 0.25, color = 'black', fill = 'transparent') + theme_pubr()+ scale_x_continuous(limits = c(-180,180)) + scale_y_continuous(limits = c(-60,80))+ scale_fill_distiller(palette = "Spectral")
#multiplot ----
plot2 <- ggarrange(controlBiome,controlBA, LGMBiome, LGMBA, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
annotate_figure(plot2, top = text_grob("ORCHIDEE", 
                                       color = "black", face = "bold", size = 18))






