library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tidync)
library(tidyr)
library(easyNCDF)
library(maps)
library(maptools)
library(rasterVis)
library(sp)
library(lattice)
library(RColorBrewer)
library(geosphere)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Rfast)
library(zoo)
library(sf)
library(readr)
#world map shapefile
rm(list = ls())
countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))


# Set color palette for plot
zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 9)
blues <- rev(brewer.pal('Blues', n = 9))
purples <- rev(brewer.pal('Purples', n = 9))
greens <-brewer.pal('Greens', n = 9)
oranges <- rev(brewer.pal('Oranges', n = 9))
greys <-rev(brewer.pal('Greys', n = 9))
cutpts <- c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1, 0.01, -0.01,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9, -1)
myTheme <- rasterTheme(region = c(blues, zeroCol, reds))
#entities for plotting----
entities <- read_csv("/Volumes/PL SSD/Fire models Jan 2021/Mean of all models/H1_H2entities.csv")
all_entities <- read_csv("/Volumes/PL SSD/Fire models Jan 2021/Mean of all models/all_entities.csv")



setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/LCF/C3grass/")
SPITFIRE <- raster("C3_grass1.nc", var = 'SPITFIRE')
SIMFIRE <- raster("C3_grass1.nc", var = 'SIMFIRE')
ORCHIDEE <- raster("C3_grass1.nc", var = 'ORCHIDEE')
LPJLM <- raster("C3_grass1.nc", var = 'LPJLM')
mean <- raster("C3_grass1.nc", var = 'Mean')

C3grass <- stack(SPITFIRE,ORCHIDEE, LPJLM, mean)
names(C3grass) <- c('SPITFIRE', 'ORCHIDEE', 'LPJLM', 'Mean')


levelplot(C3grass, at=cutpts, cuts=20, pretty=T, par.settings = myTheme, main = 'C3 grasses') + layer(sp.lines(countries))


setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/LCF/C4grass/")
SPITFIRE <- raster("C4_grass1.nc", var = 'SPITFIRE')
SIMFIRE <- raster("C4_grass1.nc", var = 'SIMFIRE')
ORCHIDEE <- raster("C4_grass1.nc", var = 'ORCHIDEE')
LPJLM <- raster("C4_grass1.nc", var = 'LPJLM')
mean <- raster("C4_grass1.nc", var = 'Mean')

C4grass <- stack(SPITFIRE,ORCHIDEE, LPJLM, mean)
names(C4grass) <- c('SPITFIRE', 'ORCHIDEE', 'LPJLM', 'Mean')


levelplot(C4grass, at=cutpts, cuts=20, pretty=T, par.settings = myTheme, main = 'C4 grasses') + layer(sp.lines(countries))



setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/LCF/Tropical/")
SPITFIRE <- raster("Tropical1.nc", var = 'SPITFIRE')
SIMFIRE <- raster("Tropical1.nc", var = 'SIMFIRE')
ORCHIDEE <- raster("Tropical1.nc", var = 'ORCHIDEE')
LPJLM <- raster("Tropical1.nc", var = 'LPJLM')
mean <- raster("Tropical1.nc", var = 'Mean')

Tropical <- stack(SPITFIRE,SIMFIRE, ORCHIDEE, LPJLM, mean)
names(Tropical) <- c('SPITFIRE', 'SIMFIRE', 'ORCHIDEE', 'LPJLM', 'Mean')


levelplot(Tropical, at=cutpts, cuts=20, pretty=T, par.settings = myTheme, main = 'Tropical') + layer(sp.lines(countries))



setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/LCF/Boreal/")
SPITFIRE <- raster("Boreal1.nc", var = 'SPITFIRE')
SIMFIRE <- raster("Boreal1.nc", var = 'SIMFIRE')
ORCHIDEE <- raster("Boreal1.nc", var = 'ORCHIDEE')
LPJLM <- raster("Boreal1.nc", var = 'LPJLM')
mean <- raster("Boreal1.nc", var = 'Mean')

Boreal <- stack(SPITFIRE,SIMFIRE, ORCHIDEE, LPJLM, mean)
names(Boreal) <- c('SPITFIRE', 'SIMFIRE', 'ORCHIDEE', 'LPJLM', 'Mean')


levelplot(Boreal, at=cutpts, cuts=20, pretty=T, par.settings = myTheme, main = 'Boreal') + layer(sp.lines(countries))




#plot correlations
print(Trees)

Trees <- tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/Trees_mean.nc")
Grass <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/Grass_mean.nc")
C3Grass <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/C3grass_mean.nc")
C4Grass <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/C4grass_mean.nc")
Temperate <- tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/Temperate_mean.nc")
Total_Veg <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/TotVeg_mean.nc")
Boreal <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/Boreal_mean.nc")
Tropical<- tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Mean ensemble/Tropical_mean.nc")
BA <-tidync::tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/Land cover/Regression with BA/mean_annual_BA.nc")

print(BA)
Trees <- Trees %>% tidync::hyper_tibble(force = T) # write variable to a dataframe
Grass <- Grass %>% tidync::hyper_tibble(force = T)
C3Grass <- C3Grass %>% tidync::hyper_tibble(force = T)
C4Grass <- C4Grass %>% tidync::hyper_tibble(force = T)
Temperate<- Temperate %>% tidync::hyper_tibble(force = T)
Total_Veg<-Total_Veg %>% tidync::hyper_tibble(force = T)
Boreal<-Boreal %>% tidync::hyper_tibble(force = T)
Tropical <- Tropical %>% tidync::hyper_tibble(force = T)
BA <- BA %>% tidync::hyper_tibble(force = T) # write variable to a dataframe


comp_df <-data.frame(BA)
comp_df1 <- left_join(comp_df,Trees[,1], by = c('lon' = 'lon', 'lat' = 'lat')))








orc <- as.data.frame(ORCHIDEE_C4, ORCHIDEE_C3, xy = T)




#LPJ_LM
LPJLM_Fire <- tidync("LPJLM_diff_vegtypes_mean.nc")
print(SPITFIRE)
tidync(LPJLM_Fire) %>% activate(time)
LPJLM_Fire <- LPJLM_Fire %>% hyper_array(force = T)
LPJLM_Fire <- data.frame(LPJLM_Fire)
#SPITFIRE ----
SPITFIRE <- tidync("SPITFIRE_diff_vegtypes_mean.nc")
SPITFIRE <- SPITFIRE %>% hyper_tibble(force = T)

#SIMFIRE----
SIMFIRE <- tidync("SIMFIRE_diff_vegtypes_mean.nc")
SIMFIRE <- SIMFIRE %>% hyper_tibble(force = T)

####ORCHIDEE ----
ORCHIDEE <- tidync("ORCHIDEE_diff_vegtypes_mean.nc")


###LPJ_LM Fire----
LPJLM_Fire <- nc_open("LPJLM_diff_vegtypes_mean.nc")
Trees_frac <- ncvar_get(LPJLM_Fire, varid = 'Treesfrac')





