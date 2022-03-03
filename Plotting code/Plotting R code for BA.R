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
countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))
#LGM ice shapefile
shp_path <- "/Volumes/PL SSD/Shapefiles/lgm/"
shp_name <- "lgm.shp"
shp_file <- paste(shp_path, shp_name, sep="")

# read the shapefile
LGM_shp <- read_sf(shp_file)
ice <- as(st_geometry(LGM_shp), Class="Spatial")

plot(ice)


setwd("/Volumes/PL SSD/Fire models Jan 2021/Mean of all models/BA/Annual means/")
setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/")
#SPITFIRE 
meanBA <- raster("1951_1970_mean_BA.nc", var = 'BA')
stdBA <- raster("1951_1970_std_BA.nc", var = 'BA')
SPITBA <-raster("SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
SIMBA <- raster("SIMFIRE_1951_1970_BA_diff1.nc", var = 'burntArea.monthly')
ORCBA <-raster("ORCHIDEE_1951_1970_BA_diff1.nc", var = 'BA')
LMBA <- raster("LPJLM_1951_1970_BA_diff1.nc", var = 'burnedf')
#entities for plotting
entities <- read_csv("/Volumes/PL SSD/Fire models Jan 2021/Mean of all models/H1_H2entities.csv")
all_entities <- read_csv("/Volumes/PL SSD/Fire models Jan 2021/Mean of all models/all_entities.csv")
#mFuel
setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/mFuel/Model means/")
SPITmFuel <-raster("SPITFIRE_1951_1970_mFuel_diff.nc", var = 'mFuel1hr')
ORCmFuel <-raster("ORCHIDEE_1951_1970_mFuel_diff1.nc", var = 'mFuel')
#NPP
setwd("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/npp/Model means/")
SPITnpp <-raster("SPITFIRE_1951_1970_npp_diff.nc", var = 'npp')
ORCnpp <-raster("ORCHIDEE_1951_1970_npp_diff1.nc", var = 'npp')
SIMnpp <-raster("SIMFIRE_1951_1970_npp_diff1.nc", var = 'npp.monthly')
LMnpp <-raster("LPJLM_1951_1970_npp_diff1.nc", var = 'NPP')
meannpp <- raster("1951_1970_mean_npp.nc", var = 'npp')
stdnpp <- raster("1951_1970_std_npp.nc", var = 'npp')




mFuel_com <- stack(SPITmFuel,ORCmFuel)
names(mFuel_com) <- c('LPJ-GUESS SPITFIRE', 'ORCHIDEE')
BA_com <- stack(SPITBA,SIMBA,ORCBA,LMBA, meanBA, stdBA)
names(BA_com) <- c('LPJ-GUESS SPITFIRE', 'SIMFIRE-BLAZE', 'ORCHIDEE','LPJ-LM Fire', 'mean BA', 'BA std')
NPP_com <- stack(SPITnpp,SIMnpp,ORCnpp,LMnpp, meannpp, stdnpp)
names(NPP_com) <- c('LPJ-GUESS SPITFIRE', 'SIMFIRE-BLAZE', 'ORCHIDEE','LPJ-LM Fire', 'mean BA', 'BA std')

#plot points on raster map
x <- entities$LONGITUDE
y <- entities$LATITUDE
name <- entities$SITE_NAME
dummy2 <- data.frame(x, y, name)
coordinates(dummy2) <- ~ x + y
levelplot(mFuel_com, cuts=20, at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries)) + layer(sp.points(dummy,pch= 1,cex=0.1, col=1)) + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))



mean<-levelplot(BA_com, cuts=5, xlim=c(-0,50), ylim=c(-40,15), at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries)) + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))
SPIT<-levelplot(SPITBA, cuts=20, xlim=c(-0,50), ylim=c(-40,15), at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries))  + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))
SIM<-levelplot(SIMBA, cuts=20, xlim=c(-0,50), ylim=c(-40,15), at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries))   + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))
ORC<-levelplot(ORCBA, cuts=20,  xlim=c(-0,50), ylim=c(-40,15),at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries))   + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))
LPJLM<-levelplot(LMBA, cuts=20, xlim=c(-0,50), ylim=c(-40,15), at=cutpts, pretty=T, par.settings = myTheme2, main = 'LGM Burnt area') + layer(sp.lines(countries))  + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))
plots <- c(SPIT, SIM, ORC,LPJLM, mean)
print(plots)
??sp.points

??levelplot
levelplot(BA_com, at = cutpts, cuts = 20,xlim=c(20, 50), ylim=c(40,70), pretty=T, par.settings = myTheme, main = 'BA anomaly 1951-1970 reference') + layer(sp.lines(countries))
levelplot(mFuel_com, pretty=T, par.settings = myTheme_rev, main = 'mFuel anomaly 1951-1970 reference') + layer(sp.lines(countries))
levelplot(NPP_com, at = rev(cutpts2), cuts = 20, pretty=T, par.settings = myTheme, main = 'NPP anomaly 1951-1970 reference') + layer(sp.lines(countries))

levelplot(stdBA, cuts=20, pretty=T, par.settings = theme2, main = 'LGM Burnt area') + layer(sp.lines(countries))



cutpts <- c(50,40,30,20,10,1,0.1, -0.1, -1,-10,-20,-30,-40,-50)
cutpts2<- c(50,40,30,20,10,1,-1,-10,-20,-30,-40,-50)
cutpts2<- seq(-1, 1, by = 0.1)

cutpts3 <- c(3:0.00001, -0.00001:-3)
cutpts <- c(1:0.1, 0.0000000001, -0.0000000001, -0.1:-1)
cutpts_NPP <- c(3:-0.1, 0.0000000001, -0.0000000001, -0.1:-3)


# Set color palette for plot
zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 9)
blues <- rev(brewer.pal('Blues', n = 9))
purples <- brewer.pal('Purples', n = 9)
cutpts_1<-c(1.0,0.875,0.75, 0.625 ,0.5, 0.375, 0.25, 0.125, 0.00000001, -0.000000001, -0.125, -0.25, -0.375, -0.5, -0.625, -0.75, -0.875, -1.0)
cutpts<-c(50:1, 0.00000001, -0.000000001, -1:-50)

myTheme <- rasterTheme(region = c(blues, zeroCol, reds))
myTheme_rev <- rasterTheme(region = c(rev(reds), zeroCol, rev(blues)))
myTheme2 <- rasterTheme(region = c(blues, reds))

theme2 <- rasterTheme(region = c(purples))
#plot difference
brewer.pal.info


levelplot(BA, att=2, at=cutpts_1, cuts=20, pretty=T, par.settings = myTheme, main = 'SPITFIRE LGM-SF2') + layer(sp.lines(countries))
levelplot(s2, att=2, at=cutpts_1, cuts=20, pretty=T, par.settings = myTheme, main = 'SIMFIRE-BLAZE LGM-SF2') + layer(sp.lines(countries))



