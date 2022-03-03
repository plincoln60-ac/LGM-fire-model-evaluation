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

#number of variables in the data frame??
nt <- 1
lat <- as.numeric(seq(-56.25, 83.25, 0.5))

# create arrays
# nlon * nlat * nt array
fillvalue <- NA
BA_array <- array(fillvalue, dim=c(nlon,nlat,nt+2))
dim(BA_array)

# loop over the rows in the data frame 
# most explicit,
ptm <- proc.time() # time the loop
nobs <- dim(BA_agreement)[1]
for(i in 1:nobs) {
  
  # figure out location in the target array of the values in each row of the data frame
  j <- which.min(abs(lon-BA_agreement$lon[i]))
  k <- which.min(abs(lat-BA_agreement$lat[i]))
  
  # copy data from the data frame to array
  BA_array[j,k,nt] <- as.matrix(BA_agreement[i,nt+2]) #columns are troublesome here
}
dim(BA_array)
print(BA_array)
#plot to check data
library(lattice)
library(RColorBrewer)

grid <- expand.grid(lon=lon, lat=lat)

# Set color palette for plot
zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- brewer.pal('YlOrRd', n = 7)
blues <- rev(brewer.pal('Blues', n = 7))

myTheme <- rasterTheme(region = c(blues, reds))
#plot difference
cutpts <- c(1,2,3,4)
grid <- expand.grid(lon=lon, lat=lat)
BA_array <- ratify(BA_array)
levelplot(BA_array[1] ~ lon * lat, at = cutpts, data=grid) + layer(sp.lines(countries))
levelplot(BA_array ~ lon * lat, data=grid, at=cutpts, cuts=20, pretty=T, par.settings = myTheme) + layer(sp.lines(countries)) +  layer(sp.points(dummy2,pch= 1,cex=0.1, col=1)) + layer(sp.points(dummy2,pch= 1,cex=0.5, col=2))


library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- map_data("world")

#plot model agreement
ggplot(data = world) + geom_map(
  data = world, map = world,
  aes(long, lat, map_id = region), color = 'gray', fill = 'white') + geom_tile(data = BA_agreement, aes(x = lon, y = lat, fill = agreement)) +
  coord_quickmap() + theme_classic() 
 

