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
library(ggpattern)
library(BSDA)
library(ggpattern)
library(rhdf5)
library(tidyverse)
library(data.table)


GFED_function <- function(){
ann_function<- function(hdf_file, yr){
# get lons and lats
lon_array <- h5read(hdf_file, "/lon")
lon <- lon_array[,1]
nlon <- length(lon)
lat_array <- h5read(hdf_file, "/lat")
lat <- lat_array[1,]
nlat <- length(lat)

lonlat <- as.matrix(expand.grid(lon,lat))

hdf_function<- function(dest, mon) {
  hdf <- array(h5read(hdf_file, dest))
  vec <- as.vector(hdf)
  hdf <- data.frame(cbind(lonlat,vec))
  names(hdf) <- c("lon","lat",mon)
  hdf2 <- hdf
  return(hdf2)
}


jan <- hdf_function("/burned_area/01/burned_fraction", 'January')
feb <- hdf_function("/burned_area/02/burned_fraction", 'February')
mar <- hdf_function("/burned_area/03/burned_fraction", 'March')
apr <- hdf_function("/burned_area/04/burned_fraction", 'April')
may <- hdf_function("/burned_area/05/burned_fraction", 'May')
jun <- hdf_function("/burned_area/06/burned_fraction", 'June')
jul <- hdf_function("/burned_area/07/burned_fraction", 'July')
aug <- hdf_function("/burned_area/08/burned_fraction", 'August')
sep <- hdf_function("/burned_area/09/burned_fraction", 'September')
oct <- hdf_function("/burned_area/10/burned_fraction", 'October')
nov <- hdf_function("/burned_area/11/burned_fraction", 'November')
dec <- hdf_function("/burned_area/12/burned_fraction", 'December')
merge(jan, feb, by = c('lat', 'lon'))
df_list<- list(jan, feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
year <- df_list %>% reduce(inner_join, by = c("lon", "lat"))                    # Apply reduce function of tidyverse
year$sum <- rowSums(year[3:14])
year <-year[c(1:2,15)]
colnames(year) <- (c('lon','lat', yr))
return(year)
}

GFED_1997 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1997.hdf5', '1997')
GFED_1998 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1998.hdf5', '1998')
GFED_1999 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1999.hdf5', '1999')
GFED_2000 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2000.hdf5', '2000')
GFED_2001 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2001.hdf5', '2001')
GFED_2002 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2002.hdf5', '2002')
GFED_2003 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2003.hdf5', '2003')
GFED_2004 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2004.hdf5', '2004')
GFED_2005 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2005.hdf5', '2005')
GFED_2006 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2006.hdf5', '2006')
GFED_2007 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2007.hdf5', '2007')
GFED_2008 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2008.hdf5', '2008')
GFED_2009 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2009.hdf5', '2009')
GFED_2010 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2010.hdf5', '2010')
GFED_2011 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2011.hdf5', '2011')
GFED_2012 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2012.hdf5', '2012')
GFED_2013 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2013.hdf5', '2013')
GFED_2014 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2014.hdf5', '2014')
GFED_2015 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2015.hdf5', '2015')
GFED_2016 <- ann_function('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_2016.hdf5', '2016')

df_list<- list(GFED_1997,GFED_1998,GFED_1999,GFED_2000,GFED_2001,GFED_2002,GFED_2003,GFED_2004,GFED_2005,GFED_2006,GFED_2007,GFED_2008,GFED_2009,GFED_2010,GFED_2011,GFED_2012,GFED_2013,GFED_2014,GFED_2015, GFED_2016)
year <- df_list %>% reduce(inner_join, by = c("lon", "lat"))                    # Apply reduce function of tidyverse
year$mean <- rowMeans(year[3:22])
year$mean <- year$mean *100 #convert mean into a percentage
year <- year[c(1:2,23)]
return(year)
} ##this function calculates the mean annual burnt area % between 1997 & 2016 from the GFED data, returning it as a data framee
GFED_function_MHa <- function(df){
  
  hdf_file <- '/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1997.hdf5'
  # get lons and lats
  ann_function<- function(hdf_file, yr){
    # get lons and lats
    lon_array <- h5read(hdf_file, "/lon")
    lon <- lon_array[,1]
    nlon <- length(lon)
    lat_array <- h5read(hdf_file, "/lat")
    lat <- lat_array[1,]
    nlat <- length(lat)
    
    lonlat <- as.matrix(expand.grid(lon,lat))
    
    hdf_function<- function(dest, mon) {
      hdf <- array(h5read(hdf_file, dest))
      vec <- as.vector(hdf)
      hdf <- data.frame(cbind(lonlat,vec))
      names(hdf) <- c("lon","lat",mon)
      hdf2 <- hdf
      return(hdf2)
    }
    
    
  
    
    ar <- hdf_function("/ancill/grid_cell_area", 'area')


  df2 <- cbind(df, hdf, by = c('lon','lat'))
  }
  

GFED_BA <- GFED_function()
GFED_MHA <- merge(GFED_BA, ar, by = c('lon', 'lat'))
GFED_MHA$area <- GFED_MHA$area /10000 #convert m2 to hectare
GFED_MHA$Mha <- GFED_MHA$area/1000000  #calculate millions of hectares
GFED_MHA$mha_burnt <- (GFED_MHA$mean/100) * GFED_MHA$Mha   #calculate millions of hectares burnt







#####make netcdf file
BA_mat <- as.matrix(GFED_BA[3])
dim(BA_mat)
lon <- h5read('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1997.hdf5', "/lon")
lon <- lon[,1]
nlon <- length(lon)
lat <- h5read('/Volumes/PL SSD/Fire models Jan 2021/GFED4/with small fires/GFED4.1s_1997.hdf5', "/lat")
lat <- lat[1,]
nlat <- length(lat)
nt <- 1
BA_mat <- array(BA_mat, dim=c(nlon,nlat,nt))


#correct the lat long issue----
ncpath <- '/Volumes/PL SSD/Fire models Jan 2021/GFED4/NetCDF files/'
ncname <- "GFED4_BA"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "BA"  # note: tmp means temperature (not temporary)

time <- seq(1)
ntime <- nt
lon1 <- ncdim_def("longitude", "degrees_east", lon)
lat2 <- ncdim_def("latitude", "degrees_north", lat)
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("time", "month", as.double(time))

# define variables
fillvalue <- 1e32
BA.def <- ncvar_def("BA","%",list(londim,latdim,timedim),fillvalue,dname,prec="single")
ncout <- nc_create(ncfname,list(BA.def),force_v4=TRUE)
ncvar_put(ncout, BA.def , BA_mat)

ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","Z")


nc_close(ncout)
















netcdf_function <-function(ad){
  BA <- tidync(ad)
  BA <- BA %>% hyper_tibble(force = T)
  return(BA)
}

BASEBA <-netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/baseline_BA_mean.nc")
SIMBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/baseline_BA_mean.nc")
ORCBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_BA_1951_1970_baseline.nc")
LPJBASE <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_1951_1970_baseline.nc")
LGMBA <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LGM_BA_mean.nc")
SIMLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LGM_BA_mean.nc")
ORCLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/LGM_BA_mean.nc")
LPJLGM <- netcdf_function("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LGM_BA_mean.nc")
Modras <- raster('~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/baseline_BA_mean.nc', var = 'BA')
SIMras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LGM_BA_mean.nc")
ORCras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/LGM_BA_mean.nc")
LPJras <- raster("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_1951_1970_baseline.nc")

MHa_function <- function(BASEBA, LGMBA, Modras) {
  
Modras<- terra::rast(Modras)
Ras_area <-terra::cellSize(Modras,  unit = 'ha')
Ras_area<- as.data.frame(Ras_area, xy = TRUE)

names(LGMBA) <- c('LGMBA','lon','lat','time')
names(BASEBA) <- c('BA','lon','lat','time')
names(Ras_area) <- c('lon','lat','hectares')

Model <- merge(BASEBA, Ras_area, by = c('lat','lon'))
Model <- merge(Model, LGMBA, by =c('lat','lon'))
Model$MHa <- Model$hectares / 1000000  #calculate millions of hectares
Model$mha_burnt <- (Model$BA/100) * Model$MHa   #calculate millions of hectares burnt
Model$LGM_mha_burnt <- (Model$LGMBA/100) * Model$MHa   #calculate millions of hectares burnt

baseline<-sum(Model$mha_burnt)
LGM<- sum(Model$LGM_mha_burnt)
anomaly<- LGM - baseline
return(c(LGM, baseline,anomaly))
}


SPITFIRE <- MHa_function(BASEBA, LGMBA, Modras)
SIMFIRE <- MHa_function(SIMBASE,SIMLGM,SIMras)
ORCHIDEE <- MHa_function(ORCBASE,ORCLGM,ORCras)
LPJLM <- MHa_function(LPJBASE,LPJLGM,LPJras)


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


ggplot(data=GFED_MHA, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = mean))  + scale_fill_continuous(type = "viridis") +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('GFED4 BA (%)')
