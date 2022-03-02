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


#open ncdf file
LGM_BA <- nc_open("/Volumes/PL SSD/Fire models Jan 2021/ORCHIDEE/LGM SF1/ORCHIDEE_LGM_npp.nc")
#correct the lat long issue----
ncpath <- '/Volumes/PL SSD/Fire models Jan 2021/ORCHIDEE/Georeffed/'
ncname <- "ORCHIDEE_LGM_npp_Nov21"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "mFuel"  # note: tmp means temperature (not temporary)
lon <- seq(-179, 180,2)
lat <- seq( -89, 90, 2)
lat<-rev(lat)
time <- seq(1, 1200)
nlon <- length(lon)
nlat <- length(lat)
ntime <- length(time)
lon1 <- ncdim_def("longitude", "degrees_east", lon)
lat2 <- ncdim_def("latitude", "degrees_north", lat)
coFire_array <-ncvar_get(LGM_BA, 'npp')
# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("time", "month", as.double(time))

# define variables
fillvalue <- 1e32
dlname <- "npp"
BA.def <- ncvar_def("npp","kg-1 m-2 s-1",list(londim,latdim,timedim),fillvalue,dlname,prec="single")


ncout <- nc_create(ncfname,list(BA.def),force_v4=TRUE)
ncvar_put(ncout, BA.def , coFire_array)


ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","Z")


nc_close(ncout)