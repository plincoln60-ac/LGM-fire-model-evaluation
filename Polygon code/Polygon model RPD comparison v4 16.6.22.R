rm(list = ls())
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(tidync)
library(tidyr)
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
library(ggpubr)
library(purrr)
library(maps)
library(splancs)
library(RMySQL)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(stars)
#import polygons
wd<-getwd()
Tropics <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Tropics.shp")
Global <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Globe.shp")

#Create polygons from model agreement data NOT FINISHED 177.22 doesn't plot correctly 
mod_agr<-read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Model data/Model_agreement_data.csv')  #Convert first two columns as lon-lat and third as value     
polygon_df_function <- function(df){
  df <- rasterFromXYZ(df[c(2,3,10)])  #Convert first two columns as lon-lat and third as value  
  # make all values the same. Either do
  r <- df > -Inf
  # or alternatively
  # r <- reclassify(x, cbind(-Inf, Inf, 1))
  # convert to polygons (you need to have package 'rgeos' installed for this to work)
  pp <- rasterToPolygons(r, dissolve=TRUE)
  projstr <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #ensure spatial data are conssitent between shapefile & data
  proj4string(pp) <- projstr
  return(pp)
}
#mod_agr <- rasterFromXYZ(mod_agr[c(2,3,10)])  #Convert first two columns as lon-lat and third as value  


N_extratropics <- mod_agr %>% filter(lat >30)
N_extratropics <- polygon_df_function(N_extratropics)


S_S_America <- mod_agr %>% subset(Neg_Count >=2 & lon < -30 & lon > -120 & lat < 10)
S_S_America <- polygon_df_function(S_S_America)

C_Africa <- mod_agr %>% subset(Neg_Count >=2 & lon < 50 & lon > -50 & lat < 10)
C_Africa <- polygon_df_function(C_Africa)


####################Create code into a function to run in multiples#################
#construct functions
LCFfunction <- function(df,model) {
  df <-  ORCLCF %>% hyper_tibble(force = T) 
  colnames(df) <-  c("Totvegfrac", "Grassfrac","Treesfrac", "lon","lat")
  df <- df[, c("Totvegfrac", "Grassfrac","Treesfrac", "lon","lat")]
  df$Grass_Tree_ratio <- df$Grassfrac / df$Treesfrac
  df$model <- model
  return(df)
}
BA_function<-function(df,model) {
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('BA', 'lon', 'lat', 'model')
  return(df)
}
GPP_function <- function(df, model){
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('gpp', 'lon', 'lat', 'model')
  return(df)
}
NPP_function <- function(df, model) {
  df <- df %>% hyper_tibble(force = T) 
  df <- df[,-4]
  df$model <- model
  colnames(df) <- c('gpp', 'lon', 'lat', 'model')
  return(df) 
} ####note that colnames has been changed to gpp rather than npp for LPJLM here for the later functions....
sp_function <- function(df, sp, string){
  pts <- as(sp, "SpatialLinesDataFrame")  
  pts <- as.data.frame(as(pts, "SpatialPointsDataFrame"))
  pts <- pts[,(5:6)]
  colnames(pts) <- c('x','y')
  t<-matrix(pts)
  alldatxx <- df[,2:3]
  colnames(alldatxx) <- c('x', 'y')
  alldatxx$in.shape <-  1:nrow(df) %in% inpip(pts = alldatxx, t)
  df$output  <- alldatxx$in.shape
  colnames(df) <- c('BA','lon','lat','model', 'output')
  df <- df %>% dplyr::filter(df[5] == 'TRUE')
  mod_mean <- df %>%
    group_by(model) %>%
    dplyr:: summarise_at(vars(BA), list(name = mean))
  colnames(mod_mean) <- c('model', 'meanBA')
  df <- merge(df, mod_mean, by= 'model')
  return(df)
}
LCFsp_function <- function(df, sp){
  pts <- as(sp, "SpatialLinesDataFrame")  
  pts <- as.data.frame(as(pts, "SpatialPointsDataFrame"))
  pts <- pts[,(5:6)]
  colnames(pts) <- c('x','y')
  t<-matrix(pts)
  alldatxx <- df[,4:5]
  colnames(alldatxx) <- c('x', 'y')
  alldatxx$in.shape <-  1:nrow(alldatxx) %in% inpip(pts = alldatxx, t)
  df$output  <- alldatxx$in.shape
  df <- df %>% dplyr::filter(df[7] == 'TRUE')
  df <- pivot_longer(df,cols =1:3, names_to = "Variable", values_to = "Value")
  df <- df %>% drop_na()
  df$concat <- paste(df$Variable, df$model, sep=" ", collapse=NULL)
  df$concat<- stringr:: str_remove(df$concat, 'frac')
  dfmod_mean <- df %>%
    group_by(concat) %>%
    dplyr:: summarise_at(vars(Value), list(name = mean))
  colnames(dfmod_mean) <- c('concat', 'meanLCF')
  df <- merge(df, dfmod_mean, by= 'concat')
  return(df)
}
killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}
###########load plotting data#############
#LGM ice raster file
p <- raster('/Volumes/PL SSD/Shapefiles/LGM mask/LGM mask2.tif')
p[p==1] <- 'ice'

dfp <- as.data.frame(p, xy= T)
dfp2 <- dfp %>% filter(LGM_mask2 == 'ice')
colnames(dfp2) <- c('lon','lat','ice')
#coastlines
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",  destfile = 'coastlines.zip')
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
coastlines <- SpatialLinesDataFrame(coastlines,
                                    coastlines@data)


#################################################create model agreement plot#############################################
agreement_function <-function() {
  ############## Load up mean BA models & convert to dfs -----
  meanBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/1951_1970_mean_BA.nc")
  SPITBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_BA_anomaly_baseline_mean.nc")
  SIMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LPJ-GUESS-BLAZE_BA_anomaly_baseline_remap_mean.nc")
  ORCBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc")
  LMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/LPJ LM/LPJLM_1951_1970_BA_diff.nc")
  
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
  LMBA$LMBA <- LMBA$LMBA *100
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
  all_BA <- meanBA[,2:3]
  all_BA <- merge(x = all_BA, y = SPITBA, by = c("lon","lat"))
  all_BA <- merge(x = all_BA, y = SIMBA, by = c("lon","lat"))
  all_BA <- merge(x = all_BA, y = ORCBA, by = c("lon","lat"))
  #all_BA <- merge(x = all_BA, y = LMBA, by = c("lon","lat")) #Excluded LMBA
  
  ################ Calculate agreement all values positive, 0 or negative
  all_BA$Neg_Count <- rowSums(all_BA[,3:5]< 0)
  all_BA$Pos_Count <- rowSums(all_BA[,3:5]>0)
  all_BA$Zero <- rowSums(all_BA[,3:5]==0)
  
  all_BA$agreement <- pmax(all_BA$Neg_Count, all_BA$Pos_Count, all_BA$Zero)
  
  BA_agreement <- all_BA[, c(1:2, 9)]
  table(BA_agreement$agreement)
  BA_agreement$agreement <- as.character(BA_agreement$agreement)
  all_BA <- all_BA %>%
    dplyr::rowwise() %>% # this is key, so the operations are applied by row and not column
    dplyr::mutate(mod_agreement = dplyr::case_when(
      `agreement` == 3  & `Pos_Count` ==3 ~ "3 positive",
      `agreement` == 3  & `Neg_Count` ==3 ~ "3 negative",
      `agreement` == 3  & `Zero` ==3 ~ "3 no change",
      `agreement` == 2  & `Pos_Count` ==2 ~ "2 positive",
      `agreement` == 2  & `Neg_Count` ==2 ~ "2 negative",
      `agreement` == 2  & `Zero` ==2 ~ "2 no change",
      `agreement` == 1   ~ "no agreement",
      # CONDITION ~ VALUE, # this is how you add more categories
      TRUE ~ NA_character_ # This is the default category
    )) %>%
    dplyr::ungroup() # Removes the 'rowwise' property from the table
  BA_agreement <- all_BA[, c(1:2, 10)]
  
  ######## Plot output ----
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
  
  cutpts<- seq(-80,80, by=  10)
  cutpts <- rev(cutpts)
  zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
  reds <- RColorBrewer::brewer.pal('YlOrRd', n = 9)
  blues <- rev(RColorBrewer::brewer.pal('Blues', n = 9))
  myTheme <- rasterTheme(region = c(blues, zeroCol, reds))
  
  
  all_BA$agreement <- as.factor(all_BA$agreement)
  pallette <- c('2 negative' = 'skyblue1',
                '3 negative' = 'deepskyblue3',
                '2 positive' = 'tomato1',
                '3 positive' = 'tomato3',
                '2 no change' = 'gray80',
                '3 no change' = 'grey51',
                'no agreement' = 'chartreuse3')
  
  AGR <-ggplot(data=BA_agreement, aes(x=lon, y=lat)) +
    geom_tile(alpha = 0.9,aes(fill = mod_agreement)) + 
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') + scale_fill_manual(values = pallette)+
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+ ggpubr:: labs_pubr(base_size = 12)
  AGR <- AGR + guides(fill=guide_legend(title="Model anomaly agreement (n)"))
  
  return(AGR)
}
AGR_plot <-agreement_function()


region_plot_function_4_8k_baseline_gpp <- function(pol, string, df, maxval,minval){
  #upload raw data
 
 SPITBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
  SIMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc", var = 'BA')
  ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
  SPITLCF <- tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_veg_type_diff_normalised.nc")
  SPITGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/SPITFIRE_gpp_anomaly_baseline.nc")
  SIMLCF <- tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIREveg_type_diff_normalised.nc")
  SIMGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/SIMFIRE_gpp_anomaly_baseline_remap.nc")
  ORCLCF <- tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEEveg_type_diff_normalised.nc")
  ORCGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_gpp_anomaly_baseline.nc")
  LGMdf <- df
  #write data to dfs
  SIMGPP <- GPP_function(SIMGPP, 'SIMFIRE')
  SPITGPP <- GPP_function(SPITGPP, 'SPITFIRE')
  ORCGPP <- GPP_function(ORCGPP, 'ORCHIDEE')
  SPITBA <- BA_function(SPITBA, 'SPITFIRE') 
  SPITLCF <- LCFfunction(SPITLCF, 'SPITFIRE') 
  SIMBA <- BA_function(SIMBA, 'SIMFIRE') 
  SIMLCF <- LCFfunction(SIMLCF, 'SIMFIRE') 
  ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
  ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
 
 #create functions functions for trees to grass fraction
   df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970_normalised.nc")
 df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_LGM_normalised.nc")
   SPITt_g<- function(df,df2) {
     df <- df %>% tidync::hyper_tibble(force = T)
     df2<- df2 %>% tidync::hyper_tibble(force = T)
     df <- df[1:4]
     df2 <- df2[1:4]
     colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
     colnames(df2)<- c('LGMlandCoverFrac', 'lon', 'lat', 'LGMvegtype')
     df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
     df<-df %>%
       dplyr::rowwise() %>%
       dplyr::mutate(
         grassland = sum(`11`,`12`, na.rm = TRUE),
         trees = sum(`1`,`10`, na.rm = TRUE))
     df <- df[c(1:2,15:16)]
     df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
     df2<-df2 %>%
       dplyr::rowwise() %>%
       dplyr::mutate(
         LGM_grassland = sum(`11`,`12`, na.rm = TRUE),
         LGM_trees = sum(`1`,`10`, na.rm = TRUE))
     df2 <- df2[c(1:2,15:16)]
     df <- merge(df,df2, by = c('lat', 'lon'))
     df$SF2_trees_grass_frac <- df$trees / df$grassland
     df$LGM_trees_grass_frac <- df$LGM_trees / df$LGM_grassland
     df$trees_grass_anomaly <- df$LGM_trees_grass_frac -  df$SF2_trees_grass_frac
     df$SF2_grass_trees_frac <- df$grassland / df$trees
     df$LGM_grass_trees_frac <- df$LGM_grassland / df$LGM_trees
     df$grass_trees_anomaly <- df$LGM_grass_trees_frac -  df$SF2_grass_trees_frac
     df <- df[c(1:2, 9,12)]
     df$model <- 'SPITFIRE'
     return(df)
   }
   SPIT_ratio <- SPITt_g(df, df2)
df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF_1951_1970_normalised.nc")
df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LGM_LCF_remap.nc")
    SIMt_g<- function(df,df2) {
     df <- df %>% tidync::hyper_tibble(force = T)
     df2<- df2 %>% tidync::hyper_tibble(force = T)
     df <- df[1:4]
     df2 <- df2[1:4]
     colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
     colnames(df2)<- c('LGMlandCoverFrac', 'LGMvegtype', 'lon', 'lat')
     df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")
     df<-df %>%
       dplyr::rowwise() %>%
       dplyr::mutate(
             grassland = as.numeric(sum(`10`,`11`, na.rm = TRUE)),
             trees = as.numeric(sum(`1`:`9`, na.rm = TRUE)))
     df <- df[c(1:2,14:15)]
     df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
     df2<-df2 %>%
       dplyr::rowwise() %>%
       dplyr::mutate(
         LGM_grassland = as.numeric(sum(`10`,`11`, na.rm = TRUE)),
         LGM_trees = as.numeric(sum(`1`:`9`, na.rm = TRUE)))
     df2 <- df2[c(1:2,14:15)]
     df <- merge(df,df2, by = c('lat', 'lon'))
     df$SF2_trees_grass_frac <- df$trees / df$grassland
     df$LGM_trees_grass_frac <- df$LGM_trees / df$LGM_grassland
     df$trees_grass_anomaly <- df$LGM_trees_grass_frac -  df$SF2_trees_grass_frac
     df$SF2_grass_trees_frac <- df$grassland / df$trees
     df$LGM_grass_trees_frac <- df$LGM_grassland / df$LGM_trees
     df$grass_trees_anomaly <- df$LGM_grass_trees_frac -  df$SF2_grass_trees_frac
     df <- df[c(1:2, 9,12)]
     df$model <- 'SIMFIRE'
     return(df)
   }
    SIM_ratio <- SIMt_g(df, df2)
df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LCF_1951_1970_normalised.nc")
df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF_matching_var.nc")   
    ORCt_g<- function(df,df2) {
  df <- df %>% tidync::hyper_tibble(force = T)
  df2<- df2 %>% tidync::hyper_tibble(force = T)
  df <- df[1:4]
  df2 <- df2[1:4]
  colnames(df)<- c('landCoverFrac', 'lon', 'lat', 'vegtype')
  colnames(df2)<- c('LGMlandCoverFrac', 'lon', 'lat', 'LGMvegtype')
  df <- df %>% pivot_wider(names_from = "vegtype", values_from = "landCoverFrac")

  df<-df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grassland = as.numeric(sum(`9`,`10`, na.rm = TRUE)),
      trees = as.numeric(sum(`1`:`8`, na.rm = TRUE)))
  df <- df[c(1:2,13:14)]
  df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
  df2<-df2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      LGM_grassland = as.numeric(sum(`10`,`11`, na.rm = TRUE)),
      LGM_trees = as.numeric(sum(`2`:`9`, na.rm = TRUE)))
  df2 <- df2[c(1:2,13:14)]
  df <- merge(df,df2, by = c('lat', 'lon'))
  df$SF2_trees_grass_frac <- df$trees / df$grassland
  df$LGM_trees_grass_frac <- df$LGM_trees / df$LGM_grassland
  df$trees_grass_anomaly <- df$LGM_trees_grass_frac -  df$SF2_trees_grass_frac
  df$SF2_grass_trees_frac <- df$grassland / df$trees
  df$LGM_grass_trees_frac <- df$LGM_grassland / df$LGM_trees
  df$grass_trees_anomaly <- df$LGM_grass_trees_frac -  df$SF2_grass_trees_frac
  df <- df[c(1:2, 9,12)]
  df$model <- 'ORCHIDEE'
  return(df)
}
    ORC_ratio <- ORCt_g(df, df2)

    #output dfs containing all cell values within the polygon
  varbind <- function(df, df1,df2) {
  df3  <- merge(df, df1, by = c('lat', 'lon', 'model'))
  df4 <- merge(df3,df2, by = c('lat','lon','model'))
  return(df4)
  }
SPITvar <- varbind(SPITLCF,SPITGPP, SPIT_ratio)
SIMvar <- varbind(SIMLCF,SIMGPP, SIM_ratio)
ORCvar <- varbind(ORCLCF,ORCGPP,ORC_ratio)

  BAdat <- data.frame(rbind(SPITBA, SIMBA, ORCBA))
  LCFdat <- data.frame(rbind(SPITvar,SIMvar,ORCvar))
  LCFsp_functionv2 <- function(df, sp){

    
    pts <- as(sp, "SpatialLinesDataFrame")  
    pts <- as.data.frame(as(pts, "SpatialPointsDataFrame"))
    pts <- pts[,(5:6)]
    colnames(pts) <- c('x','y')
    t<-matrix(pts)
    alldatxx <- df[,c(2,1)]
    colnames(alldatxx) <- c('x', 'y')
    alldatxx$in.shape <-  1:nrow(alldatxx) %in% inpip(pts = alldatxx, t)
    df$output  <- alldatxx$in.shape
    df <- df %>% dplyr::filter(df$output == 'TRUE')
    df <- pivot_longer(df,cols =c(5:6,8:9), names_to = "Variable", values_to = "Value")
    df <- df %>% drop_na()
    df$concat <- paste(df$Variable, df$model, sep=" ", collapse=NULL)
    df$concat<- stringr:: str_remove(df$concat, 'frac')
    dfmod_mean <- df %>%
      group_by(concat) %>%
      dplyr:: summarise_at(vars(Value), list(name = mean))
    colnames(dfmod_mean) <- c('concat', 'mean_value')
    df <- merge(df, dfmod_mean, by= 'concat')
    return(df)
  }
    BA_model <- sp_function(BAdat, pol, string)
  LCF_model <- LCFsp_functionv2(LCFdat,pol)

    #RPD entities----
  #compile data from entities
  ###gather entity data from RPD
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  e <-unique(LGMdf$ID_ENTITY) #subset entity file by those in the input data frame
  
  ent <- subset(ent, ID_ENTITY %in% e)
  ###make spatial point file
  
  xy <- ent[,c(4,3)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 # projstr <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #ensure spatial data are conssitent between shapefile & data
 # proj4string(pol) <- projstr
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:4]
  #####upload RPD z scores
  

  #Subset data frame ----
  #LGMdf <-merge(spdf, LGMdf, by ='ID_ENTITY')
  LGMdf <- LGMdf %>% dplyr::filter(median_age <maxval & median_age >minval)
  nentities<- length(unique(LGMdf$ID_ENTITY))
  ztm<- mean(LGMdf$zt) #calculate mean of all entities zt
  LGMdf$overall_mean <-ztm

  LGMdfmeanzt <- LGMdf[c(15:16)] #copy meanzt to spdf for plotting
  LGMdfmeanzt <-unique(LGMdfmeanzt)
  
  spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
  
  spdf$fill <- spdf$meanzt <0
  
  plotLGM <- LGMdf[c(18:19,14,21)]
  plotLGM$model <- 'RPD'
  colnames(plotLGM) <- c('lat','lon','BA','meanBA','model')
  plotdf <-rbind(BA_model[c(1:4,6)],plotLGM) #bind rpd data into models for plotting
  
  #work out bounding box coordinates of polygon
  boun <- data.frame(bbox(pol))
  
  pcoords<- as.data.frame(pol@polygons[[1]]@Polygons[[1]]@coords)
  colnames(pcoords) <- c('lon','lat')
  #plot
  
  plotdf$fill <- plotdf$meanBA <0
  plotdf$model <- factor( plotdf$model  , levels=c("LPJLM", "ORCHIDEE", "SIMFIRE","SPITFIRE", "RPD")) #re=order boxplots
  RPD <- ggplot(plotdf, aes(x=BA, y=model, fill = fill),alpha = 0.5) + 
    geom_boxplot(outlier.shape = NA) + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 10)+
    geom_vline(xintercept = 0, color = 'goldenrod1', size = 1.5)  +
    scale_x_continuous(limits = c(-10,10), name = '- <----------- BA/Charcoal anomaly -----------> + ') +
    scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.title.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(angle = 45, hjust = 1, size = 8),
          legend.position="bottom",
          panel.border = element_rect(color = "black",
                                      fill = NA)) +
    labs(fill = 'Mean anomaly value')
  
  # LCF <- ggplot(LCF_model, aes(x=Value, y=concat, fill = fill)) + geom_boxplot(outlier.shape = NA) + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 10)+
  #    geom_vline(xintercept = 0, color = 'red') +labs(title='Model Land cover fraction anomaly') +
  ##   scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"))+
  #  theme(axis.text.x=element_blank(), #remove x axis labels
  #       axis.ticks.x=element_blank(),
  #      axis.text.y = element_text(angle = 45, hjust = 1, size = 6),
  #     legend.position="none") #remove x axis ticks
  
  LCF_model$LCF_fraction <- as.factor(LCF_model$Variable)
  LCF_model$LCF_fraction <- factor(LCF_model$LCF_fraction, levels=c("grass_trees_anomaly","trees_grass_anomaly", "Treesfrac", "Grassfrac", "gpp")) #re=order boxplots
unique( LCF_model$LCF_fraction)
    LCF <- ggplot(LCF_model, aes(Value, model, fill = LCF_fraction)) + 
    geom_boxplot(outlier.shape = NA, width = 0.75, alpha = 0.8) + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 10)+ 
    geom_vline(xintercept = 0, color = 'goldenrod1', size = 1.5)  +
    scale_x_continuous(limits = c(-1,1), name = '- <----------- LCF anomaly -----------> + ') +
    scale_fill_manual(values = c('Grassfrac' = 'khaki1', 'Treesfrac' = 'forestgreen', 'trees_grass_anomaly' = 'tomato1', 'gpp' = 'violetred2'), labels = 
                        c('Grass', 'Trees', 'Tree:Grass ratio', 'gpp')) +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.title.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(angle = 45, hjust = 1, size = 8),
          legend.position="bottom",
          panel.border = element_rect(color = "black",
                                      fill = NA))+
    labs(fill = 'LCF fraction')
  
  
  spdf$col_2 <- spdf$meanzt <0
   
  
  map <- AGR_plot + geom_point(data = spdf, aes(x = lon, y = lat, color = col_2), size =2) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
   geom_polygon(data = pcoords, aes(x=lon,y=lat), color = 'purple', fill = NA) +
    scale_x_continuous(limits = c(boun[1,1]-4.75, boun[1,2]+4.75),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-4.75, boun[2,2]+4.75), breaks = seq(-60, 90, 20))+
    theme(axis.text.x=element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          legend.position = 'none',
          panel.border = element_rect(color = "black",
                                      fill = NA))

  glob <- ggplot(data=BA_model, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey34') +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 10)+ 
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    geom_polygon(data = pcoords, aes(x=lon,y=lat), alpha= 0.5, color = 'black', fill = 'purple') +
    scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(-56,85), breaks = seq(-60, 90, 20))+
    theme(axis.text.x=element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(), 
          panel.border = element_rect(color = "black",
                                      fill = NA))
  
  
  library("gridExtra")
  multimap <- grid.arrange(arrangeGrob(glob,map, ncol = 2),                          # First row with one plot spaning over 2 columns
                           arrangeGrob(LCF, RPD, ncol=2), # Second row with 2 plots in 2 different columns
                           nrow=2)  
  multimap <- annotate_figure(multimap, top = text_grob(string, 
                                                        color = "black", face = "bold", size = 14))
  killDbConnections()
  return(multimap)
  
}


Iteration_A <- read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_A.csv')
Iteration_A_dat <- c('20500','21500')
Iteration_B <- read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_B.csv')
Iteration_B_C_dat <- c('17480','23290')
Iteration_C <- read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_C.csv')

Trop <- region_plot_function_4_8k_baseline_gpp(Tropics, 'Tropics', Iteration_B, Iteration_B_C_dat[2], Iteration_B_C_dat[1])


Trop <- region_plot_function_4_8k_baseline_gpp(C_Africa, 'Tropics', Iteration_B, Iteration_B_C_dat[2], Iteration_B_C_dat[1])

Trop <- region_plot_function_4_8k_baseline_gpp(S_S_America, 'Tropics', Iteration_B, Iteration_B_C_dat[2], Iteration_B_C_dat[1])


Iteration_A_map <- region_plot_function_4_8k_baseline_gpp(Global, 'Global_Iteration A', Iteration_A, Iteration_A_dat[2], Iteration_A_dat[1])
Iteration_B_map <- region_plot_function_4_8k_baseline_gpp(S_S_America, 'South America', Iteration_B, Iteration_B_C_dat[2], Iteration_B_C_dat[1])
Iteration_C_map <- region_plot_function_4_8k_baseline_gpp(Global, 'Global_Iteration C', Iteration_C, Iteration_B_C_dat[2], Iteration_B_C_dat[1])
multimap <- grid.arrange(arrangeGrob(Iteration_A_map,Iteration_B_map, ncol = 2),                          # First row with one plot spaning over 2 columns
                         arrangeGrob(Iteration_C_map, ncol=2), # Second row with 2 plots in 2 different columns
                         nrow=2)  
multimap <- annotate_figure(multimap, top = text_grob(string, 
                                                      color = "black", face = "bold", size = 14))
Iteration_B_map








SAM<-region_plot_function_4_8k_baseline_gpp(SAm_lpol, 'Eastern South America')
SAMh <-region_plot_function_4_8k_baseline_gpp(SAm_hpol, 'South America high dipole')
AUS <-region_plot_function_4_8k_baseline_gpp(Australia, 'Australia')
EUR<-region_plot_function_4_8k_baseline_gpp(Europe, 'Southern Europe')
AS_t <- region_plot_function_4_8k_baseline_gpp(Asian_tropics, 'East Asian Tropics')
region_plot_function_4_8k_baseline_gpp(S_American_tropics, 'South American Tropics')

SEAS<-region_plot_function_4_8k_baseline_gpp(SE_Asia, 'Equatorial SE Asia')
region_plot_function_4_8k_baseline_gpp(Af_pl, 'Southern Africa')
region_plot_function(Af_ph, 'Central Africa')

region_plot_function_4_8k_baseline_gpp(Global, 'Globe')
region_plot_function_4_8k_baseline_gpp(E_Ch, 'Eastern China')
region_plot_function_4_8k_baseline_gpp(NAm_E, 'Eastern North America')


region_plot_function_4_8k_baseline_map(Global, 'Globe')


region_plot_function_4_8k_baseline_gpp(Global, 'Globe')

entity_plot_function(Global, 'Globe')


