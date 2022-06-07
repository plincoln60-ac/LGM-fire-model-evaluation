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
#import polygons
wd<-getwd()
Tropics <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Tropics.shp")
SAm_hpol <- readOGR(dsn = "/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Regional shapefiles/SAm_h.shp")
SAm_lpol <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/South_America_SE.shp")
Af_hpol <- readOGR(dsn = "/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Regional shapefiles/Af_h.shp")
Af_lpol <- readOGR(dsn = "/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Regional shapefiles/Af_l.shp")
SE_Asia <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/SE_Asia.shp")
Asian_tropics <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Asian_tropics.shp")
S_American_tropics <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/South_American_tropics.shp")
Europe <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Europe.shp")
Australia <-readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/SE_Australia.shp")
Global <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Globe.shp")
Af_pl <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/African_peak_low.shp")
Af_ph <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/African_peak_high.shp")
E_Ch <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Eastern_China.shp")
NAm_E <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/NAm_E.shp")
SAm_S <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Southern_S_America.shp")
#pick selected polygon to run

####################Create code into a function to run in multiples#################
#construct functions
LCFfunction <- function(df,model) {
  df <-  df %>% hyper_tibble(force = T) 
  df <- df[, c("TotVegfrac", "Grassfrac","Treesfrac", "lon","lat")]
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
region_plot_function<- function(pol, string){
  #upload raw data
  SPITBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
  SIMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc", var = 'BA')
  ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
  LMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/LPJLM_1951_1970_BA_diff1.nc", var = 'BA')
  SPITLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SPITFIRE/SPITFIRE.nc")
  SIMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SIMFIRE/SIMFIRE.nc")
  ORCLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/ORCHIDEE/ORCHIDEE.nc")
  LMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/LPJLM/LPJLM.nc")
  LGMdf <- read.csv('/Users/paullincoln/Dropbox/2021/Research/RPD time series/Regional_transformed_data_EGU/LGM_dataset.csv')
    #write data to dfs
  SPITBA <- BA_function(SPITBA, 'SPITFIRE') 
  SPITLCF <- LCFfunction(SPITLCF, 'SPITFIRE') 
  SIMBA <- BA_function(SIMBA, 'SIMFIRE') 
  SIMLCF <- LCFfunction(SIMLCF, 'SIMFIRE') 
  ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
  ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
  LMBA <- BA_function(LMBA, 'LPJLM') 
  LMLCF <- LCFfunction(LMLCF, 'LPJLM') 
  #output dfs containing all cell values within the polygon
  BAdat <- data.frame(rbind(SPITBA, SIMBA, ORCBA, LMBA))
  LCFdat <- data.frame(rbind(SPITLCF,SIMLCF,ORCLCF,LMLCF))
  
  BA_model <- sp_function(BAdat, pol, string)
  LCF_model <- LCFsp_function(LCFdat,pol)
  
  
  #RPD entities----
  #compile data from entities
  ###gather entity data from RPD
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')

  ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  ###make spatial point file
  
  xy <- ent[,c(4,3)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:4]
  #####upload RPD z scores

  
  #Subset data frame ----
  LGMdf <-merge(spdf, LGMdf, by =c('ID_ENTITY'))
  LGMdf <- LGMdf %>% dplyr::filter(EST_AGE <24000 & EST_AGE >17000)
 nentities<- length(unique(LGMdf$ID_ENTITY))
    zt <- na.omit(LGMdf$zt)
  zt<-mean(zt)
  #LGMdf <- LGMdf %>% filter(zt <10 & zt >-10)
  LGMdf$meanzt <- zt
  LGMdf$RPD <- 'RPD'
  
  LGMdfmeanzt<- LGMdf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')
  
  spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
  LGMdf<- LGMdf %>% drop_na(zt)
  spdf<- spdf %>% drop_na(meanzt)
  spdf$fill <- spdf$meanzt <0
  
  
  plotLGM <- LGMdf[c(3:4,12:14)]
  colnames(plotLGM) <- c('lat','lon','BA','meanBA','model')
  plotdf <-rbind(BA_model[c(1:4,6)],plotLGM)
  
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
  LCF_model$LCF_fraction <- factor( LCF_model$LCF_fraction, levels=c("TotVegfrac", "Treesfrac", "Grassfrac")) #re=order boxplots
  
 LCF <- ggplot(LCF_model, aes(Value, model, fill = LCF_fraction)) + 
    geom_boxplot(outlier.shape = NA, width = 0.75, alpha = 0.8) + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 10)+ 
    geom_vline(xintercept = 0, color = 'goldenrod1', size = 1.5)  +
   scale_x_continuous(limits = c(-1,1), name = '- <----------- LCF anomaly -----------> + ') +
    scale_fill_manual(values = c('Grassfrac' = 'khaki1', 'Treesfrac' = 'forestgreen', 'TotVegfrac' = 'olivedrab'), labels = 
                        c('Grass', 'Trees', 'Total Veg')) +
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.title.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(angle = 45, hjust = 1, size = 8),
          legend.position="bottom",
          panel.border = element_rect(color = "black",
                                      fill = NA))+
   labs(fill = 'LCF fraction')

 nent<- paste('Number of entities = ', nentities, sep = "")
 
 spdf$fill <- spdf$meanzt <0
 
  map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
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
    scale_y_continuous(limits = c(-55,85), breaks = seq(-60, 90, 20))+
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

entity_plot_function<- function(pol, string){
  #upload raw data
  LGMdf <- read.csv('/Users/paullincoln/Dropbox/2021/Research/RPD time series/Regional_transformed_data_EGU/LGM_dataset_4_8k_baseline.csv')
  #write data to dfs
  #RPD entities----
  #compile data from entities
  ###gather entity data from RPD
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  ###make spatial point file
  
  xy <- ent[,c(4,3)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:4]
  #####upload RPD z scores
  
  
  #Subset data frame ----
  LGMdf <-merge(spdf, LGMdf, by =c('ID_ENTITY'))
  LGMdf <- LGMdf %>% dplyr::filter(EST_AGE <24000 & EST_AGE >17000)
  LGMdf21 <- LGMdf %>% dplyr::filter(EST_AGE <22000 & EST_AGE >21000)
  
  nentities<- length(unique(LGMdf$ID_ENTITY))
  zt <- na.omit(LGMdf$zt)
  zt<-mean(zt)
 LGMdfmeanzt<- LGMdf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
 colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')
 
 
 
LGMdf<- merge(LGMdf, LGMdfmeanzt, by = 'entity_name')
spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
LGMdf$fill <- LGMdf$meanzt <0
LGMdf<- LGMdf %>% drop_na(zt)
spdf<- spdf %>% drop_na(meanzt)
spdf$fill <- spdf$meanzt <0

#LGMdf <- LGMdf %>% filter(meanzt !=NA)
boun <- data.frame(bbox(pol))

pcoords<- as.data.frame(pol@polygons[[1]]@Polygons[[1]]@coords)
colnames(pcoords) <- c('lon','lat')
  #plot
    RPD <- ggplot(LGMdf, aes(x=entity_name, y=zt, fill = fill),alpha = 0.5) + 
    geom_boxplot(outlier.shape = NA) + ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 6)+
    geom_hline(yintercept = 0, color = 'goldenrod1', size = 1.5)  +
    scale_y_continuous(limits = c(-10,10), name = '- <----------- Charcoal z_score -----------> + ') +
      scale_fill_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.title.x = element_blank(),
          legend.position="bottom",
          panel.border = element_rect(color = "black",
                                      fill = NA)) +
    labs(fill = 'Mean anomaly value')
    
    map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
      geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
      geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
      geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
      geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
      ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
      geom_point(data = spdf, aes(x = lon, y = lat, color = fill)) +
      scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
      geom_text(data = spdf, aes(x=lon, y=lat, label=entity_name), size = 2.5, hjust=0, vjust=-1) +
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

  multiplot <-  cowplot::ggdraw() +
    cowplot:: draw_plot(RPD,  x = 0.5, y = 0, width = .5, height = 1) +
    cowplot::  draw_plot(map, x = 0, y = 0.5, width = .5, height = .5)
    
    return(multiplot)
}
region_plot_function_4_8k_baseline_gpp <- function(pol, string){
  #upload raw data
  SPITBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
  SIMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc", var = 'BA')
  ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
  LMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/LPJLM_1951_1970_BA_diff1.nc", var = 'BA')
  SPITLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SPITFIRE/SPITFIRE.nc")
  SPITGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/SPITFIRE_gpp_anomaly_baseline.nc")
  SIMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SIMFIRE/SIMFIRE.nc")
  SIMGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/SIMFIRE_gpp_anomaly_baseline_remap.nc")
  ORCLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/ORCHIDEE/ORCHIDEE.nc")
  ORCGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_gpp_anomaly_baseline.nc")
  LMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/LPJLM/LPJLM.nc")
  LMNPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_npp_anomaly_baseline.nc")
  LGMdf <- read.csv('/Users/paullincoln/Dropbox/2021/Research/RPD time series/Regional_transformed_data_EGU/LGM_dataset_4_8k_baseline.csv')
  LGMdf <- LGMdf[1:9]
  #write data to dfs
  SIMGPP <- GPP_function(SIMGPP, 'SIMFIRE')
  SPITGPP <- GPP_function(SPITGPP, 'SPITFIRE')
  ORCGPP <- GPP_function(ORCGPP, 'ORCHIDEE')
  LMNPP <- NPP_function(LMNPP, 'LPJLM')
  SPITBA <- BA_function(SPITBA, 'SPITFIRE') 
  SPITLCF <- LCFfunction(SPITLCF, 'SPITFIRE') 
  SIMBA <- BA_function(SIMBA, 'SIMFIRE') 
  SIMLCF <- LCFfunction(SIMLCF, 'SIMFIRE') 
  ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
  ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
  LMBA <- BA_function(LMBA, 'LPJLM') 
  LMLCF <- LCFfunction(LMLCF, 'LPJLM') 
 #create functions functions for trees to grass fraction
 df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
 df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
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
df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF_remap.nc")
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
df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/SF2_LCF_1951_1970.nc")
df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF.nc")   
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
      grassland = sum(`10`,`11`, na.rm = TRUE),
      trees = sum(`2`:`9`, na.rm = TRUE))
  df <- df[c(1:2,14:15)]
  df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
  df2<-df2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      LGM_grassland = as.numeric(sum(`10`,`11`, na.rm = TRUE)),
      LGM_trees = as.numeric(sum(`2`:`9`, na.rm = TRUE)))
  df2 <- df2[c(1:2,16:17)]
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
df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LCF_1951_1970.nc")
df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LGM_landcoverfrac_90yr.nc")      
    LPJLMt_g<- function(df,df2) {
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
      grassland = as.numeric(sum(`8`,`9`, na.rm = TRUE)),
      trees = as.numeric(sum(`1`:`7`, na.rm = TRUE)))
  df <- df[c(1:2,12:13)]
  df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
  df2<-df2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      LGM_grassland = as.numeric(sum(`8`,`9`, na.rm = TRUE)),
      LGM_trees = as.numeric(sum(`1`:`7`, na.rm = TRUE)))
  df2 <- df2[c(1:2,12:13)]
  df <- merge(df,df2, by = c('lat', 'lon'))
  df$SF2_trees_grass_frac <- df$trees / df$grassland
  df$LGM_trees_grass_frac <- df$LGM_trees / df$LGM_grassland
  df$trees_grass_anomaly <- df$LGM_trees_grass_frac -  df$SF2_trees_grass_frac
  df$SF2_grass_trees_frac <- df$grassland / df$trees
  df$LGM_grass_trees_frac <- df$LGM_grassland / df$LGM_trees
  df$grass_trees_anomaly <- df$LGM_grass_trees_frac -  df$SF2_grass_trees_frac
  df <- df[c(1:2, 9,12)]
  df$model <- 'LPJLM'
  return(df)
}
    LPJLM_ratio <-  LPJLMt_g(df, df2)
    #output dfs containing all cell values within the polygon
  varbind <- function(df, df1,df2) {
  df3  <- merge(df, df1, by = c('lat', 'lon', 'model'))
  df4 <- merge(df3,df2, by = c('lat','lon','model'))
  return(df4)
  }
SPITvar <- varbind(SPITLCF,SPITGPP, SPIT_ratio)
SIMvar <- varbind(SIMLCF,SIMGPP, SIM_ratio)
ORCvar <- varbind(ORCLCF,ORCGPP,ORC_ratio)
LMvar <- varbind(LMLCF,LMNPP,LPJLM_ratio)

  BAdat <- data.frame(rbind(SPITBA, SIMBA, ORCBA, LMBA))
  LCFdat <- data.frame(rbind(SPITvar,SIMvar,ORCvar,LMvar))
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
  
  ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  ###make spatial point file
  
  xy <- ent[,c(4,3)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:4]
  #####upload RPD z scores
  
  
  #Subset data frame ----
  LGMdf <-merge(spdf, LGMdf, by =c('ID_ENTITY'))
  LGMdf <- LGMdf %>% dplyr::filter(EST_AGE <24000 & EST_AGE >17000)
  nentities<- length(unique(LGMdf$ID_ENTITY))
  zt <- na.omit(LGMdf$zt)
  zt<-mean(zt)
  #LGMdf <- LGMdf %>% filter(zt <10 & zt >-10)
  LGMdf$meanzt <- zt
  LGMdf$RPD <- 'RPD'
  
  LGMdfmeanzt<- LGMdf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')
  
  spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
  LGMdf<- LGMdf %>% drop_na(zt)
  spdf<- spdf %>% drop_na(meanzt)
  spdf$fill <- spdf$meanzt <0
  
  
  plotLGM <- LGMdf[c(3:4,12:14)]
  colnames(plotLGM) <- c('lat','lon','BA','meanBA','model')
  plotdf <-rbind(BA_model[c(1:4,6)],plotLGM)
  
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
  

  spdf$fill <- spdf$meanzt <0
  
  map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
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
region_plot_function_4_8k_baseline_map <- function(pol, string){
  #upload raw data
  SPITBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SPITFIRE_1951_1970_BA_diff.nc", var = 'BA')
  SIMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/SIMFIRE_1951_1970_BA_diff1.nc", var = 'BA')
  ORCBA <-tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline_remap_mean.nc", var = 'BA')
  LMBA <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/BA/Model means/LPJLM_1951_1970_BA_diff1.nc", var = 'BA')
  SPITLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SPITFIRE/SPITFIRE.nc")
  SPITGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/SPITFIRE_gpp_anomaly_baseline.nc")
  SIMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/SIMFIRE/SIMFIRE.nc")
  SIMGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/SIMFIRE_gpp_anomaly_baseline_remap.nc")
  ORCLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/ORCHIDEE/ORCHIDEE.nc")
  ORCGPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_gpp_anomaly_baseline.nc")
  LMLCF <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/By model for xy plots/LPJLM/LPJLM.nc")
  LMNPP <- tidync("/Users/paullincoln/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_npp_anomaly_baseline.nc")
  LGMdf <- read.csv('/Users/paullincoln/Dropbox/2021/Research/RPD time series/Regional_transformed_data_EGU/LGM_dataset_4_8k_baseline.csv')
  LGMdf <- LGMdf[1:9]
  #write data to dfs
  SIMGPP <- GPP_function(SIMGPP, 'SIMFIRE')
  SPITGPP <- GPP_function(SPITGPP, 'SPITFIRE')
  ORCGPP <- GPP_function(ORCGPP, 'ORCHIDEE')
  LMNPP <- NPP_function(LMNPP, 'LPJLM')
  SPITBA <- BA_function(SPITBA, 'SPITFIRE') 
  SPITLCF <- LCFfunction(SPITLCF, 'SPITFIRE') 
  SIMBA <- BA_function(SIMBA, 'SIMFIRE') 
  SIMLCF <- LCFfunction(SIMLCF, 'SIMFIRE') 
  ORCBA <- BA_function(ORCBA, 'ORCHIDEE')
  ORCLCF <- LCFfunction(ORCLCF, 'ORCHIDEE') 
  LMBA <- BA_function(LMBA, 'LPJLM') 
  LMLCF <- LCFfunction(LMLCF, 'LPJLM') 
  #create functions functions for trees to grass fraction
  df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LCF_1951_1970.nc")
  df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SPITFIRE/SPITFIRE_LGM_LCF.nc")
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
  df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/SIMFIRE/SIMFIRE_LCF_remap.nc")
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
  df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/SF2_LCF_1951_1970.nc")
  df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/ORCHIDEE/ORCHIDEE_LGM_LCF.nc")   
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
        grassland = sum(`10`,`11`, na.rm = TRUE),
        trees = sum(`2`:`9`, na.rm = TRUE))
    df <- df[c(1:2,14:15)]
    df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
    df2<-df2 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        LGM_grassland = as.numeric(sum(`10`,`11`, na.rm = TRUE)),
        LGM_trees = as.numeric(sum(`2`:`9`, na.rm = TRUE)))
    df2 <- df2[c(1:2,16:17)]
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
  df <-   tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LCF_1951_1970.nc")
  df2 <- tidync::tidync("/Users/paullincoln/Dropbox/2022/Research/LGM paper & new code/Biomes/LPJLM/LPJLM_LGM_landcoverfrac_90yr.nc")      
  LPJLMt_g<- function(df,df2) {
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
        grassland = as.numeric(sum(`8`,`9`, na.rm = TRUE)),
        trees = as.numeric(sum(`1`:`7`, na.rm = TRUE)))
    df <- df[c(1:2,12:13)]
    df2 <- df2 %>% pivot_wider(names_from = "LGMvegtype", values_from = "LGMlandCoverFrac")
    df2<-df2 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        LGM_grassland = as.numeric(sum(`8`,`9`, na.rm = TRUE)),
        LGM_trees = as.numeric(sum(`1`:`7`, na.rm = TRUE)))
    df2 <- df2[c(1:2,12:13)]
    df <- merge(df,df2, by = c('lat', 'lon'))
    df$SF2_trees_grass_frac <- df$trees / df$grassland
    df$LGM_trees_grass_frac <- df$LGM_trees / df$LGM_grassland
    df$trees_grass_anomaly <- df$LGM_trees_grass_frac -  df$SF2_trees_grass_frac
    df$SF2_grass_trees_frac <- df$grassland / df$trees
    df$LGM_grass_trees_frac <- df$LGM_grassland / df$LGM_trees
    df$grass_trees_anomaly <- df$LGM_grass_trees_frac -  df$SF2_grass_trees_frac
    df <- df[c(1:2, 9,12)]
    df$model <- 'LPJLM'
    return(df)
  }
  LPJLM_ratio <-  LPJLMt_g(df, df2)
  #output dfs containing all cell values within the polygon
  varbind <- function(df, df1,df2) {
    df3  <- merge(df, df1, by = c('lat', 'lon', 'model'))
    df4 <- merge(df3,df2, by = c('lat','lon','model'))
    return(df4)
  }
  SPITvar <- varbind(SPITLCF,SPITGPP, SPIT_ratio)
  SIMvar <- varbind(SIMLCF,SIMGPP, SIM_ratio)
  ORCvar <- varbind(ORCLCF,ORCGPP,ORC_ratio)
  LMvar <- varbind(LMLCF,LMNPP,LPJLM_ratio)
  
  BAdat <- data.frame(rbind(SPITBA, SIMBA, ORCBA, LMBA))
  LCFdat <- data.frame(rbind(SPITvar,SIMvar,ORCvar,LMvar))
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
  
  ent <-  dbGetQuery(mydb, "select e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.mean), min(am.mean) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        where am.mean between 17000 and 24000
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  ###make spatial point file
  
  xy <- ent[,c(4,3)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:4],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:4]
  #####upload RPD z scores
  
  
  #Subset data frame ----
  LGMdf <-merge(spdf, LGMdf, by =c('ID_ENTITY'))
  LGMdf <- LGMdf %>% dplyr::filter(EST_AGE <24000 & EST_AGE >17000)
  nentities<- length(unique(LGMdf$ID_ENTITY))
  zt <- na.omit(LGMdf$zt)
  zt<-mean(zt)
  #LGMdf <- LGMdf %>% filter(zt <10 & zt >-10)
  LGMdf$meanzt <- zt
  LGMdf$RPD <- 'RPD'
  
  LGMdfmeanzt<- LGMdf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')
  
  spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
  LGMdf<- LGMdf %>% drop_na(zt)
  spdf<- spdf %>% drop_na(meanzt)
  spdf$fill <- spdf$meanzt <0
  
  
  plotLGM <- LGMdf[c(3:4,12:14)]
  colnames(plotLGM) <- c('lat','lon','BA','meanBA','model')
  plotdf <-rbind(BA_model[c(1:4,6)],plotLGM)
  
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
  
  
  spdf$fill <- spdf$meanzt <0
  
  map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
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
  return(map)
  
}


Trop <- region_plot_function_4_8k_baseline_gpp(Tropics, 'Tropics')
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



