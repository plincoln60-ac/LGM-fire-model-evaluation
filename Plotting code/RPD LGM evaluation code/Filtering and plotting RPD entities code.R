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
Global <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/Globe.shp")
Aus <- readOGR(dsn = "/Users/paullincoln/Documents/GitHub/LGM-fire-model-evaluation/Polygon code/Regional shapefiles/SE_Australia.shp")

####################Create code into a function to run in multiples#################
#construct functions

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


LGMdf <- read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/RPD data/Z score dataframes/RPD_ZTrans_a_matching_Power.csv')
LGMdf_itc <- read.csv('/Users/paullincoln/Dropbox/2022/Publications/LGM paper/RPD data/Z score dataframes/RPD_ZTrans_c.csv')

Duplicate_function <- function(df, minval, maxval){
  #upload raw data

LGMdf <- df
 
  #RPD entities----
  #compile data from entities
  ###gather entity data from RPD
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_SITE, e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  un <-  dbGetQuery(mydb, "select e.ID_ENTITY, u.UNIT from entity e
                        left join unit u on e.ID_UNIT = u.ID_UNIT;;") 
  
  ent2 <- merge(ent[1:3],un, by = 'ID_ENTITY')
  ###make spatial point file
  
  xy <- ent[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:5],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[Global,])
  spdf <- spdf[1:5]
  
  #Subset data frame ----
  LGMdf <-merge(spdf, LGMdf, by =c('ID_ENTITY'))
  LGMdf_error <- LGMdf %>% dplyr::filter(median_age + UNCERT_75 <maxval & median_age - UNCERT_25 >minval)%>%
    drop_na(zt)
  LGMdf <- LGMdf %>% dplyr::filter(median_age <= maxval & median_age  >minval) %>%
    drop_na(zt)
   
  LGMdfmeanzt<- LGMdf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')
  ######################subset duplicates 
  nsamp <- LGMdf %>% group_by(ID_ENTITY) %>%tally() #count number of samples in LGM window
  spdf <- merge(spdf, LGMdfmeanzt, by = 'entity_name')
  spdf <- merge(spdf, ent2[3:4], by = c('entity_name'))
  duplicates <-subset(spdf,duplicated(ID_SITE)) #subset duplicated sites
  dup <- ent2 %>% filter(ID_SITE %in% duplicates$ID_SITE)
  dup <- merge(dup, nsamp, by = 'ID_ENTITY')
  
  return(dup)
} #this function outputs a dataframe consisting of duplicate entities from sites containing samples in the LGM window

Duplicates_Iteration_A <- Duplicate_function(LGMdf,20500,21500)
Duplicates_Iteration_A$inclusion <- c(0,1,0,1,1,0) #logical parameter to exclude unwanted duplicate entities
Duplicates_Iteration_B <- Duplicate_function(LGMdf,17480,23290)
Duplicates_Iteration_B$inclusion <- c(0,1,0,1,0,1,0,1,1,0) #logical parameter to exclude unwanted duplicate entities
Duplicates_Iteration_C <- Duplicate_function(LGMdf_itc,17480,23290)
Duplicates_Iteration_C$inclusion <- c(0,1,0,1,1,0) #logical parameter to exclude unwanted duplicate entities

Plotting_function_map <- function(df,dupdf,minval,maxval, basemin,basemax, title) {

f <- dupdf %>% filter(inclusion ==0)
df_comp <- df
mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')

ent <-  dbGetQuery(mydb, "select e.ID_SITE, e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 

xy <- ent[,c(5,4)]
spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:5],
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#filter which entities lie within the polygon
spdf <- data.frame(spdf[Global,])
spdf <- spdf[1:5]
spdf2 <- spdf 

##################################First filter for duplicates##################################
###############################################################################################
Ldf <-merge(spdf, df, by =c('ID_ENTITY'))
Ldf <- Ldf %>% dplyr::filter(median_age <= maxval & median_age  >minval ) %>%
  drop_na(zt) %>%
  subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
Ldf_error <-merge(spdf, df, by =c('ID_ENTITY'))
Ldf_error <- Ldf_error %>% dplyr::filter(median_age + UNCERT_75 <maxval & median_age - UNCERT_25 >minval)%>%
  drop_na(zt) %>%
  subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
##################################second filter for large mean z ##############################
###############################################entities########################################
LGMdfmeanzt<- Ldf %>%
  group_by(entity_name) %>%
  summarise_at(vars(zt), list(name = mean))
colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')  
Ldf <- merge(Ldf, LGMdfmeanzt, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
Ldf <- Ldf %>% filter(meanzt < 10)
spdf <- merge(LGMdfmeanzt, spdf, by = c('entity_name'))
spdf$fill<- spdf$meanzt <0

LGMdfmeanzt_error<- Ldf_error %>%
  group_by(entity_name) %>%
  summarise_at(vars(zt), list(name = mean))
colnames(LGMdfmeanzt_error) <-c('entity_name', 'meanzt')  
Ldf_error <- merge(Ldf_error, LGMdfmeanzt_error, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
Ldf_error <- Ldf_error %>% filter(meanzt < 10)
spdf_error <- merge(LGMdfmeanzt_error, spdf2, by = c('entity_name'))
spdf_error$fill<- spdf_error$meanzt <0
  #work out bounding box coordinates of polygon
  boun <- data.frame(bbox(Global))
  
  pcoords<- as.data.frame(Global@polygons[[1]]@Polygons[[1]]@coords)
  colnames(pcoords) <- c('lon','lat')
  #plot
 
time_series_plot <-df_comp %>% filter(ID_ENTITY %in% unique(Ldf$ID_ENTITY)) 
time_series_plot <- merge(time_series_plot, spdf, by = c('ID_ENTITY')) #add mean zt
time_series_plot$fill<- time_series_plot$meanzt <0

tseries <- ggplot(time_series_plot, aes(x= median_age/1000, y=zt, group = ID_ENTITY, color = fill)) +  geom_rect(aes(xmin = minval/1000, xmax = maxval/1000, ymin = -Inf, ymax = Inf),color = NA, fill = "grey79",  alpha = 0.2) +
  geom_rect(aes(xmin = basemin/1000, xmax = basemax/1000, ymin = -Inf, ymax = Inf), color = NA, fill = "grey79", alpha = 0.2) +
  geom_line(size= 0.25) + scale_color_manual(values = c('TRUE'= 'blue', 'FALSE' = 'red')) +scale_x_continuous(limits= c(0,24)) +  ggpubr:: theme_pubr() +ggpubr::labs_pubr(base_size = 6) +
  facet_wrap(as.factor(ID_ENTITY) ~ ., scales = "free_y") 


e <- 'n entities'
l <- length(spdf$ID_ENTITY)
enttext<-paste(c(e, sep = "=",l))
enttext <- paste(enttext, collapse = " ")
e <- 'n entities'
l <- length(spdf_error$ID_ENTITY)
enttext_error<-paste(c(e, sep = "=",l))
enttext_error <- paste(enttext_error, collapse = " ")


 Median_map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
    geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 3, size = 2) +
    geom_text(aes(x= -150, y=-55, label = enttext)) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                      fill = NA)) + ggtitle('Median_age')
 
 Median_map_error <- ggplot(data=spdf_error, aes(x=lon, y=lat)) +
   geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
   geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
   geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
   geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
   ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
   geom_point(data = spdf_error, aes(x = lon, y = lat, color = fill), size =2) +
   geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 3, size = 2) +
   geom_text(aes(x= -150, y=-55, label = enttext_error)) +
   scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
   geom_hline(yintercept = 0, col= 'tan4') +
   geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
   geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
   scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
   scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                     fill = NA)) + ggtitle('Median_accounting_for_age_error')
 
 
  multmap <-  ggarrange(Median_map, Median_map_error,
                        ncol = 1, nrow = 2,
                        heights = c(1))

    totmap <-ggarrange(multmap,tseries,
            ncol = 2, nrow = 1,
            widths = c(0.5,1))
  totmap <- annotate_figure(totmap, top = text_grob(title, 
                                        color = "black", face = "bold", size = 16))
  
  killDbConnections()
  return(totmap)
   
}
Plotting_function_map_3_samples_min <- function(df,dupdf,minval,maxval, basemin,basemax, title) {
  
  f <- dupdf %>% filter(inclusion ==0)
  df_comp <- df
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_SITE, e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;")                               #Get RPD entity data
  sam <- dbGetQuery(mydb,("select e.ID_ENTITY, am.median from age_model am
                          left join sample s on s.ID_SAMPLE = am.ID_SAMPLE
                          left join entity e on e.ID_ENTITY = s.ID_ENTITY;"))  ##Get age model data to subset by minimum 3 samples for the baseline
  
  sam_base <- sam %>% dplyr::filter(median >basemin & median <basemax) #calculate number of samples in the baseline
  sam_base_freq <- data.frame(table(sam_base$ID_ENTITY))
  colnames(sam_base_freq)[1] <- 'ID_ENTITY'

  
  xy <- ent[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:5],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[Global,])
  spdf <- spdf[1:5]
  spdf2 <- spdf 

    ##################################First filter for duplicates##################################
  ###############################################################################################
  Ldf <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf <- Ldf %>% dplyr::filter(median_age <maxval & median_age  >minval ) %>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  freq  <-  data.frame(table(Ldf$ID_ENTITY)) #exclude entities with <3 samples
  colnames(freq)[1] <- 'ID_ENTITY'
  Ldf <- merge(Ldf, freq, by = 'ID_ENTITY')
  Ldf$Freq <- as.numeric(Ldf$Freq)
  Ldf <- merge(Ldf, sam_base_freq, by = 'ID_ENTITY')
  Ldf <- Ldf %>% dplyr::filter(Freq.x >2 & Freq.y >2) 
  
  Ldf_error <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf_error <- Ldf_error %>% dplyr::filter(median_age + UNCERT_75 <maxval & median_age - UNCERT_25 >minval)%>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  freq2  <-  data.frame(table(Ldf$ID_ENTITY)) #exclude entities with <3 samples
  colnames(freq2)[1] <- 'ID_ENTITY'
  Ldf_error <- merge(Ldf_error, freq2, by = 'ID_ENTITY')
  Ldf_error$Freq <- as.numeric(Ldf_error$Freq)
  Ldf_error <- merge(Ldf_error, sam_base_freq, by = 'ID_ENTITY')
  Ldf_error <- Ldf_error %>% dplyr::filter(Freq.x >2 & Freq.y >2) 
  
  ##################################second filter for large mean z ##############################
  ###############################################entities########################################
  LGMdfmeanzt<- Ldf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')  
  Ldf <- merge(Ldf, LGMdfmeanzt, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf <- Ldf %>% filter(meanzt < 10)
  spdf <- merge(LGMdfmeanzt, spdf, by = c('entity_name'))
  spdf$fill<- spdf$meanzt <0
  
  LGMdfmeanzt_error<- Ldf_error %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt_error) <-c('entity_name', 'meanzt')  
  Ldf_error <- merge(Ldf_error, LGMdfmeanzt_error, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf_error <- Ldf_error %>% filter(meanzt < 10)
  spdf_error <- merge(LGMdfmeanzt_error, spdf2, by = c('entity_name'))
  spdf_error$fill<- spdf_error$meanzt <0
  #work out bounding box coordinates of polygon
  boun <- data.frame(bbox(Global))
  
  pcoords<- as.data.frame(Global@polygons[[1]]@Polygons[[1]]@coords)
  colnames(pcoords) <- c('lon','lat')
  #plot
  
  time_series_plot <-df_comp %>% filter(ID_ENTITY %in% unique(Ldf$ID_ENTITY)) 
  time_series_plot <- merge(time_series_plot, spdf, by = c('ID_ENTITY')) #add mean zt
  time_series_plot$fill<- time_series_plot$meanzt <0
  
  tseries <- ggplot(time_series_plot, aes(x= median_age/1000, y=zt, group = ID_ENTITY, color = fill)) +  geom_rect(aes(xmin = minval/1000, xmax = maxval/1000, ymin = -Inf, ymax = Inf),color = NA, fill = "grey79",  alpha = 0.2) +
    geom_rect(aes(xmin = basemin/1000, xmax = basemax/1000, ymin = -Inf, ymax = Inf), color = NA, fill = "grey79", alpha = 0.2) +
    geom_line(size= 0.25) + scale_color_manual(values = c('TRUE'= 'blue', 'FALSE' = 'red')) +scale_x_continuous(limits= c(0,24)) +  ggpubr:: theme_pubr() +ggpubr::labs_pubr(base_size = 6) +
    facet_wrap(as.factor(ID_ENTITY) ~ ., scales = "free_y") 
  
  
  e <- 'n entities'
  l <- length(spdf$ID_ENTITY)
  enttext<-paste(c(e, sep = "=",l))
  enttext <- paste(enttext, collapse = " ")
  e <- 'n entities'
  l <- length(spdf_error$ID_ENTITY)
  enttext_error<-paste(c(e, sep = "=",l))
  enttext_error <- paste(enttext_error, collapse = " ")
  
  
  Median_map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
    geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 3, size = 2) +
    geom_text(aes(x= -150, y=-55, label = enttext)) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                      fill = NA)) + ggtitle('Median_age')
  
  Median_map_error <- ggplot(data=spdf_error, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf_error, aes(x = lon, y = lat, color = fill), size =2) +
    geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 3, size = 2) +
    geom_text(aes(x= -150, y=-55, label = enttext_error)) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                      fill = NA)) + ggtitle('Median_accounting_for_age_error')
  
  
  multmap <-  ggarrange(Median_map, Median_map_error,
                        ncol = 1, nrow = 2,
                        heights = c(1))
  
  totmap <-ggarrange(multmap,tseries,
                     ncol = 2, nrow = 1,
                     widths = c(0.5,1))
  totmap <- annotate_figure(totmap, top = text_grob(title, 
                                                    color = "black", face = "bold", size = 16))
  
  killDbConnections()
  return(totmap)
  
} #filters entities for > 3 samples in LGM
Plotting_function_map_3_samples_df <- function(df,dupdf,minval,maxval, basemin,basemax, title) {
  
  f <- dupdf %>% filter(inclusion ==0)
  df_comp <- df
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_SITE, e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;")                               #Get RPD entity data
  sam <- dbGetQuery(mydb,("select e.ID_ENTITY, am.median from age_model am
                          left join sample s on s.ID_SAMPLE = am.ID_SAMPLE
                          left join entity e on e.ID_ENTITY = s.ID_ENTITY;"))  ##Get age model data to subset by minimum 3 samples for the baseline
  
  sam_base <- sam %>% dplyr::filter(median >basemin & median <basemax) #calculate number of samples in the baseline
  sam_base_freq <- data.frame(table(sam_base$ID_ENTITY))
  colnames(sam_base_freq)[1] <- 'ID_ENTITY'
  
  
  xy <- ent[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:5],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[Global,])
  spdf <- spdf[1:5]
  spdf2 <- spdf 
  
  ##################################First filter for duplicates##################################
  ###############################################################################################
  Ldf <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf <- Ldf %>% dplyr::filter(median_age <maxval & median_age  >minval ) %>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  freq  <-  data.frame(table(Ldf$ID_ENTITY)) #exclude entities with <3 samples
  colnames(freq)[1] <- 'ID_ENTITY'
  Ldf <- merge(Ldf, freq, by = 'ID_ENTITY')
  Ldf$Freq <- as.numeric(Ldf$Freq)
  Ldf <- merge(Ldf, sam_base_freq, by = 'ID_ENTITY')
  Ldf <- Ldf %>% dplyr::filter(Freq.x >2 & Freq.y >2) 
  
  Ldf_error <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf_error <- Ldf_error %>% dplyr::filter(median_age + UNCERT_75 <maxval & median_age - UNCERT_25 >minval)%>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  freq2  <-  data.frame(table(Ldf$ID_ENTITY)) #exclude entities with <3 samples
  colnames(freq2)[1] <- 'ID_ENTITY'
  Ldf_error <- merge(Ldf_error, freq2, by = 'ID_ENTITY')
  Ldf_error$Freq <- as.numeric(Ldf_error$Freq)
  Ldf_error <- merge(Ldf_error, sam_base_freq, by = 'ID_ENTITY')
  Ldf_error <- Ldf_error %>% dplyr::filter(Freq.x >2 & Freq.y >2) 
  
  ##################################second filter for large mean z ##############################
  ###############################################entities########################################
  LGMdfmeanzt<- Ldf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')  
  Ldf <- merge(Ldf, LGMdfmeanzt, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf <- Ldf %>% filter(meanzt < 10)
  spdf <- merge(LGMdfmeanzt, spdf, by = c('entity_name'))
  spdf$fill<- spdf$meanzt <0
  
  LGMdfmeanzt_error<- Ldf_error %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt_error) <-c('entity_name', 'meanzt')  
  Ldf_error <- merge(Ldf_error, LGMdfmeanzt_error, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf_error <- Ldf_error %>% filter(meanzt < 10)
  spdf_error <- merge(LGMdfmeanzt_error, spdf2, by = c('entity_name'))
  spdf_error$fill<- spdf_error$meanzt <0
  #work out bounding box coordinates of polygon
  boun <- data.frame(bbox(Global))
  
  pcoords<- as.data.frame(Global@polygons[[1]]@Polygons[[1]]@coords)
  colnames(pcoords) <- c('lon','lat')
  #plot
  
  time_series_plot <-df_comp %>% filter(ID_ENTITY %in% unique(Ldf$ID_ENTITY))    #change to Ldf error to filter for age uncertainty
  time_series_plot <- merge(time_series_plot, spdf, by = c('ID_ENTITY')) #add mean zt
  time_series_plot$fill<- time_series_plot$meanzt <0
  
  
  killDbConnections()
  return(time_series_plot)
  
} #outputs dataframe of sample entities (not accounting for age uncertainty as it appears to have limited effect on mean z of any entity) 

#functions to plot, output a df and save csv files for the 3 LGM iterations
Plot_Iteration_B <- Plotting_function_map_3_samples_min(LGMdf,Duplicates_Iteration_B,17480,23290,100,4000, 'Iteration B between H1 & H2')
Iteration_B_df <- Plotting_function_map_3_samples_df(LGMdf,Duplicates_Iteration_B,17480,23290,100,4000, 'Iteration B between H1 & H2')
write.csv(Iteration_B_df, '/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_B.csv')
Plot_Iteration_A <- Plotting_function_map_3_samples_min(LGMdf,Duplicates_Iteration_A,20500,21500,100,4000, 'Iteration A following Power et al. (2008)')
Iteration_A_df <- Plotting_function_map_3_samples_df(LGMdf,Duplicates_Iteration_A,20500,21500,100,4000, 'Iteration A following Power et al. (2008)')
write.csv(Iteration_A_df, '/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_A.csv')
Plot_Iteration_C <- Plotting_function_map_3_samples_min(LGMdf_itc,Duplicates_Iteration_C,17480,23290,100,1000, 'Iteration C finer baseline (0.1-1k)')
Iteration_C_df <- Plotting_function_map_3_samples_df(LGMdf_itc,Duplicates_Iteration_C,17480,23290,100,1000, 'Iteration C finer baseline (0.1-1k)')
write.csv(Iteration_C_df, '/Users/paullincoln/Dropbox/2022/Publications/LGM paper/Filtered entity zscores/Iteration_C.csv')

Plotting_function_region <- function(df,dupdf,minval,maxval, basemin,basemax, pol, title) {
  
  f <- dupdf %>% filter(inclusion ==0)
  df_comp <- df
  mydb = dbConnect(MySQL(), user='root', password='Vedde12171', dbname='RPDv2 6.2.22', host='localhost')
  
  ent <-  dbGetQuery(mydb, "select e.ID_SITE, e.ID_ENTITY, e.entity_name, e.latitude as 'lat', e.longitude as 'lon', e.TYPE, count(am.median), min(am.median) from entity e
                        left join sample s on s.ID_ENTITY = e.ID_ENTITY
                        left join age_model am on am.ID_SAMPLE = s.ID_SAMPLE
                        AND e.TYPE != 'other'
                        group by e.ID_ENTITY;") 
  
  xy <- ent[,c(5,4)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = ent[1:5],
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #filter which entities lie within the polygon
  spdf <- data.frame(spdf[pol,])
  spdf <- spdf[1:5]
  spdf2 <- spdf 
  
  ##################################First filter for duplicates##################################
  ###############################################################################################
  Ldf <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf <- Ldf %>% dplyr::filter(median_age <= maxval & median_age  >minval ) %>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  Ldf_error <-merge(spdf, df, by =c('ID_ENTITY'))
  Ldf_error <- Ldf_error %>% dplyr::filter(median_age + UNCERT_75 <maxval & median_age - UNCERT_25 >minval)%>%
    drop_na(zt) %>%
    subset(!(ID_ENTITY %in% f$ID_ENTITY))  #exclude duplicate entities
  ##################################second filter for large mean z ##############################
  ###############################################entities########################################
  LGMdfmeanzt<- Ldf %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt) <-c('entity_name', 'meanzt')  
  Ldf <- merge(Ldf, LGMdfmeanzt, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf <- Ldf %>% filter(meanzt < 10)
  spdf <- merge(LGMdfmeanzt, spdf, by = c('entity_name'))
  spdf$fill<- spdf$meanzt <0
  
  LGMdfmeanzt_error<- Ldf_error %>%
    group_by(entity_name) %>%
    summarise_at(vars(zt), list(name = mean))
  colnames(LGMdfmeanzt_error) <-c('entity_name', 'meanzt')  
  Ldf_error <- merge(Ldf_error, LGMdfmeanzt_error, by = 'entity_name')  #####second filter for mean z scores >10/-10 which are seen to dominate the regional values 
  Ldf_error <- Ldf_error %>% filter(meanzt < 10)
  spdf_error <- merge(LGMdfmeanzt_error, spdf2, by = c('entity_name'))
  spdf_error$fill<- spdf_error$meanzt <0
  #work out bounding box coordinates of polygon
  boun <- data.frame(bbox(pol))
  
  pcoords<- as.data.frame(pol@polygons[[1]]@Polygons[[1]]@coords)
  colnames(pcoords) <- c('lon','lat')
  #plot
  
  time_series_plot <-df_comp %>% filter(ID_ENTITY %in% unique(Ldf$ID_ENTITY)) 
  time_series_plot <- merge(time_series_plot, spdf, by = c('ID_ENTITY')) #add mean zt
  time_series_plot$fill<- time_series_plot$meanzt <0
  
  tseries <- ggplot(time_series_plot, aes(x= median_age/1000, y=zt, group = ID_ENTITY, color = fill)) +  geom_rect(aes(xmin = minval/1000, xmax = maxval/1000, ymin = -Inf, ymax = Inf),color = NA, fill = "grey79",  alpha = 0.2) +
    geom_rect(aes(xmin = basemin/1000, xmax = basemax/1000, ymin = -Inf, ymax = Inf), color = NA, fill = "grey79", alpha = 0.2) +
    geom_line(size= 0.25) + scale_color_manual(values = c('TRUE'= 'blue', 'FALSE' = 'red')) +scale_x_continuous(limits= c(0,24)) +  ggpubr:: theme_pubr() +ggpubr::labs_pubr(base_size = 6) +
    facet_wrap(as.factor(ID_ENTITY) ~ ., scales = "free_y") 
  
  e <- 'n entities'
  l <- length(spdf$ID_ENTITY)
  enttext<-paste(c(e, sep = "=",l))
  enttext <- paste(enttext, collapse = " ")
  e <- 'n entities'
  l <- length(spdf_error$ID_ENTITY)
  enttext_error<-paste(c(e, sep = "=",l))
  enttext_error <- paste(enttext_error, collapse = " ")
  
  
  Median_map <- ggplot(data=spdf, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf, aes(x = lon, y = lat, color = fill), size =2) +
    geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 1, size =3) +
    geom_text(aes(x= -150, y=-55, label = enttext)) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                      fill = NA)) + ggtitle('Median_age')
  
  Median_map_error <- ggplot(data=spdf_error, aes(x=lon, y=lat)) +
    geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'grey54', alpha = 0.5) +
    geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
    geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
    geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
    ggpubr:: theme_pubr()+  ggpubr::labs_pubr(base_size = 10)+ 
    geom_point(data = spdf_error, aes(x = lon, y = lat, color = fill), size =2) +
    geom_text(inherit.aes = T, aes(label =ID_ENTITY), nudge_y = 1, size =3) +
    geom_text(aes(x= -150, y=-55, label = enttext_error)) +
    scale_color_manual(values=c("FALSE"="red","TRUE"="blue"), labels = c('Positive', 'Negative'))+
    geom_hline(yintercept = 0, col= 'tan4') +
    geom_hline(yintercept = 30, col='tan1', linetype = 'dashed') +
    geom_hline(yintercept = -30, col='tan1', linetype = 'dashed') +
    scale_x_continuous(limits = c(boun[1,1]-0, boun[1,2]+0),breaks = seq(-180, 180, 30)) + 
    scale_y_continuous(limits = c(boun[2,1]-0, boun[2,2]+0), breaks = seq(-60, 90, 20))+
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
                                      fill = NA)) + ggtitle('Median_accounting_for_age_error')
  
  
  multmap <-  ggarrange(Median_map, Median_map_error,
                        ncol = 1, nrow = 2,
                        heights = c(1))
  
  totmap <-ggarrange(multmap,tseries,
                     ncol = 2, nrow = 1,
                     widths = c(0.5,1))
  totmap <- annotate_figure(totmap, top = text_grob(title, 
                                                    color = "black", face = "bold", size = 16))
  
  killDbConnections()
  return(totmap)
  
} ###Function to look at regional trends

Plot_Iteration_A_Aus <- Plotting_function_region(LGMdf,Duplicates_Iteration_A,20500,21500,100,4000, Aus, 'SE Australia, Iteration A following Power et al. (2008)')

Plot_Iteration_B_Aus <- Plotting_function_region(LGMdf,Duplicates_Iteration_B,17480,23290,100,4000, Aus, 'Iteration B between H1 & H2')






