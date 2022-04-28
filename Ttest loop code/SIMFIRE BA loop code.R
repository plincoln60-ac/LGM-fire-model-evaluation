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

############## Load up mean BA models & convert to dfs -----
SPITraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_BA_anomaly_baseline.nc")
SIMraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LPJ-GUESS-BLAZE_BA_anomaly_baseline.nc")
ORCraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_anomaly_baseline.nc")
LPJraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_anomaly_baseline.nc")
SPITraw <- SPITraw %>% hyper_tibble(force =T)
colnames(SPITraw) <- c('BA', 'lon', 'lat', 'time')
SIMraw <- SIMraw %>% hyper_tibble(force =T)
colnames(SIMraw) <- c('BA', 'lon', 'lat', 'time')
LPJraw <- LPJraw %>% hyper_tibble(force =T)
colnames(LPJraw) <- c('BA', 'lon', 'lat', 'time')
ORCraw <- ORCraw %>% hyper_tibble(force =T)
colnames(ORCraw) <- c('BA', 'lon', 'lat', 'time')

#create base dataframe with cell numbers to identify matching cells
SPIT <- SPITraw %>% filter(time == 25730) 
SPIT$cell <- as.numeric(seq(1:46984))
SPIT <- SPIT[c(2:3,5)]

#merge cell numbers to different models
SPITraw <- merge(x= SPITraw, y = SPIT, by =c('lon','lat'))
SPITraw <- SPITraw[c(1:3,5)]
colnames(SPITraw) <- c('lon', 'lat', 'BA', 'cell')
SPITraw$model <- 'SPITFIRE'
SIMraw <- merge(x= SIMraw, y = SPIT, by =c('lon','lat'))
SIMraw <- SIMraw[c(1:3,5)]
colnames(SIMraw) <- c('lon', 'lat', 'BA', 'cell')
SIMraw$model <- 'SIMFIRE'
ORCraw <- merge(x= ORCraw, y = SPIT, by =c('lon','lat'))
ORCraw <- ORCraw[c(1:3,5)]
colnames(ORCraw) <- c('lon', 'lat', 'BA', 'cell')
ORCraw$model <- 'ORCHIDEE'
LPJraw <- merge(x= LPJraw, y = SPIT, by =c('lon','lat'))
LPJraw <- LPJraw[c(1:3,5)]
colnames(LPJraw) <- c('lon', 'lat', 'BA', 'cell')
LPJraw$model <- 'LPJLM'
SPITraw<- SPITraw[order(SPITraw$cell),]
SIMraw<- SIMraw[order(SIMraw$cell),]
LPJraw<- LPJraw[order(LPJraw$cell),]
ORCraw<- ORCraw[order(LPJraw$cell),]


#find cells numbers consistent in both models
SPITcells <- unique(SPITraw$cell)
SIMcells <- unique(SIMraw$cell)
LPJLMcells <- unique(LPJraw$cell)
ORCcells <- unique(ORCraw$cell)
SPITSIMcells <- intersect(SPITcells,SIMcells)
SPITLMcells <- intersect(SPITcells, LPJLMcells)
SPITORCcells <- intersect(SPITcells, ORCcells)

rm(SPITcells,SIMcells,LPJLMcells, ORCcells)

#set up inputs for loop
SPIT_SIM_cell<- vector( length = nrow(SPIT))
SPIT_SIM_t <- vector( length = nrow(SPIT))
SPIT_SIM_p <- vector( length = nrow(SPIT))
SPIT_LPJLM_cell <- vector(length = nrow(SPIT))
SPIT_LPJLM_t <- vector(length = nrow(SPIT))
SPIT_LPJLM_p <- vector(length = nrow(SPIT))
SPIT_ORC_t <-vector(length = nrow(SPIT))
SPIT_ORC_p <- vector(length = nrow(SPIT))
SPIT_ORC_cell <- vector(length = nrow(SPIT))



ttest_function<- function(df2, df, cells,ct, t, p) {require(svMisc)
for(i in cells) {
  progress(i, 46984)
  r3 <- data.frame(df[i])
  colnames(r3) <- c('BA','cell','model')
  tt <- t.test(BA~model, r3, mu =0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)
  c <-unique(r3$cell)
  ct[i] <- c
  t[i] <- tt$statistic
  p[i] <- tt$p.value
  rm(tt)
}
p <- unlist(p)
t <- unlist(t)
ct <- unlist(ct)
SM <- data.frame(cbind(p,t,ct))
df2 <- merge(SPIT, SM, by = 'cell')
return(df2)
}



#run for loops to populate data frame
#SPITSIM loop

#create list from a dataframe to speed up loop
r <- rbind(SPITraw, SIMraw)
#raw <- raw %>% filter(cell %in% SPITSIMcells)
r <- split(r[c(3:5)],f = factor(r$cell))

require(svMisc)
for(i in SPITSIMcells) {
  
  progress(i, 46984)
  tryCatch({
  r3 <- data.frame(r[i])
  colnames(r3) <- c('BA','cell','model')
  tt <- t.test(BA~model, r3, mu =0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)
  c <-unique(r3$cell)
  SPIT_SIM_cell[i] <- c
  SPIT_SIM_t[i] <- tt$statistic
  SPIT_SIM_p[i] <- tt$p.value
  rm(tt)
  }, error=function(e){cat("ERROR :")})
}

SPIT_SIM_p <- unlist(SPIT_SIM_p)
SPIT_SIM_t <- unlist(SPIT_SIM_t)
cell <- unlist(SPIT_SIM_cell)
SPIT_SIM <- data.frame(cbind(SPIT_SIM_p,SPIT_SIM_t,cell))
SPIT_SIM <- SPIT_SIM %>% filter(cell != 0)
SPITFIRE <- merge(SPIT, SPIT_SIM, by = 'cell')

###LPJLM
#create list from a dataframe to speed up loop
r <- rbind(SPITraw, LPJraw)
#raw <- raw %>% filter(cell %in% SPITSIMcells)
r <- split(r[c(3:5)],f = factor(r$cell))

require(svMisc)
for(i in SPITLMcells) {
  
  progress(i, 46984)
  tryCatch({
    r3 <- data.frame(r[1])
    colnames(r3) <- c('BA','cell','model')
    tt <- t.test(BA~model, r3, mu =0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)
    c <-unique(r3$cell)
    SPIT_LPJLM_cell[i] <- c
    SPIT_LPJLM_t[1] <- tt$statistic
    SPIT_LPJLM_p[i] <- tt$p.value
    rm(tt)
  }, error=function(e){cat("ERROR :")})
}

SPIT_LPJLM_p <- unlist(SPIT_LPJLM_p)
SPIT_LPJLM_t <- unlist(SPIT_LPJLM_t)
cell <- unlist(SPIT_LPJLM_cell)
SPIT_LPJLM <- data.frame(cbind(SPIT_LPJLM_p,SPIT_LPJLM_t,cell))
SPIT_LPJLM <- SPIT_LPJLM %>% filter(cell != 0)

#####ORCHIDEE
r <- rbind(SPITraw, ORCraw)
#raw <- raw %>% filter(cell %in% SPITSIMcells)
r <- split(r[c(3:5)],f = factor(r$cell))

require(svMisc)
for(i in SPITLMcells) {
  
  progress(i, 46984)
  tryCatch({
    r3 <- data.frame(r[i])
    colnames(r3) <- c('BA','cell','model')
    tt <- t.test(BA~model, r3, mu =0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)
    c <-unique(r3$cell)
    SPIT_ORC_cell[i] <- c
    SPIT_ORC_t[i] <- tt$statistic
    SPIT_ORC_p[i] <- tt$p.value
    rm(tt)
  }, error=function(e){cat("ERROR :")})
}

SPIT_ORC_p <- unlist(SPIT_ORC_p)
SPIT_ORC_t <- unlist(SPIT_ORC_t)
cell <- unlist(SPIT_ORC_cell)
SPIT_ORC <- data.frame(cbind(SPIT_ORC_p,SPIT_ORC_t,cell))


SPITFIRE <- SPIT
SPITFIRE <- merge(SPITFIRE, SPIT_SIM, by = 'cell', all.x = T)
SPITFIRE <- merge(SPITFIRE, SPIT_ORC, by = 'cell', all.x = T)
SPITFIRE <- merge(SPITFIRE, SPIT_LPJLM, by = 'cell', all.x = T)




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
SPITFIRE <- SPITFIRE %>% dplyr::mutate(LPJLMsignificance = dplyr::case_when(
  SPIT_LPJLM_p < 0.05 ~'p <0.05',
  SPIT_LPJLM_p >= 0.05 ~'p >0.05',
  SPIT_LPJLM_p == 'NaN'~'NA'))
SPITFIRE <- SPITFIRE %>% dplyr::mutate(SIMFIREsignificance = dplyr::case_when(
  SPIT_SIM_p < 0.05 ~'p <0.05',
  SPIT_SIM_p >= 0.05 ~'p >0.05',
  SPIT_SIM_p == 'NaN'~'NA'))
SPITFIRE <- SPITFIRE %>% dplyr::mutate(ORCHIDEEsignificance = dplyr::case_when(
  SPIT_ORC_p < 0.05 ~'p <0.05',
  SPIT_ORC_p >= 0.05 ~'p >0.05',
  SPIT_ORC_p == 'NaN'~'NA'))


SPITFIRE_plot <- melt(SPITFIRE, id.vars = c('lat','lon','cell'), measure.vars = c('LPJLMsignificance','SIMFIREsignificance','ORCHIDEEsignificance'))


fill <- c('p >0.05' = "green", 'p <0.05' = 'red')



ggplot(data=SPITFIRE_plot, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = value)) + scale_fill_manual(values = fill)  +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) + facet_grid(~variable) + ggtitle('SPITFIRE')



ggplot(data=SPIT, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = SIMFIREsignificance)) + scale_fill_manual(values = fill)  +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('SPITFIRE-SIMFIRE BA anomaly')


ggplot(data=SPIT, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = LPJLMsignificance)) + scale_fill_manual(values = fill)  +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  geom_tile(data = dfp2, aes(x=lon,y=lat, fill=ice), fill = 'slategray1')+
  geom_tile(data=dfp2, alpha = 0.0, color = "black", size = 0.5, linejoin = "round") +
  geom_tile(data=dfp2, alpha = 1, aes(fill = ice),fill = 'slategray1' ) +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) +
  ggtitle('SPITFIRE-LPJLM BA anomaly')


