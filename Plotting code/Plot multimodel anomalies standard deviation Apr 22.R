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

############## Load up mean BA models & convert to dfs -----

SPITBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/Apr22 for agreement plots/SPITFIRE_1951_1970_BA_diff.nc")
SIMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/Apr22 for agreement plots/SIMFIRE_1951_1970_BA_diff.nc")
ORCBA <-tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/Apr22 for agreement plots/ORCHIDEE_1951_1970_BA_diff.nc")
LMBA <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/1951_1970_reference/Apr22 for agreement plots/LPJLM_1951_1970_BA_diff.nc")


SPITBA <- SPITBA %>% hyper_tibble(force = T)
colnames(SPITBA) <- c("SPITBA", "SPITBA_sd", "lon", "lat", "time")
SPITBA <- SPITBA[,1:4]
SPITBA$SPITmin <- SPITBA$SPITBA - SPITBA$SPITBA_sd
SPITBA$SPITmax <- SPITBA$SPITBA + SPITBA$SPITBA_sd
SIMBA <- SIMBA %>% hyper_tibble(force = T)
colnames(SIMBA) <- c("SIMBA", "SIMBA_sd", "lon", "lat", "time")
SIMBA <- SIMBA[,1:4]
SIMBA$SIMmin <- SIMBA$SIMBA - SIMBA$SIMBA_sd
SIMBA$SIMmax <- SIMBA$SIMBA + SIMBA$SIMBA_sd
ORCBA <- ORCBA %>% hyper_tibble(force = T)
colnames(ORCBA) <- c("ORCBA", "ORCBA_sd", "lon", "lat", "time")
ORCBA <- ORCBA[,1:4]
ORCBA$ORCmin <- ORCBA$ORCBA - ORCBA$ORCBA_sd
ORCBA$ORCmax <- ORCBA$ORCBA + ORCBA$ORCBA_sd
LMBA <- LMBA %>% hyper_tibble(force = T)
colnames(LMBA) <- c("LMBA", "LMBA_sd", "lon", "lat", "time")
LMBA <- LMBA[,1:4]
LMBA$LMmin <- LMBA$LMBA - LMBA$LMBA_sd
LMBA$LMmax <- LMBA$LMBA + LMBA$LMBA_sd

stat_function <- function(df){
  colnames(df) <- 'value'
  m<-mean(df$value)
  s <- sd(df$value)
  l <- as.list(c(m,s))
  return(l)
}
quantitative_stat_function<-function(df, model) {
  region <- c('Globe', 'Tropics', 'Northern_Extratropics', 'Southern_Extratropics')
  mean <- c('NA','NA','NA','NA')
  sd <- c('NA','NA','NA','NA')
  model <- (model)
  df3 <- data.frame(region, mean, sd, model)
  globe <- stat_function(df[1])
  tropic <- df %>% filter(lat > -30 & lat < 30)
  t_stat<- stat_function(tropic[1])
  N_extrop <-  df %>% filter(lat > 30)
  n_ex_stat <- stat_function(N_extrop[1])
  S_extrop <- df %>% filter(lat < -30)
  s_ex_stat <- stat_function(S_extrop[1])
  df3$mean[1] <-globe[1] 
  df3$sd[1] <- globe[2]
  df3$mean[2] <- t_stat[1]
  df3$sd[2] <- t_stat[2]
  df3$mean[3] <- n_ex_stat[1]
  df3$sd[3] <- n_ex_stat[2]
  df3$mean[4] <- s_ex_stat[1]
  df3$sd[4] <- s_ex_stat[2]
  return(df3)
}

LPJLM_summary <-quantitative_stat_function(LMBA, 'LPJLM') 
ORCHIDEE_summary <-quantitative_stat_function(ORCBA, 'ORCHIDEE')
SPITFIRE_summary <-quantitative_stat_function(SPITBA, 'SPITFIRE')
SIMFIRE_summary <-quantitative_stat_function(SIMBA, 'SIMFIRE')

Mod_summary <-rbind(SPITFIRE_summary,SIMFIRE_summary,ORCHIDEE_summary,LPJLM_summary)
write.csv(Mod_summary, "/Users/paullincoln/Dropbox/2022/Research/Model_BA_summary.csv")



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
all_BA <- SPITBA[,1:6]
all_BA <- merge(x = all_BA, y = SIMBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = ORCBA, by = c("lon","lat"))
all_BA <- merge(x = all_BA, y = LMBA, by = c("lon","lat"))

############### set ttest functions for for loop ############### ----
tt <-function(df_m,df_s,df2_m,df2_s){
  BSDA::tsum.test(mean.x = df_m, s.x = df_s, mean.y = df2_m, s.y = df2_s, alternative = 'two.sided', n.x = 90, n.y = 90, mu = 0, var.equal = F, conf.level = 0.95)
}

#create input data for for loop
row <- c(1:41849)

SPITFIRE_ttest <-all_BA[1:2]
SIMFIRE_ttest <-all_BA[1:2]
ORCHIDEE_ttest <-all_BA[1:2]
LPJLM_ttest <-all_BA[1:2]

  SPITFIRE_t <- numeric(nrow(all_BA))
  SPITFIRE_p <- SPITFIRE_t
  SIMFIRE_t <- SPITFIRE_t
  SIMFIRE_p <- SPITFIRE_t
  LPJLM_t <- SPITFIRE_t
  LPJLM_p <- SPITFIRE_t
  ORCHIDEE_t <- SPITFIRE_t
  ORCHIDEE_p <- SPITFIRE_t

  #run for loop for SPITFITE
  require(svMisc)
  for(i in row) {
    progress(i, 41849)
    v <- tt(all_BA$SPITBA[i], all_BA$SPITBA_sd[i], all_BA$SIMBA[i], all_BA$SIMBA_sd[i])
    w<- tt(all_BA$SPITBA[i], all_BA$SPITBA_sd[i], all_BA$ORCBA[i], all_BA$ORCBA_sd[i])
    x<- tt(all_BA$SPITBA[i], all_BA$SPITBA_sd[i], all_BA$LMBA[i], all_BA$LMBA_sd[i])
    SIMFIRE_t[i] <- v$statistic
    SIMFIRE_p[i] <- v$p.value
    ORCHIDEE_t[i] <- w$statistic
    ORCHIDEE_p[i] <- w$p.value
    LPJLM_t[i] <- x$statistic
    LPJLM_p[i] <- x$p.value
  }
  
  SPITFIRE_ttest$SIMFIRE_t <-  as.numeric(SIMFIRE_t)
  SPITFIRE_ttest$SIMFIRE_p <-  as.numeric(SIMFIRE_p)
  SPITFIRE_ttest$ORCHIDEE_t <- as.numeric(ORCHIDEE_t)
  SPITFIRE_ttest$ORCHIDEE_p <- as.numeric(ORCHIDEE_p)
  SPITFIRE_ttest$LPJLM_t <- as.numeric(LPJLM_t)
  SPITFIRE_ttest$LPJLM_p <- as.numeric(LPJLM_p)
  rm(SPITFIRE_t,SPITFIRE_p,SIMFIRE_t,SIMFIRE_p,ORCHIDEE_t,ORCHIDEE_p,LPJLM_t,LPJLM_p)
  #run for loop for SIMFIRE
  SPITFIRE_t <- numeric(nrow(all_BA))
  SPITFIRE_p <- SPITFIRE_t
  SIMFIRE_t <- SPITFIRE_t
  SIMFIRE_p <- SPITFIRE_t
  LPJLM_t <- SPITFIRE_t
  LPJLM_p <- SPITFIRE_t
  ORCHIDEE_t <- SPITFIRE_t
  ORCHIDEE_p <- SPITFIRE_t
  
  require(svMisc)
  for(i in row) {
    progress(i, 41849)
    v <- tt(all_BA$SIMBA[i], all_BA$SIMBA_sd[i], all_BA$SPITBA[i], all_BA$SPITBA_sd[i])
    w<- tt(all_BA$SIMBA[i], all_BA$SIMBA_sd[i], all_BA$ORCBA[i], all_BA$ORCBA_sd[i])
    x<- tt(all_BA$SIMBA[i], all_BA$SIMBA_sd[i], all_BA$LMBA[i], all_BA$LMBA_sd[i])
    SPITFIRE_t[i] <- v$statistic
    SPITFIRE_p[i] <- v$p.value
    ORCHIDEE_t[i] <- w$statistic
    ORCHIDEE_p[i] <- w$p.value
    LPJLM_t[i] <- x$statistic
    LPJLM_p[i] <- x$p.value
  }
  
  SIMFIRE_ttest$SPITFIRE_t <-  as.numeric(SPITFIRE_t)
  SIMFIRE_ttest$SPITFIRE_p <-  as.numeric(SPITFIRE_p)
  SIMFIRE_ttest$ORCHIDEE_t <- as.numeric(ORCHIDEE_t)
  SIMFIRE_ttest$ORCHIDEE_p <- as.numeric(ORCHIDEE_p)
  SIMFIRE_ttest$LPJLM_t <- as.numeric(LPJLM_t)
  SIMFIRE_ttest$LPJLM_p <- as.numeric(LPJLM_p)
  rm(SPITFIRE_t,SPITFIRE_p,SIMFIRE_t,SIMFIRE_p,ORCHIDEE_t,ORCHIDEE_p,LPJLM_t,LPJLM_p)
  #run for loop for ORCHIDEE
  SPITFIRE_t <- numeric(nrow(all_BA))
  SPITFIRE_p <- SPITFIRE_t
  SIMFIRE_t <- SPITFIRE_t
  SIMFIRE_p <- SPITFIRE_t
  LPJLM_t <- SPITFIRE_t
  LPJLM_p <- SPITFIRE_t
  ORCHIDEE_t <- SPITFIRE_t
  ORCHIDEE_p <- SPITFIRE_t
  
require(svMisc)
for(i in row) {
  progress(i, 41849)
  v <- tt(all_BA$ORCBA[i], all_BA$ORCBA_sd[i], all_BA$SPITBA[i], all_BA$SPITBA_sd[i])
  w<- tt(all_BA$ORCBA[i], all_BA$ORCBA_sd[i], all_BA$SIMBA[i], all_BA$SIMBA_sd[i])
  x<- tt(all_BA$ORCBA[i], all_BA$ORCBA_sd[i], all_BA$LMBA[i], all_BA$LMBA_sd[i])
  SPITFIRE_t[i] <- v$statistic
  SPITFIRE_p[i] <- v$p.value
  SIMFIRE_t[i] <- w$statistic
  SIMFIRE_p[i] <- w$p.value
  LPJLM_t[i] <- x$statistic
  LPJLM_p[i] <- x$p.value
}
  ORCHIDEE_ttest$SPITFIRE_t <- as.numeric(SPITFIRE_t)
  ORCHIDEE_ttest$SPITFIRE_p <- as.numeric(SPITFIRE_p)
  ORCHIDEE_ttest$SIMFIRE_t <-  as.numeric(SIMFIRE_t)
  ORCHIDEE_ttest$SIMFIRE_p <-  as.numeric(SIMFIRE_p)
  ORCHIDEE_ttest$LPJLM_t <- as.numeric(LPJLM_t)
  ORCHIDEE_ttest$LPJLM_p <- as.numeric(LPJLM_p)
  rm(SPITFIRE_t,SPITFIRE_p,SIMFIRE_t,SIMFIRE_p,ORCHIDEE_t,ORCHIDEE_p,LPJLM_t,LPJLM_p)

  #run for loop for LPJLM
  SPITFIRE_t <- numeric(nrow(all_BA))
  SPITFIRE_p <- SPITFIRE_t
  SIMFIRE_t <- SPITFIRE_t
  SIMFIRE_p <- SPITFIRE_t
  LPJLM_t <- SPITFIRE_t
  LPJLM_p <- SPITFIRE_t
  ORCHIDEE_t <- SPITFIRE_t
  ORCHIDEE_p <- SPITFIRE_t
  
  require(svMisc)
  for(i in row) {
    progress(i, 41849)
    v <- tt(all_BA$LMBA[i], all_BA$LMBA_sd[i], all_BA$SPITBA[i], all_BA$SPITBA_sd[i])
    w<- tt(all_BA$LMBA[i], all_BA$LMBA_sd[i], all_BA$SIMBA[i], all_BA$SIMBA_sd[i])
    x<- tt(all_BA$LMBA[i], all_BA$LMBA_sd[i], all_BA$ORCBA[i], all_BA$ORCBA_sd[i])
    SPITFIRE_t[i] <- v$statistic
    SPITFIRE_p[i] <- v$p.value
    SIMFIRE_t[i] <- w$statistic
    SIMFIRE_p[i] <- w$p.value
    ORCHIDEE_t[i] <- x$statistic
    ORCHIDEE_p[i] <- x$p.value
  }
  LPJLM_ttest$SPITFIRE_t <- as.numeric(SPITFIRE_t)
  LPJLM_ttest$SPITFIRE_p <- as.numeric(SPITFIRE_p)
  LPJLM_ttest$SIMFIRE_t <-  as.numeric(SIMFIRE_t)
  LPJLM_ttest$SIMFIRE_p <-  as.numeric(SIMFIRE_p)
  LPJLM_ttest$ORCHIDEE_t <- as.numeric(ORCHIDEE_t)
  LPJLM_ttest$ORCHIDEE_p <- as.numeric(ORCHIDEE_p)
  rm(SPITFIRE_t,SPITFIRE_p,SIMFIRE_t,SIMFIRE_p,ORCHIDEE_t,ORCHIDEE_p,LPJLM_t,LPJLM_p)
  
  
  SPITFIRE_ttest <- SPITFIRE_ttest[c(1:2,4,6,8)]
  SPITFIRE_ttest <- SPITFIRE_ttest %>% melt(id = c('lon', 'lat'))
  SPITFIRE_ttest <- SPITFIRE_ttest %>% dplyr::mutate(significance = dplyr::case_when(
    value < 0.05 ~'p <0.05',
    value >= 0.05 ~'p >0.05'
  ))
  SIMFIRE_ttest <- SIMFIRE_ttest[c(1:2,4,6,8)]
  SIMFIRE_ttest <- SIMFIRE_ttest %>% melt(id = c('lon', 'lat'))
  SIMFIRE_ttest <- SIMFIRE_ttest %>% dplyr::mutate(significance = dplyr::case_when(
    value < 0.05 ~'p <0.05',
    value >= 0.05 ~'p >0.05'
  ))
  ORCHIDEE_ttest <- ORCHIDEE_ttest[c(1:2,4,6,8)]
  ORCHIDEE_ttest <- ORCHIDEE_ttest %>% melt(id = c('lon', 'lat'))
  ORCHIDEE_ttest <- ORCHIDEE_ttest %>% dplyr::mutate(significance = dplyr::case_when(
    value < 0.05 ~'p <0.05',
    value >= 0.05 ~'p >0.05'
  ))
  LPJLM_ttest <- LPJLM_ttest[c(1:2,4,6,8)]
  LPJLM_ttest <- LPJLM_ttest %>% melt(id = c('lon', 'lat'))
  LPJLM_ttest <- LPJLM_ttest %>% dplyr::mutate(significance = dplyr::case_when(
    value < 0.05 ~'p <0.05',
    value >= 0.05 ~'p >0.05'
  ))
  
  
  ############### plot ############### ----  
#colour scale
fill <- c('p >0.05' = "green", 'p <0.05' = 'red')
  
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

SPIT <- ggplot(data=SPITFIRE_ttest, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = significance)) + scale_fill_manual(values = fill)  +
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
        axis.ticks.y=element_blank()) + facet_grid(~variable) + 
  ggtitle('SPITFIRE')

SIM <- ggplot(data=SIMFIRE_ttest, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = significance)) + scale_fill_manual(values = fill)  +
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
        axis.ticks.y=element_blank()) + facet_grid(~variable) + 
  ggtitle('SIMFIRE')


ORC <- ggplot(data=ORCHIDEE_ttest, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = significance)) + scale_fill_manual(values = fill)  +
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
        axis.ticks.y=element_blank()) + facet_grid(~variable) + 
  ggtitle('ORCHIDEE')

LPJLM <- ggplot(data=LPJLM_ttest, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = significance)) + scale_fill_manual(values = fill)  +
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
        axis.ticks.y=element_blank()) + facet_grid(~variable) + 
  ggtitle('LPJLM')

ggpubr::ggarrange(SPIT,SIM, ORC, LPJLM,
          labels = c("A", "B", "C", "D"),
          ncol = 1, nrow = 4, common.legend = T)



