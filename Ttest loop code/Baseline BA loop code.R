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
SPITraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SPITFIRE/LPJ-GUESS-SPITFIRE_LGM_Reference_BA_1951_1970_raw.nc")
SIMraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/SIMFIRE/LPJ-GUESS-BLAZE_v2_BA_1951_1970raw_remap.nc")
ORCraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/ORCHIDEE/ORCHIDEE_BA_1951_1970_raw.nc")
LPJraw <- tidync("~/Dropbox/2021/Research/RPD LGM for model comparison/November_21_new_references/LPJ LM/LPJLM_BA_1951_1970_raw.nc")
SPITraw <- SPITraw %>% hyper_tibble(force =T)
colnames(SPITraw) <- c('BA', 'lon', 'lat', 'time')
SIMraw <- SIMraw %>% hyper_tibble(force =T)
colnames(SIMraw) <- c('BA', 'lon', 'lat', 'time')
LPJraw <- LPJraw %>% hyper_tibble(force =T)
colnames(LPJraw) <- c('BA', 'lon', 'lat', 'time')
ORCraw <- ORCraw %>% hyper_tibble(force =T)
colnames(ORCraw) <- c('BA', 'lon', 'lat', 'time')

#create base dataframes with cell numbers to identify matching cells
SPIT <- SPITraw %>% filter(time == SPITraw$time[1]) 
SPIT$cell <- as.numeric(seq(1:nrow(SPIT)))
SPIT <- SPIT[c(2:3,5)]
SIM <- SIMraw %>% filter(time == SIMraw$time[1]) 
ORC<- ORCraw %>% filter(time == ORCraw$time[1]) 
LPJLM <- LPJraw %>% filter(time == LPJraw$time[1]) 
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
SIM <- merge(x=SIM[2:3], y=SPIT, by = c('lon','lat'))
LPJLM <- merge(x=LPJLM[2:3], y=SPIT, by = c('lon','lat'))
ORC <- merge(x=ORC, y=SPIT, by = c('lon','lat'))
#find cells numbers consistent in both models
SPITcells <- unique(SPITraw$cell)
SIMcells <- unique(SIMraw$cell)
LPJLMcells <- unique(LPJraw$cell)
ORCcells <- unique(ORCraw$cell)
SPITSIMcells <- sort(intersect(SPITcells,SIMcells))
SPITLMcells <- sort(intersect(SPITcells, LPJLMcells))

SPITORCcells <- sort(intersect(SPITcells, ORCcells))
SIMSPITcells <- sort(intersect(SPITcells,SIMcells))
SIMLMcells <- sort(intersect(SIMcells, LPJLMcells))
SIMORCcells <- sort(intersect(SIMcells, ORCcells))
ORCSPITcells<- sort(intersect(ORCcells, SPITcells))
ORCSIMcells<- sort(intersect(ORCcells, SIMcells))
ORCLMcells<- sort(intersect(ORCcells, LPJLMcells))
LMSPITcells <- sort(intersect(LPJLMcells, SPITcells))
LMSIMcells <- sort(intersect(LPJLMcells, SIMcells))
LMORCcells <- sort(intersect(LPJLMcells, ORCcells))

rm(SPITcells,SIMcells,LPJLMcells, ORCcells)

#df and df 2 are the raw value dataframes, df3 is the output df, cellnum is the unique cells

ttest_function<- function(df,df2, df3, cellnum) {
  #create list from a dataframe to speed up loop
  r <- rbind(df,df2)
  r<- r %>% filter(r$cell %in% cellnum) 
  r <- split(r[c(3:5)],f = factor(r$cell))
  #set up list to write ttest values to
  cell<- vector( length = nrow(df3))
  tt_2 <- list( cell = cell,
                t= cell, 
                p=cell, 
                df=cell,  
                mod_mean1=cell, 
                mod_mean2=cell,
                null.value= cell, 
                stderr=cell, 
                alternative=cell, 
                method=cell)
  require(svMisc)
  for(i in 1:length(cellnum)) {
    progress(i, nrow(df3))
    tryCatch({
      r3 <- data.frame(r[i])
      colnames(r3) <- c('BA','cell','model')
      tt <- t.test(BA~model, r3, mu =0, alt = 'two.sided', conf = 0.95, var.eq = F, paired = F)
      c <-unique(r3$cell)
      tt_2$cell[i] <- c
      tt_2$t[i] <- tt$statistic
      tt_2$df[i]<-tt$parameter
      tt_2$p[i] <- tt$p.value
      tt_2$mod_mean1[i]<-tt$estimate[1]
      tt_2$mod_mean2[i]<-tt$estimate[2]
      tt_2$null.value[i]<-tt$null.value
      tt_2$stderr[i]<-tt$stderr
      tt_2$alternative[i]<- tt$alternative
      tt_2$method[i] <- tt$method
      rm(tt)
    }, error=function(e){cat("ERROR :")})
  }
  modout <- data.frame(tt_2)
  modout <- modout %>% filter(cell != 0)
  modout <- merge(modout, df3, by = 'cell')
  rm(tt_2)
  return(modout)
}

SPIT_LPJLM <- ttest_function(SPITraw,LPJraw,SPIT,SPITLMcells)
SPIT_LPJLM$model <- 'SPIT_LPJLM'
SPIT_ORC <- ttest_function(SPITraw,ORCraw,SPIT,SPITORCcells)
SPIT_ORC$model <- 'SPIT_ORC'
SPIT_SIM <- ttest_function(SPITraw,SIMraw,SPIT,SPITSIMcells)
SPIT_SIM$model <- 'SPIT_SIM'
SIM_ORC<-ttest_function(SIMraw,ORCraw,SIM,SIMORCcells)
SIM_ORC$model <- 'SIM_ORC'
SIM_LPJLM<-ttest_function(SIMraw,LPJraw,SIM,SIMLMcells)
SIM_LPJLM$model <- 'SIM_LPJLM'
ORC_LPJLM<-ttest_function(ORCraw,LPJraw,ORC,ORCLMcells)
ORC_LPJLM$model <- 'ORC_LPJLM'

df <-rbind(SPIT_LPJLM,SPIT_ORC,ORC_LPJLM[c(1:12,15)])
df$meanBA_diff <- df$mod_mean1 - df$mod_mean2 
df <- df%>% dplyr::mutate(significance = dplyr::case_when(
  meanBA_diff < 1 & meanBA_diff > -1 ~'difference <1%',
  p < 0.05 ~'p <0.05',
  p >= 0.05 ~'p >0.05'))


#LGM ice raster file
p <- raster('/Volumes/PL SSD/Shapefiles/LGM mask/LGM mask2.tif')
p[p==1] <- 'ice'

dfp <- as.data.frame(p, xy= T)
dfp2 <- dfp %>% filter(LGM_mask2 == 'ice')
colnames(dfp2) <- c('lon','lat','ice')
#####get SPITple global coastline shapefile####
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",  destfile = 'coastlines.zip')
unzip(zipfile = "coastlines.zip", 
      exdir = 'ne-coastlines-10m')
coastlines <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
coastlines <- SpatialLinesDataFrame(coastlines,
                                    coastlines@data)



fill <- c('p >0.05' = "green", 'p <0.05' = 'red', 'difference <1%' = 'lightgrey')

ggplot(data=df, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = significance))   + scale_fill_manual(values = fill) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) + facet_wrap(~model,nrow=3) + ggtitle('SF1 Baseline t-test comparison')





zeroCol <-"#FFFFFF" # (gray color, same as your figure example)
reds <- RColorBrewer:: brewer.pal('YlOrRd', n = 9)
blues <- rev(RColorBrewer::brewer.pal('Blues', n = 9))
revblue <-RColorBrewer:: brewer.pal('Blues', n = 9)
cutpts<- seq(-80,80, by=  10)

BAanom <- ggplot(data=df, aes(x=lon, y=lat)) +
  geom_tile(alpha = 0.9,aes(fill = meanBA_diff))    + scale_fill_binned(low = blues ,high = reds, breaks = c(cutpts), limits= c(-80,80)) +
  geom_path(data = coastlines,  aes(x=long, y=lat, group = group), size = 0.25, color = 'black') +
  ggpubr:: theme_pubr()+   ggpubr::labs_pubr(base_size = 12)+ 
  scale_x_continuous(limits = c(-180, 180),breaks = seq(-180, 180, 30)) + 
  scale_y_continuous(limits = c(-60, 84), breaks = seq(-60, 90, 20))+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()) + facet_wrap(~model,nrow=3) + ggtitle('Model mean BA difference (%)')
BAanom
ggpubr::ggarrange(BAanom, legend = 'right')

