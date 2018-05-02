
setwd("C:/Users/fsmoura/Desktop/docs/")

files <- list.files('geo/')
filenames <- list.files(path = "C:/Users/fsmoura/Desktop/docs/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 

library(ggmap)
mh_map_set_dois = get_googlemap(center = c(-51.200421939611394, -30.06753685571282), zoom = 12, source="osm")
mapPoints <- ggmap(mh_map_set_dois)
mapPoints
names(data)
dados <- cbind(data$long, data$lat, data$default, data$tipo)
dados <- as.data.frame(dados)
dados$V1 <- as.numeric(as.character(dados$V1))
dados$V2 <- as.numeric(as.character(dados$V2))
dim(dados)
dados$newrow <- sample(1000, size = nrow(dados), replace = TRUE)

mapPoints <- ggmap(mh_map_set_dois) + 
  geom_point(data = dados, aes(x = V1, y = V2), 
             fill = "red", alpha =0.2, size = 0.5, shape = 21,stroke = 0) + 
  ggtitle("EPTC Pontos de Atratividade")
mapPoints

#########################
library(ggplot2)
ggmap(mh_map_set_dois, extent='device') +geom_point(aes(x=V1, y=V2), colour="red", alpha=0.1, size=1, data=dados)

library(ggplot2)
ggmap(mh_map_set_dois, extent='device') +geom_point(aes(x=V1, y=V2, colour = V4), alpha=0.1, size=1, data=dados)


###############################
ggmap(mh_map_set_dois, extent='device') +
  #geom_density2d(data=dados, aes(x=V1, y=V2), size=.1) +
  stat_density2d(data=dados, aes(x=V1, y=V2,  fill = ..level.., alpha = ..level..), size = 0.2, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  ggtitle("EPTC Pontos Comerciais")


ggmap(mh_map_set_dois) +
  stat_density2d(data = dados, aes(x = V1, y = V2, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "red", high = "green") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)



ggmap(mh_map_set_dois) %+% dados +
  aes(x = V1, y = V2, z = (V1*V2)) +
  stat_summary2d(fun = median, binwidth = c(.5, .5), alpha = 0.5) +
  scale_fill_gradientn(name = "Median", colours = terrain.colors(10), space = "Lab") +
  labs(x = "Longitude", y = "Latitude") +
  coord_map()





library(ggplot2)
library(ggthemes)
library(viridis) # devtools::install_github("sjmgarnier/viridis)
library(ggmap)
library(scales)
library(grid)
library(dplyr)
library(gridExtra)





#https://stackoverflow.com/questions/32148564/heatmap-plot-by-value-using-ggmap

dados$cut <- cut(dados$newrow, breaks=seq(0,1000,500), labels=sprintf("Score %d-%d",seq(0, 500, 500), seq(500,1000,500)))
gg <- ggmap(mh_map_set_dois)
gg <- gg + stat_density2d(data=dados, aes(x=V1, y=V2, colours= cut, fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=55)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg + labs(x=NULL, y=NULL, title="Score Distribution Across All Schools\n")
gg <- gg + theme_map(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg

#http://data-analytics.net/cep/Schedule_files/geospatial.html
ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = V1, y = V2, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = dados) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map")


ggmap(mh_map_set_dois) + 
  stat_density2d(aes(x = V1, y = V2,  fill = ..level..,alpha=..level..), bins = 100, geom = "polygon", data = dados) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map")


#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
ggmap(mh_map_set_dois) + 
  geom_tile(data = dados, aes(x = V1, y = V2, alpha = newrow),fill = 'red') +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())





#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
dados$newrow <- sample(100, size = nrow(dados), replace = TRUE)
ggmap(mh_map_set_dois) + 
  geom_point(data=dados, aes(x=V1, y=V2, color=newrow, size=0.1)) + 
  scale_color_gradient(low='yellow', high='red')





#https://datascienceplus.com/visualising-thefts-using-heatmaps-in-ggplot2/
ggmap(mh_map_set_dois) + 
  geom_tile(data = dados, aes(x = V1, y = V2, alpha = newrow),fill = 'red') +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())


library(splitstackshape)
dfexp <- expandRows(dados, "newrow")
ggmap(mh_map_set_dois)  %+% dados +
  aes(x = V1, y = V2) +
  stat_density_2d(aes(fill = ..level..), geom="polygon") +
  geom_point(position="jitter", alpha=.2, colour="white")  


ggmap(mh_map_set_dois)  %+% dados +
  aes(x = V1, y = V2) +
  stat_density_2d(aes(fill = ..level..), geom="polygon") +
  scale_colour_gradientn(colours=rainbow(4))



HoustonMap <- ggmap(mh_map_set_dois, extent = "device", legend = "topleft")
HoustonMap +
  stat_density2d(
    aes(x = V1, y = V2, fill = ..level..,
        alpha = ..level..),
    size = 2, bins = 60, data = dados,
    geom = "polygon")



#https://community.rstudio.com/t/plot-graphical-distribution-using-ggplot2-and-ggmap/4415/2
ggmap(mh_map_set_dois) + 
  geom_point(data=dados,aes(x = V1, y = V2,  z = newrow, fill = V1*V2)) + 
  geom_tile() + 
  #coord_equal() +
#  geom_contour(color = "white", alpha = 0.5) +
  #geom_point(data = , mapping = aes(Longitude, Latitude),shape=1, inherit.aes = FALSE)+
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0,0.4)) + 
  #scale_x_reverse()+
  theme_bw()+
  ggtitle("Chlorophyll-a distribution")+
  ylab("Latitude S") + xlab("Longitude E")+
  labs(fill = "Chl-a (mg/m3)")



#https://stackoverflow.com/questions/18285415/density2d-plot-using-another-variable-for-the-fill-similar-to-geom-tile

theme_set(theme_bw(base_size = 8))
colormap <- c("Violet","Blue","Green","Yellow","Red","White")
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                 binwidth = c(.001, .001),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Tempo",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)
ggsave(filename = "NYSubsamplePredStatMappng",
       plot = pred.stat.map,
       scale = 1,
       width = 5, height = 3,
       dpi = 300)



colormap <- c("Violet","Blue","Green","Yellow","Red","White")
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                  binwidth = c(.01, .01),
                  alpha = 0.5) + 
  scale_fill_gradientn(name = "Tempo",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)
ggsave(filename = "NYSubsamplePredStatMappng2",
       plot = pred.stat.map,
       scale = 1,
       width = 5, height = 3,
       dpi = 300)




theme_set(theme_bw(base_size = 8))
pred.stat.map <- ggmap(mh_map_set_dois) %+% dados + 
  aes(x = V1,
      y = V2,
      z = newrow) +
  stat_summary_2d(fun = median, 
                 binwidth = c(.005, .005),
                 alpha = 0.5) + 
  scale_fill_gradient2(low = "yellow", mid = "white", high = "red") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map)
