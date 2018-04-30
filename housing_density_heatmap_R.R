# Tartu housing density map ###
library(ggplot2)
library(ggmap)

tartu_housing_xy_wgs84_a <- read.csv("C:/ANTO/loengud/GIS Maps and Spatial Analyses for Urban Planning/data/accessibility/tartu_housing_xy_wgs84_a.csv", sep=";")
tartu_map <- get_map(location="tartu", maptype="satellite", zoom = 12)
ggmap(tartu_map, extent='device') +geom_point(aes(x=lon, y=lat), colour="red", alpha=0.1, size=2, data=tartu_housing_xy_wgs84_a)

# change parameters
tartu_map_g_str <- get_map(location="tartu", zoom = 13) 
ggmap(tartu_map_g_str, extent='device') +
geom_density2d(data=tartu_housing_xy_wgs84_a, aes(x=lon, y=lat), size=.3) +
stat_density2d(data=tartu_housing_xy_wgs84_a, aes(x=lon, y=lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0, 0.30), guide = FALSE)

# sample for individual work #####
jg_housing_xy_wgs84 <- read.csv("C:/ANTO/loengud/GIS Maps and Spatial Analyses for Urban Planning/data/accessibility/jg_housing_xy_wgs84.csv", sep=";")
jogeva_map_g_str <- c(lon = 26.4, lat = 58.7)
jogeva_map_g_str_m <- get_map(location=jogeva_map_g_str, zoom = 9)
ggmap(jogeva_map_g_str_m, extent='device') +
geom_density2d(data=jg_housing_xy_wgs84, aes(x=lon, y=lat), size=.3) +
stat_density2d(data=jg_housing_xy_wgs84, aes(x=lon, y=lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
scale_fill_gradient(low = "green", high = "red", guide=FALSE) +scale_alpha(range = c(0, 0.30), guide = FALSE)


### jg country side housing density ####
housing_sett_type8_jg_wgs84_aa <- read.csv("C:/ANTO/loengud/GIS Maps and Spatial Analyses for Urban Planning/data/accessibility/housing_sett_type8_jg_wgs84_aa.csv", sep=";")
jogeva_map_g_str_m <- get_map(location=jogeva_map_g_str, maptype="hybrid", zoom = 9)
ggmap(jogeva_map_g_str_m, extent='device') +
geom_density2d(data=housing_sett_type8_jg_wgs84_aa, aes(x=lon, y=lat), size=.3) +
stat_density2d(data=housing_sett_type8_jg_wgs84_aa, aes(x=lon, y=lat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
scale_fill_gradient(low = "green", high = "red", guide=FALSE) +scale_alpha(range = c(0, 0.30), guide = FALSE)
