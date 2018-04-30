setwd("/Users/fsmoura/Desktop/docs/")
#http://www.datacarpentry.org/R-spatial-raster-vector-lesson/06-vector-open-shapefile-in-r/
#https://stackoverflow.com/questions/27906021/merge-2-columns-into-one-in-dataframe
list.files()

library(sf)
library(raster)  
eix_log <- st_read("eixoslogradouros_22032018/EixosLogradouros_22032018.shp")  
head(eix_log)
View(eix_log)
subueu <- st_read("pddua_shp/CONV_SUBUEU.shp")  
head(subueu)
View(subueu)
ueu <- st_read("pddua_shp/CONV_UEU.shp")  
head(ueu)
View(ueu)

#library(xlsx)
#alvara_com <- read.xlsx("servico/servico.xlsx", 1)

servicos <- read.csv("servico/servico.csv", header = TRUE, sep = ";")
comercio <- read.csv("comercio/comercio.csv", header = TRUE, sep = ";")
names(servicos)
names(comercio)
View(servicos)
View(comercio)






#st_geometry_type(ueu)  
#st_crs(ueu)
#plot(ueu, col = "cyan1", border = "black", lwd = 3, main = "AOI Boundary Plot")

