library(raster)
library(ggmap)

### Get data (map and polygons)
brz <- get_map('Porto Alegre', zoom = 4)
mymap <- getData("GADM", country = "brazil", level = 1)

### I realized that a few state names were not spelled properly.
### I changed them in order to subset polygons in the following
### process.

places <- c("Distrito Federal", "Bahia", "Ceará", "Espírito Santo",
            "Minas Gerais", "Paraná", "Rio de Janeiro", "Rio Grande do Sul",
            "Santa Catarina", "São Paulo")
ds_DadosAcessos <- places
ds_DadosAcessos$char <- ds_DadosAcessos$places

### Subset polygons for the states in your data. Then, convert my map
### to data frame, which is necessary for ggplot2.

mymap <- subset(mymap, NAME_1 %in% places)
temp <- fortify(mymap)

### Reorder ds_DadosAcessos by the state order in mymap
ds_DadosAcessos$order <- match(ds_DadosAcessos$char, mymap@data$NAME_1)
ds_DadosAcessos <- ds_DadosAcessos[order(ds_DadosAcessos$order), ]


### Finally, draw a map

ggmap(brz) +
  geom_map(data = temp, map = temp,
           aes(x = long, y = lat, group = group, map_id = id),
           color = "black", size = 0.2) +
  geom_map(data = ds_DadosAcessos, map = temp,
           aes(fill = val_Qtd, map_id = unique(temp$id)),
           alpha = 0.5) +
  scale_fill_gradientn(limits = c(min(ds_DadosAcessos$val_Qtd), max(ds_DadosAcessos$val_Qtd)),
                       colours = c("blue", "red") ) +
  theme(legend.position = "right")