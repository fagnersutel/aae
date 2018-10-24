dados = read.csv('1540367094.76644.csv', header = TRUE, sep = ";")
plot(dados$long, dados$lat, pch = 16, col=rgb(0,0,0,alpha=0.1))
dados2 <- dados[dados$lat < -29.929, ]
plot(dados2$long, dados2$lat)
dados2 <- dados2[dados2$long < -51.02595, ]
plot(dados2$long, dados2$lat, pch = 16, col=rgb(0,0,0,alpha=0.1))
