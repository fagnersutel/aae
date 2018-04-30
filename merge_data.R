um <- read.csv("data_1-10410.csv", header = TRUE, sep = ";")
dois <- read.csv("data_10410-30374.csv", header = TRUE, sep = ";")
Tres <- read.csv("data_.csv", header = TRUE, sep = ";")

um$index <- NULL
dois$index <- NULL


dados1 <- rbind(um, dois)
dim(dados1)
View(dados1)

dados2 <- rbind(dados1, Tres)
dim(dados2)
View(dados2)

write.table(dados2,file="data.csv",sep=";",dec = " ", row.names=FALSE)


format(Sys.time(), "%d-%m-%Y-%X")

Sys.Date()
