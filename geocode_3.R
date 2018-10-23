rm(list = ls(all.names = TRUE))
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
setwd('~/OneDrive/Cursos/meusR/docs/')
library(ggmap)
register_google("AIzaSyBA2Kt4jMYJe3up70J0Kl48Idrm69K6n0I")
#Carga de dados
origAddress <- read.csv("comercio/comercio.csv", header = TRUE, sep = ";")
#Verificamos o tamanho do data set
dim(origAddress)
#View(origAddress)
#Removemos os duplicados
origAddress <- origAddress[!duplicated(origAddress$Empresa), ]
#Verificamos o tamanho do data set
dim(origAddress)
#criamos a variavel de pesquisa
origAddress$addresses <- origAddress$Endereco
#agregamosao endereço o bairro e cidade
origAddress$addresses <- paste(origAddress$Endereco,  origAddress$Bairro, "Porto Alegre", sep = " ")
origAddress$addresses <- paste(origAddress$addresses, "&key=MYKEY", sep = "")
#Eliminamos a virgula
origAddress$addresses <- gsub(",", "", origAddress$addresses)
#fazemos o tipecast
origAddress$addresses <- as.character(origAddress$geoAddress)
#verificamos o resultado final
head(origAddress)
#definimoso range da pesquisa
origAddress<- origAddress[1:length(origAddress$addresses),]
length(origAddress)
#fazemos a restricao
#addresses <- origAddress$addresses
addresses <- origAddress
#verificamos o tamanho
length(addresses)
#executamosa funcao
getGeoDetails <- function(address){
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausando as:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(1)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  if (geo_reply$status != "OK"){
    return(answer)
  }
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}
geocoded <- data.frame()

startindex <- 1

for (ii in seq(startindex, length(addresses$addresses))){
  print(paste("Trabalhando no item ", ii, " de", length(addresses$addresses)))
  
  result = getGeoDetails(addresses$addresses[ii]) 
  print(result$status)
  result$index <- ii
  
  geocoded <- rbind(geocoded, result)
  addresses$lat[ii] = result$lat
  addresses$long[ii] = result$long
  addresses$accuracy[ii] = result$accuracy
  addresses$formatted_address[ii] = result$formatted_address
}

#head
geocoded$ramo = "Comercio"
geocoded$address_type<-NULL
geocoded$status<-NULL
geocoded$lat <- gsub(" ", ".", geocoded$lat)
geocoded$long<- gsub(" ", ".", geocoded$long)
head(geocoded)
setwd('~/OneDrive/Cursos/meusR/docs/geo')
data <- paste(as.character(as.numeric(Sys.time())), "csv", sep = ".")
write.table(geocoded,file=data,sep=";",dec = " ", row.names=FALSE)
library(beepr)
beep()
