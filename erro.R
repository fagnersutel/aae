origAddress$addresses[5]
Encoding(origAddress$addresses[5]) <- "bytes"
origAddress$addresses[5]
geo_reply = geocode(origAddress$addresses[5], output='all', messaging=TRUE, override_limit=TRUE)
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