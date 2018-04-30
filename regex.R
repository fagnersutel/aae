mywords<- c("AV SERTORIO 6500","met","sally","subway 10","1800Movies","12345") 
numbers <- grepl("^[[:digit:]]+$", mywords) 
letters <- grepl("^[[:alpha:]]+$", mywords) 
both <- grepl("^[[:digit:][:alpha:]]+$", mywords) 

mywords[letters] 
mywords[numbers] 
mywords[xor((letters | numbers), both)] # letters & numbers mixed 

  

mywords <- origAddress$addresses
numbers <- grepl("^[[:digit:]]+$", mywords) 
letters <- grepl("^[[:alpha:]]+$", mywords) 
both <- grepl("^[[:digit:][:alpha:]]+$", mywords) 
mywords[xor((letters | numbers), both)] # letters & number

mywords <- origAddress$addresses[1]
mywords
numbers <- grepl("^[[:digit:]]+$", mywords) 
letters <- grepl("^[[:alpha:]]+$", mywords) 
both <- grepl("^[[:digit:][:alpha:]]+$", mywords) 
mywords[letters] 
mywords[numbers] 
mywords[xor((letters | numbers), both)] # letters & numbers mixed 


origAddress$final <- ifelse(stri_count(origAddress$Endereco, regex="(?i),"), (origAddress$Endereco), "erro")
head(origAddress,1)


x <- "AV SERTORIO 6500" 


library(stringi)

stri_count(str, regex="hello")

str1 <- "It is very good to speak, like thevery good"
stri_count(str1, regex="(?i)speak ,")
stri_count(origAddress$Endereco[1], regex="(?i),")
