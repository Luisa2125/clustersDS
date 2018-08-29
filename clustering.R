# install.packages("e1071")
# install.packages("mclust")
# install.packages("fpc")
# install.packages("dplyr")
# install.packages("VIM")
# install.packages("lubridate")
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(dplyr)
library(VIM)
library(lubridate)

#obtencion de datos
datos<-read.csv("./FoodPrices.csv")
summary(datos)

#se dividieron los datos por year (5 por subset) ya que habian demasiados registros
#por cada grupo de 5 se realizo la agrupacion de clusters.
datos1<-subset(datos,mp_year >= 1992 & mp_year <= 1996)
#distancia 
d1 <- dist(datos1, method = "euclidean") 
fit1 <- hclust(d1, method="ward.D") 
plot(fit1)#dendograma
groups <- cutree(fit1, k=5)
rect.hclust(fit1, k=5, border="red") #dendograma dividido por clusters

#datos de 1997 a 2001---------------------------------------------------------
datos2<-subset(datos,mp_year >= 1997 & mp_year <= 2001)
d2 <- dist(datos2, method = "euclidean")
fit2 <- hclust(d2, method="ward.D") 
plot(fit2)
groups2 <- cutree(fit2, k=5)
rect.hclust(fit2, k=5, border="red")

#datos de 2002 a 2006---------------------------------------------------------
datos3<-subset(datos,mp_year >= 2002 & mp_year <= 2006)
d3 <- dist(datos1, method = "euclidean")
fit3 <- hclust(d3, method="ward.D") 
plot(fit3)
groups3 <- cutree(fit3, k=5)
rect.hclust(fit3, k=5, border="red")

#datos de 2007 a 2011---------------------------------------------------------
datos4<-subset(datos,mp_year >= 2007 & mp_year <= 2011)
d4 <- dist(datos1, method = "euclidean")
fit4 <- hclust(d4, method="ward.D") 
plot(fit4)
groups4 <- cutree(fit4, k=5)
rect.hclust(fit4, k=5, border="red")

#datos de 2012 a 2017---------------------------------------------------------
datos5<-subset(datos,mp_year >= 2012 & mp_year <= 2017)
d5 <- dist(datos1, method = "euclidean")
fit5 <- hclust(d5, method="ward.D")
plot(fit5)
groups5 <- cutree(fit5, k=5)
rect.hclust(fit5, k=5, border="red")