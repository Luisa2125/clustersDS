library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(dplyr)
library(VIM)
library(lubridate)


datos<-read.csv("./FoodPrices.csv")
#se obtienen los datos numericos de la matriz
nums <- unlist(lapply(datos, is.numeric)) 
#se usan esos datos para sacar el km y crear los grupos
numDatos1<-datos[ ,nums]
km1<-kmeans(numDatos1,4)
datos$grupo<-km1$cluster

#obtenemos el grupo de datos por cluster
g1<- datos[datos$grupo==1,]
prop.table(table(g1$Species))*100
g2<- datos[datos$grupo==1,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==1,]
prop.table(table(g3$Species))*100
g4<- datos[datos$grupo==1,]
prop.table(table(g4$Species))*100

#ploteamos los datos, al tener muchos rgistros se demora bastante en plotear
plotcluster(numDatos1,km1$cluster) 
