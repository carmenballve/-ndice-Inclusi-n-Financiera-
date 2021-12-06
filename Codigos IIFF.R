library(ggplot2)
library(faraway)
library(FactoMineR)
library(cluster) 
library(gridExtra)
library(dplyr)
library(factoextra)
library(readxl) 
library(moments) 
library(ggsignif)



###############################################################################
###############################################################################
############################# PCA PARA ACCESO ################################# 
###############################################################################
###############################################################################

setwd("/Users/carmenballve/Documents/Tesis/DATA/ACCESO")
getwd()

datosA <- read_xlsx("ACCESOT.xlsx")
View(datosA)

matriz_corA <- cor(datosA)
matriz_corA

matriz_covA <- cov(datosA)
matriz_cov

eigenA <- eigen(matriz_cov)
eigenA

eigenvaluesA <- eigen$values


summary(datosA$PDA)
summary(datosA$`PDA CADA 10.000 HABITANTES`)
summary(datosA$`PUNTOS DE EXTRACCION EXTRABANCARIOS*`)
summary(datosA$`% LOCALIDADES CON AL MENOS 1 PDA`)

pca1 <- prcomp(datosA, scale = TRUE)
pca1


##Podemos ver que la priemr componente principal pondera de manera muy similar a als 4 variables 
##signficnado que estan bastante correlacionadas entre si. 
## la menos correlacionada es puntos de extraccion extrabancarios. 

summary(pca1)

#la primera componente explica el 91% de la variabilidad en los datos que  tengo

plot(pca1, type = "l")

# ya nos muestra qu edebemos esocger la primera comonente pricnipal (analisis del codo)

pca1$center

pca1$scale

##rotation --> loadings de los componentes principales 
##(cada columna contiene el vector de loadings de cada componente principal)

pca1$rotation

head(pca1$x)

dim(pca1$x)

## Mediante la función biplot()	se puede obtener una 
## representación bidimensional de las dos primeras componentes

x11()
biplot(x = pca1, sacale = 0,  cex = 0.6, col  = c("blue4", "brown3"))

## todas tienen una direccion horizontal significando que estan mejor expicadas por PC1. 

fviz_screeplot(pca1, addlabels = TRUE, ylim = c(0, 100))

prop_varianza <- pca1$sdev^2 / sum(pca1$sdev^2)
prop_varianza

par(mfrow = c(1,2))

plot(prop_varianza, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(prop_varianza), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")

acceso <- sum(pca1$rotation[,1]*eigenvaluesA)
acceso
acceso1<- acceso/sum(eigenvaluesA)
acceso1

acceso_dato <- apply(pca1$rotation[,1]*datosA,1,sum)
acceso_dato

datosA$acceso_dato <- acceso_dato

datosA[,1:4] <- NULL

###############################################################################
###############################################################################
############################# PCA PARA USO ####################################
###############################################################################
###############################################################################


setwd("/Users/carmenballve/Documents/Tesis/DATA/USO")

DatosU <- read_xlsx("USOT.xlsx")

matriz_corU <- cor(DatosU)
matriz_corU

matriz_covU <- cov(DatosU)
matriz_covU

eigenU <- eigen(matriz_covU)
eigenU

eigenvaluesU <- eigenU$values

summary(DatosU$`CANT PLAZOS FIJOS CADA 10.000 ADULTOS ($ Y UVA)`)
summary(DatosU$`PROMEDIO MENSUAL DE LA CANTIDAD DE PAGOS CON TARJETA DE DEBITO POR ADULTO`)
summary(DatosU$`PROMEDIO MENSUAL DE LA CANTIDAD DE PAGOS CON TARJETA DE CREDITO POR ADULTO`)
summary(DatosU$`% ADULTOS CON AL MENOS 1 CBU`)
summary(DatosU$`% ADULTOS CON AL MENOS 1 CVU`)
summary(DatosU$`% DE ADULTOS CON DEUDA EN EL SISTMEA FINANCIERO AMPLIADO`)

pca2 <- prcomp(DatosU, scale = TRUE)
pca2

summary(pca2)

## 2 pca, el 3 agrega muy poco 

dim(pca2$x)

par(mfrow = c(1,1))
biplot(x = pca2, sacale = 0,  cex = 0.4, col  = c("blue4", "brown3"))

fviz_screeplot(pca2, addlabels = TRUE, ylim = c(0, 100))
fviz_contrib(pca2, choice = "var", axes = 1, top = 10)

prop_varianzaU <- pca2$sdev^2 / sum(pca2$sdev^2)
prop_varianzaU

par(mfrow = c(1,2))

plot(prop_varianzaU, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(prop_varianzaU), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")

uso <- sum(pca2$rotation[,1]*eigenvaluesU)
uso
uso1<- uso/sum(eigenvaluesU)
uso1

uso_dato <- apply(pca2$rotation[,1]*DatosU,1,sum)
uso_dato

DatosU$uso_dato <- uso_dato

DatosU[,1:4] <- NULL

###############################################################################
###############################################################################
############################# PCA PARA BARRERAS ###############################
###############################################################################
###############################################################################

setwd("/Users/carmenballve/Documents/Tesis/DATA/BARRERAS")

DatosB <- read_xlsx("BARRERAST.xlsx")

matriz_corB <- cor(DatosB)
matriz_corB

matriz_covB <- cov(DatosB)
matriz_covB

eigenB <- eigen(matriz_covB)
eigenB

eigenvaluesB <- eigenB$values

summary(DatosB$`If does not have account: b/c too far away`)
summary(DatosB$`If does not have account: b/c too expensive`)
summary(DatosB$`If does not have account: b/c lack documentation`)
summary(DatosB$`If does not have account: b/c lack trust`)
summary(DatosB$`If does not have account: b/c religious reasons`)
summary(DatosB$`If does not have account: b/c lack of money`)
summary(DatosB$`If does not have account: b/c family member already has one `)
summary(DatosB$`If does not have account: b/c no need for financial services `)

pca3 <- prcomp(DatosB, scale = TRUE)
pca3 

dim(pca3$x)

summary(pca3)

par(mfrow = c(1,1))
biplot(x = pca3, sacale = 0,  cex = 0.4, col  = c("blue4", "brown3"))

fviz_screeplot(pca3, addlabels = TRUE, ylim = c(0, 100))
fviz_contrib(pca3, choice = "var", axes = 1, top = 10)

prop_varianzaB <- pca3$sdev^2 / sum(pca3$sdev^2)
prop_varianzaB

par(mfrow = c(1,2))

plot(prop_varianzaB, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(prop_varianzaB), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")


barreras <- sum(pca3$rotation[,1]*eigenvaluesB)
barreras
barreras1<- barreras/sum(eigenvaluesB)
barreras1

barreras_dato <- apply(pca3$rotation[,1]*DatosB,1,sum)
barreras_dato

DatosB$barreras_dato <- barreras_dato

DatosB[,1:4] <- NULL


###############################################################################
###############################################################################
############################## ARMADO DEL INDICE ##############################
###############################################################################
###############################################################################

data_indice<- data_frame(datosA, DatosU, DatosB)
data_indice

matriz_corI <- cor(data_indice)
matriz_corI

matriz_covI <- cov(data_indice)
matriz_covI

eigenI <- eigen(matriz_covI)
eigenI

eigenvaluesI <- eigenI$values


pcai<- prcomp(data_indice, scale = TRUE)
pcai

summary(pcai)

dim(pcai$x)

par(mfrow = c(1,1))
biplot(x = pcai, sacale = 0,  cex = 0.4, col  = c("blue4", "brown3"))

fviz_screeplot(pcai, addlabels = TRUE, ylim = c(0, 100))
fviz_contrib(pcai, choice = "var", axes = 1, top = 10)

pcai$rotation

prop_varianzaI <- pcai$sdev^2 / sum(pcai$sdev^2)
prop_varianzaI

par(mfrow = c(1,2))

plot(prop_varianzaB, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(prop_varianzaB), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")

iiff <- sum(pcai$rotation*eigenvaluesI)
iiff
iiff1<- iiff/sum(eigenvaluesI)
iiff1





