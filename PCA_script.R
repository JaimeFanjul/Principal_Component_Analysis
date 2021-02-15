# Jaime Fanjul García
# Principal component analysis 
# Dimensional reduction

# Define working directory
#\""~
setwd("C:/Users/Jaime Fanjul/Desktop/MEBDS/Prep_Limpieza_Datos/Sesion3/DATA")

install.packages("factoextra")
install.packages("corrplot")
install.packages("psych")
install.packages("tidyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("latticeExtra")
install.packages("ggplot2")

library(factoextra)
library(corrplot)
library(psych)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


cpa=read.csv("cpa_serie.csv",sep=";")
cpa
summary(cpa)

str(cpa)

cpa$fec=dmy(cpa$fec)
cpa

#time series
library(latticeExtra)

c18 <- xyplot(c18 ~ fec, cpa, type = "l", lwd=1, col="blue",
              xlab="Tiempo", ylab = "CDT 180 días (%)")
c18

c36 <- xyplot(c36 ~ fec, cpa, type = "l", lwd=1, col="red",
              xlab="Tiempo", ylab = "CDT 360 días (%)")
c36
# --> Make the plot with second y axis AND legend:
doubleYScale(c18, c36)
update(doubleYScale(c18,c36,text = c("CDT 180", "CDT 360"),add.ylab2=TRUE),
       par.settings = simpleTheme(col=c("blue","red")))

#Pearson
cor=cor(na.omit(cpa[,c(2:8)]))
cor

corrplot(cor, method = "circle", order="hclust", addrect=2, main='Pearson')
col<- colorRampPalette(c("blue", "white", "red"))
heatmap(x = cor, symm = TRUE, main = "Pearson")

p_1=prcomp(na.omit(cpa[,c(2:8)]),scale. = TRUE)

p_1

fviz_eig(p_1)

(eig.val <- get_eigenvalue(p_1))


fviz_pca_var(p_1, geom = "text",axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title="Pearson"
             )

########## work

tra=read.csv("trafam.csv",sep=";")
View(tra)

summary(tra)

str(tra)


####
#Pearson
cor=cor(na.omit(tra[,c(2:15)]))
cor

col<- colorRampPalette(c("blue", "white", "red"))

heatmap(x = cor, symm = TRUE, main = "Pearson")

p_1=prcomp(na.omit(tra[,c(2:15)]),scale. = TRUE)
summary(p_1)
p_1


fviz_eig(p_1, main = "Pearson")

fviz_pca_var(p_1, geom = "text",axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title="Pearson"
)

(eig.val <- get_eigenvalue(p_1))

#polychoric

pol=polychoric(na.omit(tra[,c(2:15)]))
str(pol)

col<- colorRampPalette(c("blue", "white", "red"))

heatmap(x = pol$rho, symm = TRUE,main = "Policórica")

p_2=prcomp(pol$rho,scale. = TRUE)
p_2

fviz_eig(p_2,main = "Policórica")

fviz_pca_var(p_2, geom = "text",axes = c(1,2),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title = "Policórica")