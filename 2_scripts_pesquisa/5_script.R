#### Mclust e Sm.Density #############
#outros testes para avaliar heterogeinidade que utilizei, mas como não tive muito tempo para estudá-los melhor e saber usar melhor o teste e interpretar, acabei nãoutilizando. Outro teste bastante usado para isso é o Kmeans.

#bibliotecas utilizadas
library(raster)
library(sp)
#teste mclust
library(mclust)
abrir o raster da cobertura vegetal para a zte 125
setwd("/home/gessica/Documents/Dados/imagens tif/tree") #imagem tree cover
eco125 <- raster("zte125.tif")

#abrir o raster do mapbiomas para a zte 125
setwd("/home/gessica/Documents/Dados/imagens tif/mapbio3/") # imagem map biomas
e125<- raster("ztee125.tif")

# colocando as duas imagens em mesma projeção
eco125Points <-rasterToPoints(eco125) 
coords125 <- SpatialPoints(eco125Points[,1:2],
                           proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#extraindo e colocando na tabela-dataframe
aux <- extract(e125, coords125)
mtx125 <- data.frame(cbind(eco125Points, aux))


#separando apenas florestas e savanas
mtx125n <- mtx125[which(mtx125[,4] == 3 | mtx125[,4] == 4), ] # for + sav
mtx125f <- mtx125[which(mtx125[,4] == 3), ] # floresta
mtx125s <- mtx125[which(mtx125[,4] == 4), ] # savana

 ###############  MCLUST ################
#teste mclust
gmm125n = Mclust(mtx125n[,3])
summary(gmm125n)


 ####### SM.DENSITY  ################
#teste sm.density
library(sm)

t <- sm.density.compare(mtx125n[,3], as.factor(mtx125n[, 4]), h = 3, model = "equal", 
                        xlab="Tree Cover", ylab= "Density")+
  title(main="Density of Savanna and Florest (ZTE-125-MA) ")
  legend("topleft", c("Florest", "Savanna"),
  col=c("red", "green"), lty=1:2, cex=0.8, box.lty=0)


