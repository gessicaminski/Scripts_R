library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)
library(dplyr)
library(sp)



# abrir os estados brasileiros
setwd("C:/Users/gessi/Documents/Limites/estados_2010/")
est <- readOGR(dsn = '.' , layer = 'estados_2010')
plot(est, add=T)

# Biomas 
setwd("C:/Users/gessi/Documents/Limites/biomas_brasileiros/")
bio <- readOGR(dsn = '.' , layer = 'BR_BIOMAS_IBGE', encoding="UTF-8")
plot(bio)


# abrir o shapefile das ZTEs
setwd("C:/Users/gessi/Documents/Limites/mapas ibge/")
veg <- readOGR(dsn = '.' , layer = 'Vegetation', encoding="UTF-8")


# selecionar apenas as ZTEs
ate <- which(veg@data$TIPO2 == "Área de Tensão Ecológica", arr.ind = T)
fod <- which(veg$TIPO == "Floresta Ombrófila Densa ", arr.ind = T)       
#plot(veg[ate,], add=T)
aten <-veg[ate,]

#plotar as ZTEs de uma cor só 
#plot(veg[ate,], add=T, border ="chocolate4")
#plot(veg[ate,], add=TRUE, lwd = 3, border = NA, col = "chocolate4")


##plotando os biomas com cores diferentes
plot(bio[bio$CD_LEGENDA == "MATA ATLÂNTICA",], add=T, col = adjustcolor("darkolivegreen3", alpha.f =  0.3))   
plot(bio[bio$CD_LEGENDA == "AMAZÔNIA",], add=T, col = adjustcolor("darkgreen", alpha.f =  0.3))     
plot(bio[bio$CD_LEGENDA == "CERRADO",], add=T, col = adjustcolor("darkgoldenrod2", alpha.f =  0.3))   
plot(bio[bio$CD_LEGENDA == "PAMPA",], add=T, col = adjustcolor("gold", alpha.f =  0.3))  
plot(bio[bio$CD_LEGENDA == "PANTANAL",], add=T, col = adjustcolor("aquamarine2", alpha.f =  0.3))    
plot(bio[bio$CD_LEGENDA == "CAATINGA",], add=T, col = adjustcolor("red", alpha.f =  0.3))

## é pra deixar uma linha maiis escura que outra
MA <- bio [bio@data$CD_LEGENDA=="CERRADO",]
lines(MA, col="gray21", lwd=0.5)

MA <- bio [bio@data$CD_LEGENDA=="CAATINGA",]
lines(MA, col="gray21", lwd=1)

MA <- bio [bio@data$CD_LEGENDA=="MATA ATLÂNTICA",]

lines(MA, col="gray21", lwd=1)
plot (bio, lwd=0.5)
lines(bio, col= "gray72", lwd= 0.5)
lines(est, col= "gray72", lwd= 0.5)


##plotar as ztes com cores diferentes conforme o tipo de  transição
#savana/floresta ombrofila
pol1 <- (aten[107,])
plot(pol1, add=T, col="darkgreen", border = NA) 
pol1 <- (aten[108,])
plot(pol1, add=T, col="darkgreen", border = NA) 
pol1 <- (aten[109,])
plot(pol1, add=T, col="darkgreen", border = NA)
pol1 <- (aten[120,])
plot(pol1, add=T, col="darkgreen", border = NA)
pol1 <- (aten[116,]) 
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[97,])
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[99,]) 
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[94,])
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[95,]) 
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[100,])
plot(pol1, add=T, col= "darkgreen", border = NA)
pol1 <- (aten[49,])
plot(pol1, add=T, col="darkgreen", border = NA)
pol1 <- (aten[54,])
plot(pol1, add=T, col="darkgreen", border = NA)

pol1 <- (aten[191,])
plot(pol1, add=T, col= "darkgreen", border = NA)


#savana/floresta estacional
pol1 <- (aten[125,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[145,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[103,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[117,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[119,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[43,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[44,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[46,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[63,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[67,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[80,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[82,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[90,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[91,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[93,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[101,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[199,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[197,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[185,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[56,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[70,])
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[188,]) 
plot(pol1, add=T, col= "turquoise4", border = NA)
pol1 <- (aten[35,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[40,])
plot(pol1, add=T, col="turquoise4", border = NA)
pol1 <- (aten[28,])
plot(pol1, add=T, col="turquoise4", border = NA)

#Floresta ombrofila/floresta estacional
pol1 <- (aten[41,]) 
plot(pol1, add=T, col= "mediumspringgreen", border = NA)
pol1 <- (aten[42,])
plot(pol1, add=T, col= "mediumspringgreen", border = NA)
pol1 <- (aten[52,]) 
plot(pol1, add=T, col= "mediumspringgreen", border = NA)
pol1 <- (aten[53,])
plot(pol1, add=T, col= "mediumspringgreen", border = NA)
pol1 <- (aten[55,])
plot(pol1, add=T, col="mediumspringgreen", border = NA)
pol1 <- (aten[92,])
plot(pol1, add=T, col="mediumspringgreen", border = NA)
pol1 <- (aten[32,])
plot(pol1, add=T, col="mediumspringgreen", border = NA)
pol1 <- (aten[187,])
plot(pol1, add=T, col="mediumspringgreen", border = NA)
