#### HISTOGRAMA + DENSIDADE (linha)
#analise exploratória
##só um histograma para interpretação visual


s#bibliotecas utilizadas
library(raster)
library(sp)

#abrir o raster da cobertura vegetal para a zte 125
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

#density(mtx125n$ecoSel99)
#hist(mtx125n$ecoSel99, xlab="Tree Cover", ylab= "Frequency")
#plot (density(mtx125n$ecoSel99), xlab="Tree Cover")
#plot (density(mtx125n$ecoSel99,adjust = 2), xlab="Tree Cover")

hist(mtx125n$zte125, # histogram do tree cover
col="springgreen3", # column color
border="black",
prob = TRUE, # show densities instead of frequencies
xlab = "Tree Cover",
main = "Histogram of Savanna and Florest (ZTE 99 - PA) ")
lines(density(mtx125n$zte125, adjust = 5), # density plot
lwd = 1, # thickness of line
col = "black")


### LOOP ####
numero<- c(103, 116, 117, 119, 120, 89, 94, 99, 100, 32, 35, 40, 41, 42 ,  43, 44, 46, 49, 54, 55, 63, 67, 70, 82, 90, 91, 92, 93, 97, 185, 187, 188, 191, 197, 199, 145,  95, 107, 108, 109, 125)


setwd("/home/gessica/Documents/Dados/imagens tif/tree/")  

for(i in numero){
  
  eval(parse(text = paste0('eco125 <- raster("zte',as.character(i),'.tif")'))) 


  setwd("/home/gessica/Documents/Dados/imagens tif/mapbio3/")
  
  eval(parse(text = paste0('e125 <- raster("zte',as.character(i),'.tif")'))) 

  
  eco125Points <-rasterToPoints(eco125) # colocando as duas imagens em mesma projeção
  coords125 <- SpatialPoints(eco125Points[,1:2],
                             proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  aux <- extract(e125, coords125)
  mtx125 <- data.frame(cbind(eco125Points, aux))
  mtx125n <- mtx125[which(mtx125[,4] == 3 | mtx125[,4] == 4), ] # for + sav
  mtx125f <- mtx125[which(mtx125[,4] == 3), ] # for
  mtx125s <- mtx125[which(mtx125[,4] == 4), ] # sav
  eval(parse(text = paste0('ge<- mtx125n$"ecoSel', as.character(i),')")')
  

  hist= (ge,
       col="springgreen3", 
       border="black",
       prob = TRUE, 
       xlab = "Tree Cover",
       main = paste0("Histogram of Savanna and Florest (ZTE " , as.character(i),") ")
       lines(density(ge, adjust = 5),
lwd = 1, # thickness of line
col = "black")
 dev.off()
  
}

