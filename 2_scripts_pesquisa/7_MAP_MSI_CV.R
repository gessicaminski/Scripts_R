###### MAP, MSI e CV #######
###Dados de precipitação do Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS) (0,05º, 1981 – 2017)

# Calculou-se o regime de chuvas para todo o período disponível (1981 – 2017), i.e., 37 anos. Como cada ZTE compreende uma área, ou seja, contém mais de um pixel, as métricas associadas ao regime de precipitação de cada ZTE foram calculadas a partir do centróide  da área total da ZTE. 
#A partir deste conjunto de dados, caracterizou-se o regime de chuvas para cada uma das ZTEs, a partir de três métricas: 
#i.	média dos totais anuais de precipitação (MAP, sigla em inglês): 
#ii.	variabilidade intra-anual (sazonalidade)
#iii.	variabilidade interanual: calculada a partir do coeficiente de variação (CV) dos acumulados anuais dos 37 anos, definido como a razão entre o desvio padrão dos valores e a média (MAP). 


library(raster)
library(rgeos)
library(rgdal)
library(ncdf4)
library(dplyr)
library(sp)


# inicializando as funcoes
source('/home/gessica/Documents/scripts R/climatic indices/ap.R')
source('/home/gessica/Documents/scripts R/climatic indices/msi.R')
source('/home/gessica/Documents/scripts R/climatic indices/mwd.R')
source('/home/gessica/Documents/scripts R/climatic indices/rasterCrop.R')

###dados chirps
setwd('/home/gessica/Documents/Dados/climate/chirps_monthly_v2.0/')
ncP <- nc_open("chirps-v2.0.monthly_TSA.nc", readunlim = FALSE)
prec <- ncvar_get(ncP, "precip")

#shapefile
setwd("/home/gessica/Documents/Dados/Limites/mapas ibge/")
veg <- readOGR(dsn = '.' , layer = 'Vegetation')

# Selecionar Areas de tensão ecologica
ate <- which(veg@data$TIPO2 == "Área de Tensão Ecológica", arr.ind = T)
#plot(veg[ate,])
aten <-veg[ate,]

#Boxplot + Histograma + Mediana + Area e comprimento + Rasterplot

numero <- c(103, 116, 117, 119, 120, 89, 94, 99, 100, 32, 35, 40, 41, 42 ,  43, 44, 46, 49, 54, 55, 63, 67, 70, 82, 90, 91, 92, 93, 97, 185, 187,188, 191, 197, 199, 145,  95, 107, 108, 109, 125)

#LOOP MAP
setwd("/home/gessica/Documents/imagens tif/tree/")  

for(i in numero){
  
eval(parse(text = paste0('eco125 <- raster("zte',as.character(i),'.grd")'))) 
  
app <- ap(prec)
map <- apply(app, 1:2,mean)
sdp <- apply(app, 1:2, sd)
  
  
  # conversor pro raster
mapeco125 <- rasterCrop(map, aten[i,])
  #plot(mapeco125)
jpeg(filename = paste0("../../../Documents/Results/histmap",as.character(i)),
       width = 900, height = 600, units = "px")
  hist(mapeco125, xlab="mm", ylab= "Frequency",
       main =paste0( "Median Annual precipitation (Ecotono ",as.character(i),")"))
  dev.off()
  
  jpeg(filename = paste0("../../../Documents/Results/boxplotmap",as.character(i)),
       width = 800, units = "px")
  boxplot(mapeco125, xlab= "", ylab="mm",
          main =paste0("Median Annual precipitation (Ecotono ",as.character(i),")"))
  dev.off()
  
  print(paste0("Mediana do MAP do ecótono ",i,": ", median(values(mapeco125), na.rm = T), " mm/ano"))
}



# CV
setwd("/home/gessica/Documents/imagens tif/tree/")  

for(i in numero){
  
eval(parse(text = paste0('eco125 <- raster("zte',as.character(i),'.grd")'))) 
# cv e sdp dividido por map
app <- ap(prec)
map <- apply(app, 1:2,mean)
sdp <- apply(app, 1:2, sd)
cvp <- sdp/map

# conversor pro raster
cvpeco125 <- rasterCrop(cvp, aten[i,])
jpeg(filename = paste0(".../../../Documents/Results/boxplotcvp",as.character(i)),
     width = 800, units = "px")
boxplot(cvpeco125, xlab= "", ylab="mm",
        main =paste0("Coefficient of variation (Ecotono ",as.character(i),")"))
dev.off()
jpeg(filename = paste0("../../../Documents/Results/histcvp",as.character(i)),
     width = 900, height = 600, units = "px")
hist(cvpeco125, xlab= "mm", ylab="Frequency", 
     main =paste0( "Coefficient of variation (Ecotono ",as.character(i),")"))
dev.off()
print(paste0("Mediana do CVP do ecótono ",i,": ", median(values(cvpeco125), na.rm = T), " mm/ano"))
}


# MSI
setwd("/home/gessica/Documents/imagens tif/tree/")  

for(i in numero){
  
eval(parse(text = paste0('eco125 <- raster("zte',as.character(i),'.grd")'))) 

apm <- msi(prec)
msim <- apply(apm, 1:2,mean)
sdm <- apply(apm, 1:2, sd)

# conversor pro raster
msieco125 <- rasterCrop(msim, aten[i,])
#plot(msieco125)

# Boxplot e Histograma
jpeg(filename = paste0("../../../Documents/Results/histmsi",as.character(i)),
     width = 900, height = 600, units = "px")
hist(msieco125, xlab="MSI", ylab= "",
     main =paste0("Markan Seasonality Index (Ecotono ",as.character(i),")"))
dev.off()
jpeg(filename = paste0("../../../Documents/Results/boxplotmsi",as.character(i)),
     width = 800, units = "px")
boxplot(msieco125, xlab="", ylab= "MSI",
        main =paste0("Markan Seasonality Index (Ecotono ",as.character(i),")"))
dev.off()
print(paste0("Mediana do MSI do ecótono ",i,": ", median(values(msieco125), na.rm = T), " mm/ano"))
}

##dados foram salvos em uma tabela csv no excel