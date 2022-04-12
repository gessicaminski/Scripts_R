###### SD e IQR #########
#Para quantificar a heterogeneidade espacial da vegetação de cada ZTE a partir da %CA,utilizaram-se duas métricas estatísticas detectar a variabilidade de um conjunto de dados para melhor entender seu comportamento:
#Desvio Padrão (sd): é uma medida de dispersão e o seu valor reflete a variabilidade das observações em relação à média. 
#Intervalo interquartil (IQR): foi desenvolvido a fim de avaliar o grau de dispersão dos dados em torno da medida central (mediana). 

#pontos do tree cover 

eco125 <- raster("zte125.tif")
eco125Points <-rasterToPoints(eco125)

#valores como mediana, sd, maximae minima 
median(eco125Points[,3])/mean(eco125Points[,3]) #mediana
sd(eco125Points[,3])/mean(eco125Points[,3]) #sd
max(eco125Points[,3])/mean(eco125Points[,3]) #max
min(eco125Points[,3])/mean(eco125Points[,3]) #min



### LOOP ####
numero<- c(103, 116, 117, 119, 120, 89, 94, 99, 100, 32, 35, 40, 41, 42 ,  43, 44, 46, 49, 54, 55, 63, 67, 70, 82, 90, 91, 92, 93, 97, 185, 187, 188, 191, 197, 199, 145,  95, 107, 108, 109, 125)

setwd("/home/gessica/Documents/imagens tif/tree/")  

for(i in numero){
  
  eval(parse(text = paste0('eco125 <- raster("ecoSel',as.character(i),'.grd")'))) 
  
  eco125Points <-rasterToPoints(eco125)
  
  print(paste0("Mediana ",i,": ", median(eco125Points[,3])/mean(eco125Points[,3])))
  print(paste0("Sd ",i,": ", sd(eco125Points[,3])/mean(eco125Points[,3])))
  print(paste0("Max ",i,": ", max(eco125Points[,3])/mean(eco125Points[,3])))
  print(paste0("Min ",i,": ", min(eco125Points[,3])/mean(eco125Points[,3])))
}
  





