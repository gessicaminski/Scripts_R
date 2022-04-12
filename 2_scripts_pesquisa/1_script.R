##dados: 
#1:Shapefile das zonas de tensão ecológica(zte) do Brasil do IBGE
#2: Shapefile dos estados brasileiros ano 2010-IBGE
#arquivos estão na pasta 3_arquivos
#3: Cobertura Arborea do Hansen ano 2000- baixei os quadrantes para 
#os biomas da amazônia e cerrado.
#como os arquivos são  muito pesados deixarei só o link do download:
#https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.2.html

#4:Classificação dos biomas utilizando os dados do MAPBIOMAS coleção 3.0 ano 2000
#Fiz o download para os biomas amazônia-cerrado. Diferente do Hansen que são quadrantes, esse vem o bioma inteiro.
#link para download: https://mapbiomas.org/download

#bibliotecas utilizadas
library(raster)
library(rgeos)
library(maptools)
library(raster)
library(rgdal)
library(ncdf4)
library(dplyr)
library(sp)


# inicializando a função rasterCrop-cortar imagem raster
#função esta na pasta 3_arquivos
source('/home/gessica/Documents/scripts R/climatic indices/rasterCrop.R')


                 ######  SHAPEFILE   #####
#abrir shapefile  das ZTEs
setwd("/home/gessica/Documents/Dados/Limites/mapas ibge/")
veg <- readOGR(dsn = '.' , layer = 'Vegetation')

# Selecionar Areas de tensão ecologica
ate <- which(veg@data$TIPO2 == "Área de Tensão Ecológica", arr.ind = T)
#plot(veg[ate,])
aten <-veg[ate,] # mostrou todas as ZTEs para o Brasil
#plot(aten, add=T)

#como as ZTEs eram para o Brasil todo eu não sabia qual era qual eu plotei uma por uma e vi sua localização. Anotei o número(nome/id da area) das que estavam na área de estudo. Selecionei 46 áreas.
#plotei o shapefile dos estados também para saber a qual estado pertencia cada um,para já deixar anotado. 
#exemplo
plot(aten[125, ])


#ZTes selecionadas 
zte <- c(103, 116, 117, 119, 120, 89, 94, 99, 100, 32, 35, 40, 41, 42 ,  43, 44, 46, 49, 54, 55, 63, 67, 70, 82, 90, 91, 92, 93, 97, 185, 187, 188, 191, 197, 199, 145,  95, 107, 108, 109, 125)

# abrir os estados brasileiros
setwd("/home/gessica/Documents/Dados/Limites/estados_2010/")
est <- readOGR(dsn = '.' , layer = 'estados_2010')
plot(est, add=T)

# como selecionar o estado desejado
ma <- est[est$sigla =="MA",] #Ex: Maranhão
#plot(ma, add = T)


            ###### RASTER ########
#abrir imagens (quadrantes) raster baixadas do hansen
setwd("/home/gessica/Documents/Dados/imagens tif/")   
tree<- raster("Hansen_GFC-2018-v1.6_treecover2000_00N_050W.tif")
tree2<- raster("Hansen_GFC-2018-v1.6_treecover2000_10S_050W.tif")
tree<- raster("Hansen_GFC-2018-v1.6_treecover2000_10S_070W.tif")
tree2<- raster("Hansen_GFC-2018-v1.6_treecover2000_00N_070W.tif")
tree3<- raster("Hansen_GFC-2018-v1.6_treecover2000_10S_060W.tif")
tree4<- raster("Hansen_GFC-2018-v1.6_treecover2000_00N_060W.tif")

#unir os quadrantes para montar os biomas usando a função: mosaic 
tree_bi <- mosaic(tree, tree2, tree3,tree4, fun = mean)
plot(tree_bi) 


#### Cortar a cobertura vegetal de cada  ZTE utilizando o shapefile

#obs*: como estava começando fiz do jeito mais trabalhoso, um por um, como estava começando não sabia fazer loop. Mas para automatizar esse processo é só montar um loop.
#Algo a se levar em consideração aqui também é a capacidade do computador, realmente as imagens são pesadas então muitas vezes dava erro e tive que cortar primeiro por estado para diminuir o tamanho do arquivo.

#Mas se for fazer da forma mais rápida é cortar as ZTEs direto das imagens raster que foram unidas.

# conversor pro raster, cortando ZTE um por um ex: ZTE 125
cropped <- crop(tree_bi, aten[125,], snap = 'out') # extensao (output é um retangulo)
cropped2 <- setValues(cropped, NA) # cria um novo raster com NA na extensao do cropped
shpAux <- rasterize(aten[125,], cropped2) # 
tree_ma_crop <- mask(x = cropped, mask = shpAux)

# salvando a imagem em grd e tif
writeRaster(tree_ma_crop, filename = "zte125.grd", format ="raster")
writeRaster(tree_ma_crop, filename = "zte125.tif", format = "GTiff")

## salvei a cobertura vegetal das 46 ZTEs para poder extrair os dados.

######  MAPBIOMAS ######
#mesmo processo que foi feito com o raster do Hansen
#Será utilizado o raster do Mapbiomas para classificar os pontos dentro da zte como amazônia 3 (floresta) e cerrado 4 (savana)

#abrir imagens para os biomas em raster baixadas no mabbiomas
setwd("/home/gessica/Documents/Dados/imagens tif/mapbio31/")
cer <- raster("CERRADO31.tif")
amz <- raster("AMAZONIA31.tif")

#unir os biomas usando a função: mosaic 
cer_amz <- mosaic(cer, amz, fun = mean)
plot(cer_amz) 


# conversor pro raster, cortando ZTE um por um ex: ZTE 125
cropped <- crop(cer_amz, aten[125,], snap = 'out') # extensao (output é um retangulo)
cropped2 <- setValues(cropped, NA) # cria um novo raster com NA na extensao do cropped
shpAux <- rasterize(aten[125,], cropped2) # 
tree_cer_amz <- mask(x = cropped, mask = shpAux)

# salvando a imagem em grd e tif
writeRaster(tree_cer_amz, filename = "ztee125.grd", format="raster")
writeRaster(tree_cer_amz, filename = "ztee125.tif", format ="GTiff")

## salvei o raster do mapbiomas das 46 ZTEs para poder extrair os dados.
