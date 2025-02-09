##Postereiormente com os dados que serão utilizados já organizados pode-se comoçar a análise e extração.
#O objetivo aqui é identificar se essas ZTEs são ecótonos, e para isso vamos avaliar a heterogeinidade de cada uma.
# Primeiro dado extraído foi a porcentagem de vegetação para cada bioma: amazônia-cerrado, que posteriomente será utilizado para criar um índice de heterogeinidade
#dados Hansen: cobertura vegetal (tree cover)
#dados Mapbiomas: classificação de biomas amazonia 3 (floresta) e cerrado 4 (savana)

#bibliotecas utilizadas
library(raster)
library(sp)

#abrir o raster da cobertura vegetal para a zte 125
setwd("/home/gessica/Documents/Dados/imagens tif/tree") #imegem tree cover
eco <- raster("zte125.tif")

#abrir o raster do mapbiomas para a zte 125
setwd("/home/gessica/Documents/Dados/imagens tif/mapbio3/") # imagem map biomas
e<- raster("ztee125.tif")


ecoPoints <-rasterToPoints(eco) # colocando as duas imagens em mesma projeção
coords <- SpatialPoints(ecoPoints[,1:2], 
                           proj4string= CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

aux <- extract(e, coords) #extraindo os pontos 
mtx <- data.frame(cbind(ecoPoints, aux)) #colocando no dataframe-tabela

#extraindo as porcentagens
mtxn <- mtx125[which(mtx[,4] == 3 | mtx[,4] == 4), ] # for + sav
mtxf <- mtx125[which(mtx[,4] == 3), ] # floresta
mtxs <- mtx125[which(mtx[,4] == 4), ] # savana

#porcentagens
dim(mtxs)[1]/dim(mtx)[1]
dim(mtxf)[1]/dim(mtx)[1]
dim(mtxn)[1]/dim(mtx)[1]

#novamente, fiz de um por um porque não sabia fazer um loop.
#extrai os dados e coloquei em uma tabela csv no excel que esta disponvel na pasta 3_arquivos

#Com esse dados, além de filtrar os locais em que apenas cobertura natural ocorre, as %F e %S foram utilizadas para avaliar caracterizar os ecótonos, e utilizados para investigar a proposta de um índice de heterogeneidade. Além disso, computou-se a proporção de pixels que correspondem a outras classes de cobertura não naturais (%A) em relação ao total de pixels, a fim de ilustrar as correntes mudanças de uso do solo nas ZTEs.

### saber o tipo de transição-tem que fazer isso para cada um
#ex zte 125
aten@data$TIPO[125]

##area 
aten@data$SHAPE_LENG[125] 
aten@data$SHAPE_AREA[125]