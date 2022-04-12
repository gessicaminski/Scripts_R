###########  FLEXMIX ############
#Avaliar heterogeinidade
#Teste estatístico: modelagem de mistura flexível (flexmix); 

#O flexmix é um modelo de mistura gaussiana, que modela heterogeneidade não observada ou para aproximar funções de distribuição geral e agrupamento de classes, ou para testar multimodalidade (GRÜN; LEISCH, 2012).  
#ref: GRÜN, B., LEISCH, F. Complement: Finite Mixture Model Diagnostics Using Resampling Methods. Semantic Scholar, 2012. Disponível em: <https://pdfs.semanticscholar.org/843f/a203e9bf3cf921456659df3470f7bf861d1e.pdf>. Acesso em: 01 de outubro de 2020.

#bibliotecas utilizadas
library(raster)
library(sp)
#teste flexmix 
library(flexmix)

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
#renomeandoo as colunas
colnames(mtx125n) <- c("lon", "lat", "tc", "mbio")


# teste flexmix

#utilizamos uma quantidade parcial de pontos- o total dividido por 100
mtx125n$tc_asin <- asin(sqrt(mtx125n$tc/100))

# sampling by 0.1% of the total number of points
nsample <- round(0.001*nrow(mtx125n)) 
if (nrow(mtx125n) > nsample) {  
  set.seed(90) ## esse aqui você coloca  para fixar um dado 
  mtx125ns <- mtx125n[sample(nrow(mtx125n), nsample),];
}

# effect of arcsin transformation
plot(mtx125ns$tc/100, mtx125ns$tc_asin)
   #main = "Histogram of Savanna and Florest (ZTE 125 - MA)",
  # xlab = "Tree Cover (arcsine transformation)")+
#ylab = "Waiting time to next eruption (min)")

mSA = stepFlexmix(tc_asin ~ 1, data = mtx125ns, k = 1:5, nrep = 5);
# list BIC criterions # aqui voce escolhe qual quer usar normalmente usa o BIC, eu testei os tres e foram parecidos
plot(mSA)
BIC(mSA)
AIC(mSA)
ICL(mSA)
which.min(ICL(mSA))
# get the best models
# BEST REGRESSION MODEL
m1=getModel(mSA, which="BIC")
#m1=getModel(mSA, which="AIC")
#m1=getModel(mSA, which="ICL")
parameters(m1)
m1
parameters(m2)
m2
parameters(m3)
m3


xx1 = seq(0, 1.5, by = 0.2);
hist(mtx125ns$tc_asin, 15, freq=FALSE, col="gray", xaxt="n", 
     xlim=c(0, asin(sqrt(1))), 
     main = "Histogram of Savanna and Florest (ZTE 125 - MA)",
     xlab = "Tree Cover (arcsine transformation)")
axis(1, at = asin(sqrt(xx1)))


xx1 = seq(0, 1.5, by = 0.2);
hist(mtx125ns$tc_asin, 15, freq=FALSE, col="gray", xaxt="n", 
     xlim=c(0, asin(sqrt(1))), 
     main = "Histogram of Savanna and Florest (ZTE 125 - MA)",
     xlab = "Tree Cover (arcsine transformation)")
axis(1, at = asin(sqrt(xx1)))


## o grafico 
x1 = seq(min(mtx125ns$tc_asin),max(mtx125ns$tc_asin), by = 0.01);
y1 = table(clusters(m1))[1]/nrow(mtx125ns)*dnorm(x1, mean = parameters(m1)[1,1], sd = parameters(m1)[2,1]);

y2=table(clusters(m1))[2]/nrow(mtx125ns)*dnorm(x1,mean=parameters(m1)[1,2],sd=parameters(m1)[2,2]);

y3=table(clusters(m1))[3]/nrow(mtx125ns)*dnorm(x1,mean=parameters(m1)[1,3],sd=parameters(m1)[2,3]);

y4=table(clusters(m1))[4]/nrow(mtx125ns)*dnorm(x1,mean=parameters(m1)[1,4],sd=parameters(m1)[2,4]);

y5=table(clusters(m1))[5]/nrow(mtx125ns)*dnorm(x1,mean=parameters(m1)[1,5],sd=parameters(m1)[2,5]);

lines(x1,y1,lty=3,xaxt="n") # each cluster distribution
lines(x1,y2,lty=3, xaxt="n");
lines(x1,y3,lty=3,xaxt="n");
lines(x1,y4,lty=3,xaxt="n");
lines(x1,y5,lty=3,xaxt="n");
lines(x1,y1+y2+y3+y4 +y5, xaxt="n");
lines(x1,y1+y2 + y3,xaxt="n") #total
lines(x1,y1+y2 ,xaxt="n") #total
lines(x1,y1,xaxt="n") #total


#refit for p values of parameters
rm1 = refit(m1)
summary(rm1)

#o final  utilizei as figuras e o sumary para os resultados
#fiz de um por um por que não sabia fazer o loop