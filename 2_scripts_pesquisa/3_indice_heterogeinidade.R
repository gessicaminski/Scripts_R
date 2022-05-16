####   ÍNDICE DE HETEROGEINIDADE ######

#Ainda, para obter um índice de heterogeneidade (IH), %F e %S foram combinadas da seguinte forma:
#IH=1-|%F-%S|/100  ,


##arquivo com as porcentagens de floresta/savana
setwd("C:/Users/gessi/Documents/")
ecott <- read.csv("dados3.CSV", sep = ";", stringsAsFactors = FALSE)

#IH
#primeiro fiz a porcetagem de savana e floresta pra que a soma das 
#duas de 100% por isso dividi ambas pelo total das duas : floresta/floresta+savana
#e savana/floresta+savana

ecott$flo <- ecott$floresta /ecott$natural #floresta
ecott$sav <- ecott$savana /ecott$natural #savana
ecott$het5 <- abs(ecott$flo - ecott$sav)# 100% #abs-tirar o sinas negativos

#depois fiz 1- o valor pra inverter os valores e floresta ficar como sendo 1 e savana 0
ecott$het6 <- 1- ecott$het5
ecott$het6 

#ou seja, quanto mais o IH é próximo de 0, menos heterogêneo; quanto mais próximo de 1, mais heterogêneo. 

#colunas da tabela 
#estado" - estado brasileiro em que a zte esta localizada   
#"ecotonos" - numero da zte que veio no shapefile, é o nome ou identificador  
#"savana" -porcentagem de savana   
#"floresta" -porcentagem de floresta 
 "natural"   
#"lucc" - porcentagem do que é não natural      
#"composicao" - qual  tipo de vegetação compõe a zte floresta estacional/savana (s/fe), floresta ombrofila e savana (s/fo) e floresta ombrofila  com floresta estacional (fo/fe) 
#"map":  média dos totais anuais de precipitação (MAP, sigla em inglês):  
# "cv" : variabilidade interanual: calculada a partir do coeficiente de variação (CV) dos acumulados anuais dos 37 anos, definido como a razão entre o desvio padrão dos valores e a média (MAP). 
#"msi" : variabilidade intra-anual (sazonalidade)
# "area" : area de cada zte
 #"sd" : desvio padrão       
#"iqr" : iiintervalo interquartil       
# "flo": porcentagem de floresta em relação ao total do que é   ntural    floresta/floresta+savana  
# "sav" :porcentagem de savana em relação ao total do que é   ntural    savana/floresta+savana      
#"het5" : são os valores de ih com savanas proximo de 1 e floresta 0     
#"ih: os valores foram invertidos fazedo 1-, para que as ztes com  mais savanas ficassem proximo de 0 e floresta 1. Foram os valores utilizados 