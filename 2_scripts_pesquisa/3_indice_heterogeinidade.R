####   ÍNDICE DE HETEROGEINIDADE ######

#Ainda, para obter um índice de heterogeneidade (IH), %F e %S foram combinadas da seguinte forma:
#IH=1-|%F-%S|/100  ,


##arquivo com as porcentagens de floresta/savana
setwd("C:/Users/gessi/Documents/")
ecott <- read.csv("dados2.CSV", sep = ";", stringsAsFactors = FALSE)

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