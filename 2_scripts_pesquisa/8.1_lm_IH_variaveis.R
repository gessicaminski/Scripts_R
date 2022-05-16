
###### REGRESSÃO  LINEAR  IH X MAP/MSI/CV  #########

# serão utilizados o MAP,MSI,CV, IH E AREA que vai ser o tamanho das bolinhas
#1-Regressão linear geral IHxvariavel (1 grafico)
#2-Regressão linear de IHxvariavel por transição (3 graficos) 
# sera feito para cada variavel : MAP,MSI e CV

# regressões lineares gerais
rg1<-lm(ecott$ih~ecott$map)
rg2<-lm(ecott$ih~ecott$msi)
rg3<-lm(ecott$ih~ecott$cv)

summary(rg1) #IHx MAP
summary(rg2) #IHxMSI
summary(rg3) #IHxCV


library(ggplot2)
##abrir a tabela com os dados 
setwd("C:/Users/gessi/Documents/")
ecott <- read.csv("dados3.CSV", sep = ";", stringsAsFactors = FALSE)

#cores
cols <- c("S/Fe" = "red", "S/Fo" = "blue", "Fo/Fe" = "green")
 
 #########  REGRESSÃO  IHX MAP ######
 ##regressão linear do IH x MAP mais a figura com o gráfico
ggplot(data = table, aes(x = map, y = ih,  size = area))+
   geom_smooth(method="lm",colour = "black",size = 0.5, se = F) +
  geom_point(aes(color = composicao))+
  
  scale_colour_manual(values = cols)+
  
  scale_x_continuous("MAP(mm/ano)", breaks = seq(1000, 2400, 200),
                     expand = c(0, 200)) +
  scale_y_continuous("Índice de Heterogeneidade", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        axis.text=element_text(color="black",size=14),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "MAP (mm/ano)",
       y = "Indice de Heterogeneidade"
  )

#rregressão linear pra cada tipo de transição 

#transições: map x ih
#primeiro separei as composições e a area que irei usar em todos as regressões
# composições
C1<-  ecott$composicao[ecott$composicao == "S/Fe"]
C2 = ecott$composicao[ecott$composicao == "S/Fo"]
C3 = ecott$composicao[ecott$composicao == "Fo/Fe"]

# areas 
area1=ecott$area[ecott$composicao == "S/Fe"]
area2=ecott$area[ecott$composicao == "S/Fo"]
area3=ecott$area[ecott$composicao == "Fo/Fe"]

#por IH
y1<- ecott$ih[ecott$composicao == "S/Fe"]
y2=ecott$ih[ecott$composicao == "S/Fo"]
y3=ecott$ih[ecott$composicao == "Fo/Fe"]

##MAP a variavel que muda nos proximos graficos
x1<-  ecott$map[ecott$composicao == "S/Fe"]
x2 = ecott$map[ecott$composicao == "S/Fo"]
x3 = ecott$map[ecott$composicao == "Fo/Fe"]


#nova tabela com os dados selecionados
table1 <- data.frame(x1,y1,C1, area1)
table2 <- data.frame(x2,y2,C2, area2)
table3 <- data.frame(x3,y3,C3, area3)

#regressões lineares por tansição MAPxIH
rg4<-lm(ecott$y1~ecott$x1)
rg5<-lm(ecott$y2~ecott$x2)
rg6<-lm(ecott$y3~ecott$x3)

summary(rg4) #IHx MAP -s/fe
summary(rg5) #IHxMAP - s/fo
summary(rg6) #IHxMAP- fo/fe


#ih x map x s/fe
ggplot(data = table1, aes(x = x1, y=y1,  size = area1))+
     geom_smooth(data = table1, mapping = aes(x=x1, y=y1), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(color = "red")+
  
  
  scale_colour_manual(values = cols)+
  scale_x_continuous("MAP(mm/ano)", breaks = seq(1000, 2400, 400),
                     expand = c(0, 400)) +
  scale_y_continuous("IH", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        axis.text=element_text(color="black",size=16),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "IH",
       y = "") 

#ih x map x s/fo
ggplot(data = table2, aes(x = x2, y=y2,  size = area2))+
     geom_smooth(data = table2, mapping = aes(x=x2, y=y2), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(color = "blue")+
  
  
  scale_colour_manual(values = cols)+
  scale_x_continuous("MAP(mm/ano)", breaks = seq(1000, 2400, 400),
                     expand = c(0, 400)) +
  scale_y_continuous("IH", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
    
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        axis.text=element_text(color="black",size=16),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "IH",
       y = "") 


#ih x map fo/fe
ggplot(data = table3, aes(x = x3, y=y3,  size = area3))+
     geom_smooth(data = table3, mapping = aes(x=x3, y=y3), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(color = "green")+
  
  
  scale_colour_manual(values = cols)+
  scale_x_continuous("MAP(mm/ano)", breaks = seq(1000, 2400, 400),
                     expand = c(0, 400)) +
  scale_y_continuous("", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 1.0)) +
  
  theme(axis.title = element_text(size = 16),
        axis.text=element_text(color="black",size=16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "",
       y = "") 



 ############  REGRESSÃO  IH x MSI  ##########     
 ##MAP a variavel que muda nos proximos graficos

#regressão linear MSI xIH
ggplot(data = table, aes(x = msi, y = ih,  size = area))+
  geom_smooth(method="lm",colour = "black",size = 0.5, se = F) +
  geom_point(aes(color = composicao))+
  
  scale_colour_manual(values = cols)+
  scale_x_continuous("MSI", breaks = seq(0.45, 0.65, 0.05),
                     expand = c(0, 0.05)) +
  scale_y_continuous("Índice de Heterogeneidade", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        axis.text=element_text(color="black",size=16),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "MSI ",
       y = "Indice de Heterogeneidade")

#transições MSI x IH
x1<-  ecott$msi[ecott$composicao == "S/Fe"]
x2 = ecott$msi[ecott$composicao == "S/Fo"]
x3 = ecott$msi[ecott$composicao == "Fo/Fe"]


#nova tabela com os dados selecionados
table1 <- data.frame(x1,y1,C1, area1)
table2 <- data.frame(x2,y2,C2, area2)
table3 <- data.frame(x3,y3,C3, area3)

#regressões lineares por tansição MSIxIH
rg7<-lm(ecott$y1~ecott$x1)
rg8<-lm(ecott$y2~ecott$x2)
rg9<-lm(ecott$y3~ecott$x3)

summary(rg7) #IHx MSI -s/fe
summary(rg8) #IHxMSI - s/fo
summary(rg9) #IHxMSI- fo/fe

# s/fe
ggplot(data = table1, aes(x = x1, y=y1,  size = area1))+
  geom_smooth(data = table1, mapping = aes(x=x1, y=y1), method="lm", colour = "black",size = 0.5, se=FALSE)+
  #geom_point(color = "green")+
  geom_point(aes(color = C1))+
  facet_wrap(~C1) +
  xlim(0.45, 0.65)+ 
  
 
  
  scale_colour_manual(values = cols)+
  scale_y_continuous("IH", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(#axis.title = element_text(size = 16),
        axis.text=element_text(color="black",size=16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "MSI",
       y = "MSI")
  

# s/fo
ggplot(data = table2, aes(x = x2, y=y2,  size = area2))+
  geom_smooth(data = table2, mapping = aes(x=x2, y=y2), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(aes(color = C2))+
  #facet_wrap(~C2) +
  xlim(0.45, 0.65)+ 
  
  
  
  scale_colour_manual(values = cols)+

                     scale_y_continuous("", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(#axis.title = element_text(size = 16),
    axis.text=element_text(color="black",size=16),
    axis.title.x = element_text( size=24),
    axis.title.y = element_text(size=24),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_rect(fill = NA),
    legend.title = element_text(size = 10),
    panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "MSI",
       y = "")

#fo/fe
ggplot(data = table3, aes(x = x3, y=y3,  size = area3))+
  geom_smooth(data = table3, mapping = aes(x=x3, y=y3), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(aes(color = C3))+
  facet_wrap(~C3) +
  xlim(0.45, 0.65)+ 
  
  
  
  scale_colour_manual(values = cols)+
  scale_y_continuous("", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 1.0)) +
  
  theme(#axis.title = element_text(size = 16),
    axis.text=element_text(color="black",size=16),
    axis.title.x = element_text( size=24),
    axis.title.y = element_text(size=24),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_rect(fill = NA),
    legend.title = element_text(size = 10),
    panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "",
       y = "")



 ############  REGRESSÃO  IHxCV  ##########
##regressão linear do IH x CV mais a figura com o gráfico
ggplot(data = table, aes(x = cv, y = ih,  size = area))+
  geom_smooth(method="lm", colour = "black",size = 0.5, se = F) +
  geom_point(aes(color = composicao))+
  
  
  scale_colour_manual(values = cols)+
  scale_x_continuous("CV", breaks = seq(0.0, 0.3, 0.05),
                     expand = c(0, 0.05)) +
  scale_y_continuous("Indice de Heterogeneidade", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  
  theme(axis.title = element_text(size = 16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        axis.text=element_text(color="black",size=14),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "CV",
       y = "Indice de Heterogeneidade")


#rregressão linear pra cada tipo de transição 

#transições: cv x ih
# CV de cada tipo de transição
x1<-  ecott$cv[ecott$composicao == "S/Fe"]
x2 = ecott$cv[ecott$composicao == "S/Fo"]
x3 = ecott$cv[ecott$composicao == "Fo/Fe"]

#nova tabela com os dados selecionados
table1 <- data.frame(x1,y1,C1, area1)
table2 <- data.frame(x2,y2,C2, area2)
table3 <- data.frame(x3,y3,C3, area3)

#regressões lineares por tansição CVxIH
rg10<-lm(ecott$y1~ecott$x1)
rg11<-lm(ecott$y2~ecott$x2)
rg12<-lm(ecott$y3~ecott$x3)

summary(rg10) #IHx CV -s/fe
summary(rg11) #IHxCV - s/fo
summary(rg12) #IHxCV- fo/fe


###regressão linear de CVxIH para S/FE mais o grafico 
ggplot(data = table1, aes(x = x1, y=y1,  size = area1))+
  geom_smooth(data = table1, mapping = aes(x=x1, y=y1), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(aes(color = C1))+
  xlim(0.05,0.25)+
  
  scale_colour_manual(values = cols)+
  
  scale_y_continuous("IH", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
        axis.text=element_text(color="black",size=16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "CV",
       y = "")


####regressão linear de MAPxIH para S/FO 
ggplot(data = table2, aes(x = x2, y=y2,  size = area2))+
  geom_smooth(data = table2, mapping = aes(x=x2, y=y2), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(aes(color = C2))+
  xlim(0.05,0.25)+
  
  scale_colour_manual(values = cols)+
  
  scale_y_continuous("", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 0.20)) +
  
  theme(axis.title = element_text(size = 16),
        axis.text=element_text(color="black",size=16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "",
       y = "")



####regressão linear de CVxIH para S/FE mais o grafico 

ggplot(data = table3, aes(x = x3, y=y3,  size = area3))+
  geom_smooth(data = table3, mapping = aes(x=x3, y=y3), method="lm", colour = "black",size = 0.5, se=FALSE)+
  geom_point(aes(color = C3))+
  xlim(0.05,0.25)+
  
  scale_colour_manual(values = cols)+
  
  scale_y_continuous("", breaks = seq(0.0, 1.0, 0.20),
                     expand = c(0.0, 1.0)) +
  
  theme(axis.title = element_text(size = 16),
        axis.text=element_text(color="black",size=16),
        axis.title.x = element_text( size=24),
        axis.title.y = element_text(size=24),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(fill = NA),
        legend.title = element_text(size = 10),
        panel.background = element_blank())+
  
  scale_size(range = c(2, 10), guide = "none")+
  
  
  labs(color = "Composição",
       x = "",
       y = "")


##### SUMMARY #####
#GERAL
summary(rg1) #IHx MAP
summary(rg2) #IHxMSI
summary(rg3) #IHxCV

#TRANSIÇÕES
summary(rg4) #IHx MAP -s/fe
summary(rg5) #IHxMAP - s/fo
summary(rg6) #IHxMAP- fo/fe

summary(rg7) #IHx MSI -s/fe
summary(rg8) #IHxMSI - s/fo
summary(rg9) #IHxMSI- fo/fe

summary(rg10) #IHx CV -s/fe
summary(rg11) #IHxCV - s/fo
summary(rg12) #IHxCV- fo/fe