###### REGRESSÃO  POLINOMIAL  IH X MAP/MSI/CV  #########

# serão utilizados o MAP,MSI,CV, IH E AREA que vai ser o tamanho das bolinhas

#1-Regressão polinomial de 2 e 3 potencia , geral IHxvariavel (3 graficos)
#2-Regressão polinomial para cada transição (9 graficos)
# sera feito para cada variavel : MAP,MSI e CV]
## primeiro eu testei as potencias 2 e 3 e aqui esta o codigo do grafico com a regressão que utilzei 
library(ggplot2)

## reressão polinomial geral
#preparação dos dados
y = with(ecott,ih)
x = with(ecott,map)
xx=with(ecott,msi) 
xxx=with(ecott,cv)

tamanho=dim(ecott)[1]
speed.novo=seq(min(x), max(x), length.out=tamanho)
speed.novo2=seq(min(xx), max(xx), length.out=tamanho)
speed.novo3=seq(min(xxx), max(xxx), length.out=tamanho)

### regreessão polinomial geral quadratica e cúbica de: 
#IH x MAP
#ajuste regressão quadratica
rgp2.1=lm(y~poly(x,2))
summary(rgp2.1)#IH x MAP

#ajuste regressão cubica
rgp3.1=lm(y~poly(x,3))
summary(rgp3.1)#IH x MAP

#IH x MSI
#ajuste regressão quadratica
rgp2.2=lm(y~poly(xx,2))
summary(rgp2.2) #IH x MSI

#ajuste regressão cubica
rgp3.2=lm(y~poly(xx,3))
summary(rgp3.2) #IH x MSI

#IH x CV
#ajuste regressão quadratica
rgp2.3=lm(y~poly(xxx,2))
summary(rgp2.3) #IH x CV

#ajuste regressão cubica
rgp3.3=lm(y~poly(xxx,3))
summary(rgp3.3) #IH x CV


# IH x CV
ggplot(data = table, aes(x = cvap, y = ih,  size = area))+
  geom_smooth(method="lm", colour = "black",size = 0.5, se = F, formula = y~poly(x,2)) +
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


#transições 

#transições cv x ih
x1<-  ecott$cvap[ecott$composicao == "S/Fe"]
x2 = ecott$cvap[ecott$composicao == "S/Fo"]
x3 = ecott$cvap[ecott$composicao == "Fo/Fe"]

C1<-  ecott$composicao[ecott$composicao == "S/Fe"]
C2 = ecott$composicao[ecott$composicao == "S/Fo"]
C3 = ecott$composicao[ecott$composicao == "Fo/Fe"]

table1 <- data.frame(x1,y1,C1, area1)
table2 <- data.frame(x2,y2,C2, area2)
table3 <- data.frame(x3,y3,C3, area3)

# s/fe

ggplot(data = table1, aes(x = x1, y=y1,  size = area1))+
  # geom_smooth(method="lm", se = F, formula = y~poly(x,3), show.legend = T) +
  #geom_smooth(formula = y ~ poly(x, 3), method = "lm")+
 # geom_smooth(method="lm", colour = "black",size = 0.5, se = F, formula = y~poly(x,2)) +
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




#ih x map x s/fo
ggplot(data = table2, aes(x = x2, y=y2,  size = area2))+
  # geom_smooth(method="lm", se = F, formula = y~poly(x,3), show.legend = T) +
  #geom_smooth(formula = y ~ poly(x, 3), method = "lm")+
 # geom_smooth(method="lm", colour = "black",size = 0.5, se = F, formula = y~poly(x,2)) +
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




#fofe

ggplot(data = table3, aes(x = x3, y=y3,  size = area3))+
  # geom_smooth(method="lm", se = F, formula = y~poly(x,3), show.legend = T) +
  #geom_smooth(formula = y ~ poly(x, 3), method = "lm")+
  # geom_smooth(method="lm", colour = "black",size = 0.5, se = F, formula = y~poly(x,2)) +
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




### SUMMARY ######
summary(rgp2.1)#rgp2 IH x MAP

summary(rgp3.1)#rgp3 IH x MAP

summary(rgp2.2) #rgp2 IH x MSI

summary(rgp3.2) #rgp3 IH x MSI

summary(rgp2.3) #rgp2 IH x CV

summary(rgp3.3) #rgp3 IH x CV