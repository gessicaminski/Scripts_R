### Correlacionar  o regime de chuva com a heterogeneidade das ZTEs ####

#Parar testar se as heterogeneidades das ZTEs são correlacionadas a um regime de chuva mais variável (e.g., menor total acumulado anual e maiores variabilidades), utilizaram-se os modelos de regressão linear e polinomial. 
#As variáveis dependentes testadas foram as métricas de heterogeneidade: IH, SD e IQR. Já as variáveis explicativas utilizadas foram MAP, MSI e CV. 

#Considerando a hipótese de correlação entre a heterogeneidade e MAP, utilizou-se a regressão polinomial com opção para verificar se o modelo não linear fornece resultados melhores com respeito ao ajuste dos dados. Teoricamente, podemos aproximar qualquer função com um polinômio. A regressão polinomial é uma extensão da regressão linear, em que adicionam-se termos quadráticos, cúbicos, etc, até o décimo grau. A variável independente x é expandida num polinômio com os termos x2 = x.x, x3 = x.x.x, etc (BATTISTI, 2001).
#ref: BATTISTI, I. D. E. Comparação entre modelos de regressão com uma aplicação embiometria florestal. 2001. Dissertação (Mestrado em Agronomia) - Universidade Federal de Lavras. Lavras, p.96. 2001.


#Depois de testar todas as combinações  criei uma tabela com  todos os valores resultante das regressoes, deixaarei na pasta 3_arquivos.   