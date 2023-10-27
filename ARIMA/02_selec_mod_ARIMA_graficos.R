library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos guardados para ARIMA (2,1,1): 

selec_modelo_arima211_50 <- read.csv("data/selec_modelo_arima211_50_n.csv", header = TRUE)
selec_modelo_arima211_200 <- read.csv("data/selec_modelo_arima211_200_n.csv", header = TRUE)
selec_modelo_arima211_500 <- read.csv("data/selec_modelo_arima211_500_n.csv", header = TRUE)


unique(selec_modelo_arima211_50$ID_aic)
puntos_c <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g7 <- selec_modelo_arima211_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_50, ID_aic=="(2,1)"), fill="#9FCAE6", colour="#2A5783", size=0.5) +
  geom_bar(data = puntos_c, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 


# Para el g2 agrego los puntos adicionales que corresponden a los modelos 
# nunca elegidos y le asigno cero en en eje y:
# Crear un nuevo dataframe con el punto adicional (1,0)
unique(selec_modelo_arima211_200$ID_aic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))

g8 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_aic=="(2,1)"), fill="#B3E0A6", colour="#256D3D", size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 

unique(selec_modelo_arima211_500$ID_aic)
puntos_d <- data.frame(opciones = c("(0,0)","(0,1)","(1,0)" ,"(0,2)", "(1,1)"), valor =rep(0,5))
g9 <- selec_modelo_arima211_500 %>% ggplot(aes(ID_aic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_500, ID_aic=="(2,1)"), fill="#FE8F7F", colour="#BD1100", size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 

unique(selec_modelo_arima211_50$ID_bic)
puntos_b <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g10 <- selec_modelo_arima211_50 %>% ggplot(aes(ID_bic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_50, ID_bic=="(2,1)"), fill="#9FCAE6", colour="#2A5783", size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 

unique(selec_modelo_arima211_200$ID_bic)
g11 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_bic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_bic=="(2,1)"), fill="#B3E0A6", colour="#256D3D", size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 


unique(selec_modelo_arima211_500$ID_bic)
g12 <- selec_modelo_arima211_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(colour="black") +
  geom_bar(data=subset(selec_modelo_arima211_500, ID_bic=="(2,1)"), fill="#FE8F7F", colour="#BD1100", size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() 


grid.arrange(g7,g8,g9,g10,g11,g12, nrow=2)


