library(ggplot2)
library(tidyverse)
library(gridExtra)


selec_modelo_arima211_50 <- read.csv("data/selec_modelo_arima211_50.csv", header = TRUE)
selec_modelo_arima211_200 <- read.csv("data/selec_modelo_arima211_200.csv", header = TRUE)
selec_modelo_arima211_500 <- read.csv("data/selec_modelo_arima211_500.csv", header = TRUE)

unique(selec_modelo_arima211_50$ID_aic)
#puntos_c <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g7 <- selec_modelo_arima211_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_50, ID_aic=="(2,2)"), 
           fill=alpha("tomato", 0.5),size=0.5) +
  #geom_bar(data = puntos_c, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


# Para el g2 agrego los puntos adicionales que corresponden a los modelos 
# nunca elegidos y le asigno cero en en eje y:
# Crear un nuevo dataframe con el punto adicional (1,0)
unique(selec_modelo_arima211_200$ID_aic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))

g8 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_aic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima211_500$ID_aic)
g9 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_aic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima211_50$ID_bic)
#puntos_b <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g10 <- selec_modelo_arima211_50 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_50, ID_bic=="(2,2)"), fill=alpha("tomato", 0.5), size=0.5) +
  #geom_bar(data = puntos_b, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima211_200$ID_bic)
puntos_b <- data.frame(opciones = c("(0,0)","(0,1)","(0,2"), valor =rep(0,3))
g11 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_bic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


unique(selec_modelo_arima211_500$ID_bic)

g12 <- selec_modelo_arima211_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_500, ID_bic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


selec_arima_211 <- grid.arrange(g7,g8,g9,g10,g11,g12, nrow=2)



