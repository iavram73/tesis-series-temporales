
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos guardados: 
arma_21_500 <- read.csv("data/selec_modelo_arma21_500.csv", header = TRUE)
arma_21_200 <- read.csv("data/selec_modelo_arma21_200.csv", header = TRUE)
arma_21_50 <- read.csv("data/selec_modelo_arma21_50.csv", header = TRUE)
colnames(arma_21_500)

g1 <- arma_21_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_50, ID_aic=="(2,1)"), fill = alpha("tomato", 0.5), size = 0.5) +
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

unique(arma_21_200$ID_aic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))

g2 <- arma_21_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_200, ID_aic=="(2,1)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

unique(arma_21_500$ID_aic)
puntos_b <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)"), valor =rep(0,3))
g3 <- arma_21_500 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_500, ID_aic=="(2,1)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


g4 <- arma_21_50 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_50, ID_bic=="(2,1)"),fill = alpha("tomato", 0.5), size=0.5) +
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


unique(arma_21_200$ID_bic)
puntos_b <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)"), valor =rep(0,3))
g5 <- arma_21_200 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_200, ID_bic=="(2,1)"),fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


unique(arma_21_500$ID_bic)
g6 <- arma_21_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_21_500, ID_bic=="(2,1)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "black", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


selec_arma_21 <- grid.arrange(g1,g2,g3,g4,g5,g6, nrow=2)


# Leo los datos guardados para arma22
arma_22_500 <- read.csv("data/selec_modelo_arma22_500.csv", header = TRUE)
arma_22_200 <- read.csv("data/selec_modelo_arma22_200.csv", header = TRUE)
arma_22_50 <- read.csv("data/selec_modelo_arma22_50.csv", header = TRUE)

unique(arma_22_50$ID_aic)
puntos_c <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g7 <- arma_22_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_50, ID_aic=="(2,2)"), fill = alpha("tomato", 0.5), size = 0.5) +
  geom_bar(data = puntos_c, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


# Para el g2 agrego los puntos adicionales que corresponden a los modelos 
# nunca elegidos y le asigno cero en en eje y:
# Crear un nuevo dataframe con el punto adicional (1,0)
unique(arma22_200$ID_aic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))

g8 <- arma_22_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_200, ID_aic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,500)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

unique(arma_22_500$ID_aic)
puntos_d <- data.frame(opciones = c("(0,0)","(0,1)","(1,0)" ,"(0,2)", "(1,1)"), valor =rep(0,5))
g9 <- arma_22_500 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_500, ID_aic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

unique(arma_22_50$ID_bic)
g10 <- arma_22_50 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_50, ID_bic=="(2,2)"), fill = alpha("tomato", 0.5), size=0.5) +
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

unique(arma_22_200$ID_bic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))
g11 <- arma_22_200 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_200, ID_bic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(arma_22_500$ID_bic)
g12 <- arma_22_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(arma_22_500, ID_bic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


selec_arma_22 <- grid.arrange(g7,g8,g9,g10,g11,g12, nrow=2)

