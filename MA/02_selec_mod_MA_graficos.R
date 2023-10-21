library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos giardados: 

orden <- read.csv("data/df_aic_bic_ma.csv", header = TRUE)
View(orden)


g1 <- orden %>% ggplot(aes(aic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, aic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# Crear un nuevo dataframe con el punto adicional (1,0)
punto_adicional <- data.frame(aic_200 = 1, y = 0)
# Debo hacer esto para el grafico que sigue porque nunca eligio orden 1, 
# y quiero incluir ese numero igual

g2 <- orden %>% ggplot(aes(aic_200)) +
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, aic_200==3), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = punto_adicional, fill = alpha("gray", 0.5), size = 0.5) +  # Agregar el punto adicional
  labs(x = "", y = "") +
  ggtitle("", subtitle = "200 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


g3 <- orden %>% ggplot(aes(aic_500)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, aic_500==3), fill=alpha("#26A63A",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "500 observaciones")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())  

g4 <- orden %>% ggplot(aes(bic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, bic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

punto_adicional <- data.frame(bic_200 = 1, y = 0)
g5 <- orden %>% ggplot(aes(bic_200)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, bic_200==3), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = punto_adicional, fill = alpha("gray", 0.5), size = 0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "200 observaciones")+
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


g6 <- orden %>% ggplot(aes(x=factor(bic_500))) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, bic_500==3), fill=alpha("#26A63A",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "500 observaciones")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

selec_ma <- grid.arrange(g1,g2,g3,g4,g5,g6, nrow=2)
