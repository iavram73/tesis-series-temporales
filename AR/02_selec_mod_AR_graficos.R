library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos guardados en el script 02_selec_mod_AR_datos: 

orden <- read.csv("data/df_aic_bic.csv", header = TRUE)
View(orden)

# Hago los gráficos
colores <- c("tomato", "orange","#26A63A" )

# Metrica AIC - 50 observaciones por serie
g1 <- orden %>% ggplot(aes(aic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, aic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Metrica AIC - 200 observaciones por serie
g2 <- orden %>% ggplot(aes(aic_200)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, aic_200==3), fill=alpha("orange",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "200 observaciones")+
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

# Metrica AIC - 500 observaciones por serie
g3 <- orden %>% ggplot(aes(aic_500)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, aic_500==3), fill=alpha("#26A63A",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "500 observaciones")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())  

# Metrica BIC - 50 observaciones por serie
g4 <- orden %>% ggplot(aes(bic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, bic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Metrica BIC - 200 observaciones por serie
g5 <- orden %>% ggplot(aes(bic_200)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, bic_200==3), fill=alpha("orange",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "200 observaciones")+
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

# Metrica BIC - 500 observaciones por serie
g6 <- orden %>% ggplot(aes(x=factor(bic_500))) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, bic_500==3), fill=alpha("#26A63A",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "500 observaciones")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 

selec_ar <- grid.arrange(g1,g2,g3,g4,g5,g6, nrow=2)
