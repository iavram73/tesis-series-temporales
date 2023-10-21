library(ggplot2)
library(tidyverse)
library(gridExtra)

# SARIMA selec modelo, grafico solo para 50 observaciones por serie.

selec_sarima_50 <- read.csv("data/frec_selec_sarima_50.csv", header = TRUE)

#selec_sarima_50$Modelo <- as.factor(selec_sarima_50$Modelo)


# Crear un dataframe con los colores deseados para cada Modelo
colores <- data.frame(Modelo = c("A", "B", "C", "D"), 
                      color = c("orange", "gray", "gray", "gray"))

# Utilizar ggplot2 y escala de colores manual para asignar colores personalizados
g1 <- selec_sarima_50 %>%
  ggplot(aes(x = Modelo, y = Frec_AIC, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = alpha(colores$color, 0.5)) +
  labs(x = "", y = "Frecuencia") +
  ggtitle("", subtitle = "AIC") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        legend.position = "none")



g2 <- selec_sarima_50 %>%
  ggplot(aes(x = Modelo, y = Frec_BIC, fill = Modelo)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = alpha(colores$color,0.5))+
  labs(x="",y="") + ggtitle("", subtitle = "BIC") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,800)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank()) 


selec_sarima_50 <- grid.arrange(g1,g2,nrow=1, widths = c(0.4, 0.5))

