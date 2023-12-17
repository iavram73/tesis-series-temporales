library(ggplot2)
library(tidyverse)
library(gridExtra)


# seleccion de modelo AR

orden <- read.csv("data/df_aic_bic.csv", header = TRUE)
View(orden)
colores <- c("tomato", "orange","#26A63A" )

g1 <- orden %>% ggplot(aes(aic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, aic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

  
g2 <- orden %>% ggplot(aes(aic_200)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, aic_200==3), fill=alpha("orange",0.5), size=0.5) +
  labs(x="",y="") + ggtitle("", subtitle = "200 observaciones")+
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
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
  
# grid.arrange(g1,g2,g3, nrow=1)

g4 <- orden %>% ggplot(aes(bic_50)) + 
  geom_bar(fill = alpha("gray", 0.5)) +  # Ajusta la transparencia y el color
  geom_bar(data = subset(orden, bic_50 == 3), fill = alpha("tomato", 0.5), size = 0.5) +  # Ajusta la transparencia
  labs(x = "", y = "Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones") +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


g5 <- orden %>% ggplot(aes(bic_200)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(orden, bic_200==3), fill=alpha("orange",0.5), size=0.5) +
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

selec_ar <- grid.arrange(g1,g2,g3,g4,g5,g6, nrow=2)

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec-ar.png", selec_ar,dpi = 300, width = 8, height = 4, units = "in")

###################################################################
# seleccion de modelo MA

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

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec-ma.png", selec_ma,dpi = 300, width = 8, height = 4, units = "in")

######################################################################################
# seleccion de modelo ARMA
# Leo los datos guardados para arma21 
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

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec_arma_21.png", selec_arma_21,dpi = 300, width = 9, height = 5, units = "in")


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

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec_arma_22.png", selec_arma_22,dpi = 300, width = 9, height = 5, units = "in")


########################################################################
# ARIMA (212) selec modelo

selec_modelo_arima212_50 <- read.csv("data/selec_modelo_arima212_50.csv", header = TRUE)
selec_modelo_arima212_200 <- read.csv("data/selec_modelo_arima212_200.csv", header = TRUE)
selec_modelo_arima212_500 <- read.csv("data/selec_modelo_arima212_500.csv", header = TRUE)

unique(selec_modelo_arima212_50$ID_aic)
puntos_c <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g7 <- selec_modelo_arima212_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_50, ID_aic=="(2,2)"), fill=alpha("tomato", 0.5),size=0.5) +
  geom_bar(data = puntos_c, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica AIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


# Para el g2 agrego los puntos adicionales que corresponden a los modelos 
# nunca elegidos y le asigno cero en en eje y:
# Crear un nuevo dataframe con el punto adicional (1,0)
unique(selec_modelo_arima212_200$ID_aic)
puntos <- data.frame(opciones = c("(0,0)","(0,1)", "(0,2)", "(1,0)"), valor =rep(0,4))

g8 <- selec_modelo_arima212_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_200, ID_aic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima212_500$ID_aic)
puntos_d <- data.frame(opciones = c("(0,0)","(0,1)","(1,0)" ,"(0,2)", "(1,1)"), valor =rep(0,5))
g9 <- selec_modelo_arima212_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_200, ID_aic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima212_50$ID_bic)
puntos_b <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g10 <- selec_modelo_arima212_50 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_50, ID_bic=="(2,2)"), fill=alpha("tomato", 0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("Métrica BIC", subtitle = "50 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima212_200$ID_bic)
g11 <- selec_modelo_arima212_200 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_200, ID_bic=="(2,2)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


unique(selec_modelo_arima212_500$ID_bic)
g12 <- selec_modelo_arima212_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima212_500, ID_bic=="(2,2)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos_d, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


selec_arima_212 <- grid.arrange(g7,g8,g9,g10,g11,g12, nrow=2)


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec_arima_212.png", selec_arima_212,dpi = 300, width = 9, height = 5, units = "in")

##########################################################################
########################################################################
# ARIMA (211) selec modelo

selec_modelo_arima211_50 <- read.csv("data/selec_modelo_arima211_50.csv", header = TRUE)
selec_modelo_arima211_200 <- read.csv("data/selec_modelo_arima211_200.csv", header = TRUE)
selec_modelo_arima211_500 <- read.csv("data/selec_modelo_arima211_500.csv", header = TRUE)

unique(selec_modelo_arima211_50$ID_aic)
#puntos_c <- data.frame(opciones = c("(0,0)","(0,1)"), valor =rep(0,2))
g7 <- selec_modelo_arima211_50 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_50, ID_aic=="(2,1)"), 
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
  geom_bar(data=subset(selec_modelo_arima211_200, ID_aic=="(2,1)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())

unique(selec_modelo_arima211_500$ID_aic)
g9 <- selec_modelo_arima211_200 %>% ggplot(aes(ID_aic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_200, ID_aic=="(2,1)"), fill=alpha("#26A63A",0.5), size=0.5) +
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
  geom_bar(data=subset(selec_modelo_arima211_50, ID_bic=="(2,1)"), fill=alpha("tomato", 0.5), size=0.5) +
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
  geom_bar(data=subset(selec_modelo_arima211_200, ID_bic=="(2,1)"), fill=alpha("orange",0.5), size=0.5) +
  geom_bar(data = puntos_b, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "200 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


unique(selec_modelo_arima211_500$ID_bic)

g12 <- selec_modelo_arima211_500 %>% ggplot(aes(ID_bic)) + 
  geom_bar(fill = alpha("gray", 0.5)) +
  geom_bar(data=subset(selec_modelo_arima211_500, ID_bic=="(2,1)"), fill=alpha("#26A63A",0.5), size=0.5) +
  geom_bar(data = puntos, aes(opciones), colour = "gray", size = 0.5)+
  labs(x="",y="Frecuencia") + ggtitle("", subtitle = "500 observaciones por serie") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,600)) +  # Agregamos expand = c(0, 0)
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), # Saca lineas de cuadricula
        panel.grid.minor.x = element_blank())


selec_arima_211 <- grid.arrange(g7,g8,g9,g10,g11,g12, nrow=2)


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec_arima_211.png", selec_arima_211,dpi = 300, width = 9, height = 5, units = "in")


########################################################################
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

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/selec_sarima_50.png", selec_sarima_50,
       dpi = 300, width = 6, height = 3, units = "in")


