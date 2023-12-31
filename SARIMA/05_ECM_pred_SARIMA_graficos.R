library(ggplot2)
library(tidyverse)
library(gridExtra)

# Quiero comparar el ECM de las predicciones cuando las series son ajustadas
# con el modelo con el cual fueron generadas con modelos 
# incorrectos

# Leo los datos
sarima_series <- read.csv("data/muestra_sarima_111_101_12.csv")
sarima_series <- sarima_series[,101:600] # Saco los primeros 100 datos de cada serie
# para asegurar estabilidad

# Hago un df con el conjunto de test
test <- data.frame(sarima_series[,491:500])  
dim(test) 

# Quiero calcular el ECM de la prediccion a t1. Para eso hago el cuadrado 
# de la diferencia entre el valor predicho y el observado a t1 
# para las 1000 series, y saco el promedio. Repito para las predicciones
# t2 a t10.


# Para el modelo correcto pred_sarima_111_101_12 (modelo 1)
df1 <- read.csv("data/ECM_pred_sarima_111_101_12.csv",  header = TRUE)

dim(df1)

ECM_1<- c()
for (i in 1:10){
  ECM_1[i] <- mean((df1[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_1 

###############################################################################

# Para el modelo incorrecto sarima_101_101_12 (modelo 2)

df2 <- read.csv("data/ECM_pred_sarima_101_101_12_fuera_mod.csv",  header = TRUE)

ECM_2 <- c()
for (i in 1:10){
  ECM_2[i] <- mean((df2[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_2


###############################################################################

# Para el modelo incorrecto sarima_111_001_12

df3 <- read.csv("data/ECM_pred_sarima_111_001_12_fuera_mod.csv",  header = TRUE)

ECM_3 <- c()
for (i in 1:10){
  ECM_3[i] <- mean((df3[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_3

###########################################################################

# Junto en un df los ECM para t1 a t1 calculados para los tres modelos

tiempos <-  c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred <- data.frame("filtro"= c(rep(1:10,3)),
                       "tiempos"=c(rep(tiempos,3)), 
                       "ECM"= c(ECM_1, ECM_2,ECM_3),
                       "ID"= c(rep("M1-C",10),rep("M2-I",10), rep("M3-I",10)))


#write.csv(ECM_pred, file = "data/ECM_pred_sarima.csv", row.names = FALSE)


ECM_pred_sarima <- read.csv("data/ECM_pred_sarima.csv",  header = TRUE)

# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_sarima$tiempos <- factor(ECM_pred_sarima$tiempos, levels = c(paste0("t", 1:9), "t10"))

leyenda_5 <- c("SARIMA(1,1,1)(1,0,1)x12", 
               "SARIMA(1,0,1)(1,0,1)x12",
               "SARIMA(1,1,1)(0,0,1)x12")

colores_2 <- c("#26A63A", "orange","tomato")
tamanio_letras <- 14 
espaciado <- 0.5  # controlar el espaciado entre conjuntos


ECM_pred_sarima %>% 
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 300)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero SARIMA(1,1,1)(1,0,1)x12") +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.98),  # Cambia la posición de la leyenda
    legend.justification = c(0, 1),   # Alinea la leyenda a la izquierda y arriba
    legend.box.margin = margin(0, 0, 0, 0),  # Ajusta los márgenes de la leyenda
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  
    axis.title = element_text(size = tamanio_letras), 
    legend.text = element_text(size = tamanio_letras),
    plot.margin = margin(0.1, 0.5, 0.5, 0.5, "cm"),  # Ajusta los márgenes del gráfico
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores_2, labels=leyenda_5) +
  scale_fill_manual(values = colores_2)



