library(ggplot2)
library(tidyverse)
library(gridExtra)

# Quiero comparar el ECM de las predicciones cuando las series son ajustadas
# con el modelo con el cual fueron generadas (MA(3)) y con dos modelos 
# incorrectos (MA(1) y MA(2)).

# Leo los datos
series_ma3 <- read.csv("data/muestra_ma3_0.7_0.2_-0.3.csv")
series_ma3 <- series_ma3[,101:600] # Saco los primeros 100 datos de cada serie
# para asegurar estabilidad

# Hago un df con el conjunto de test
test <- data.frame(series_ma3[,491:500])  
dim(test) 

# Quiero calcular el ECM de la prediccion a t1. Para eso hago el cuadrado 
# de la diferencia entre el valor predicho y el observado a t1 
# para las 1000 series, y saco el promedio. Repito para las predicciones
# t2 a t10.


# Para el modelo correcto AR(3)
df_pred_ma3 <- read.csv("data/df_ECM_pred_ma3.csv",  header = TRUE)

ECM_ma3 <- c()
for (i in 1:10){
  ECM_ma3[i] <- mean((df_pred_ma3[,i] - test[,i])^2)
}


# Para el modelo incorrecto AR(2)
df_pred_ma2 <- read.csv("data/df_ECM_pred_ma2.csv",  header = TRUE)

ECM_ma2 <- c()
for (i in 1:10){
  ECM_ma2[i] <- mean((df_pred_ma2[,i] - test[,i])^2)
}


# Y lo mismo, pero ajustando con otro modelo incorrecto: AR(1)
df_pred_ma1 <- read.csv("data/df_ECM_pred_ma1.csv",  header = TRUE)

ECM_ma1 <- c()
for (i in 1:10){
  ECM_ma1[i] <- mean((df_pred_ma1[,i] - test[,i])^2)
}


# Junto en un df los ECM para t1 a t1 calculados para los tres modelos

tiempos <-  c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred_ma <- data.frame("filtro"= c(rep(1:10,3)),
                       "tiempos"=rep(tiempos,3), 
                       "ECM"= c(ECM_ma1, ECM_ma2,ECM_ma3),
                       "ID"= c(rep("MA(1)",10),rep("MA(2)",10), rep("MA(3)",10)))

# Guardo esta df en un csv
write.csv(ECM_pred_ma, file = "data/ECM_pred_ma.csv", row.names = FALSE)

ECM_pred_ma <- read.csv("data/ECM_pred_ma.csv",  header = TRUE)
# View(ECM_pred_ma)

colores <- c("tomato", "orange","#26A63A" )
tamanio_letras <- 14 
espaciado <- 0.5  # controlar el espaciado entre conjuntos
leyenda_2 <- c("MA(1)", "MA(2)","MA(3)")

ECM_pred_ma %>% ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 2)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero MA(3)") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras),  # Tamaño de los títulos de los ejes
    legend.text = element_text(size = tamanio_letras),  # Tamaño de las letras de la leyenda
    plot.margin = margin(0.1, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels=leyenda_2) +
  scale_fill_manual(values = colores)


