library(ggplot2)
library(tidyverse)
library(gridExtra)

# Quiero comparar el ECM de las predicciones cuando las series son ajustadas
# con el modelo con el cual fueron generadas (AR(3)) y con dos modelos 
# incorrectos (AR(1) y AR(2)).

# Leo los datos
series_ar3 <- read.csv("data/muestra_ar3_0.7_0.2_-0.3.csv")
series_ar3 <- series_ar3[,101:600] # Saco los primeros 100 datos de cada serie
# para asegurar estabilidad

# Hago un df con el conjunto de test
test <- data.frame(series_ar3[,491:500])  
dim(test) 

# Quiero calcular el ECM de la prediccion a t1. Para eso hago el cuadrado 
# de la diferencia entre el valor predicho y el observado a t1 
# para las 1000 series, y saco el promedio. Repito para las predicciones
# t2 a t10.


# Para el modelo correcto AR(3)
df_pred_ar3 <- read.csv("data/df_ECM_pred_ar3.csv",  header = TRUE)

ECM_ar3 <- c()
for (i in 1:10){
  ECM_ar3[i] <- mean((df_pred_ar3[,i] - test[,i])^2)
}

ECM_ar3 

#[1] 1.036184 1.684136 2.173203 2.166678
#[5] 2.082914 2.008885 2.179658 2.046137
#[9] 2.152358 2.148724
###############################################################################

# Para el modelo incorrecto AR(2)
df_pred_ar2 <- read.csv("data/df_ECM_pred_ar2.csv",  header = TRUE)

ECM_ar2 <- c()
for (i in 1:10){
  ECM_ar2[i] <- mean((df_pred_ar2[,i] - test[,i])^2)
}

ECM_ar2
#[1] 1.166533 1.864024 2.353576 2.302482
#[5] 2.202396 2.117182 2.266242 2.088224
#[9] 2.153932 2.146090


###############################################################################

# Y lo mismo, pero ajustando con otro modelo incorrecto: AR(1)

df_pred_ar1 <- read.csv("data/df_ECM_pred_ar1.csv",  header = TRUE)

ECM_ar1 <- c()
for (i in 1:10){
  ECM_ar1[i] <- mean((df_pred_ar1[,i] - test[,i])^2)
}

ECM_ar1
#[1] 1.160131 1.873197 2.363293 2.311342
#[5] 2.213582 2.122887 2.271768 2.091223
#[9] 2.153973 2.146516
###########################################################################

# Junto en un df los ECM para t1 a t10 calculados para los tres modelos

tiempos <-  c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred <- data.frame("filtro"= c(rep(1:10,3)),
                       "tiempos"=rep(tiempos,3), 
                       "ECM"= c(ECM_ar1, ECM_ar2, ECM_ar3),
                       "ID"= c(rep("AR(1)",10), rep("AR(2)",10), rep("AR(3)",10)))

# Guardo esta df en un csv
write.csv(ECM_pred, file = "data/ECM_pred.csv", row.names = FALSE)

# Leo los datos
ECM_pred <- read.csv("data/ECM_pred.csv",  header = TRUE)
View(ECM_pred)

colores <- c("tomato", "orange","#26A63A" )
tamanio_letras <- 14 
espaciado <- 0.5  # controlar el espaciado entre conjuntos
leyenda_1 <- c("AR(1)", "AR(2)","AR(3)")

ECM_pred %>%
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 3)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero AR(3)") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras),  # Tamaño de los títulos de los ejes
    legend.text = element_text(size = tamanio_letras),  # Tamaño de las letras de la leyenda
    plot.margin = margin(0.5, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels=leyenda_1) +
  scale_fill_manual(values = colores)
