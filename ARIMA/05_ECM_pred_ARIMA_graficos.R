
library(ggplot2)
library(tidyverse)
library(gridExtra)

# Leo los datos
series_arima211 <- read.csv("data/muestra_arima211_0.7_0.2_-0.3.csv")
series_arima211 <- series_arima211[,101:600] # Saco los primeros 100 datos
dim(series_arima211)

# hago un df con el conjunto de test
test <- data.frame(series_arima211[,491:500]) 
dim(test) # 1000   10

# Quiero calcular el ECM de la prediccion a t1. Para eso hago el cuadrado 
# de la diferencia entre el valor predicho y el observado desde t1 a t10 
# para las 1000 series, y saco el promedio.

pred_arima211 <- read.csv("data/df_ECM_pred_arima211.csv",  header = TRUE)
dim(pred_arima211)

ECM_arima211 <- c()
for (i in 1:10){
  ECM_arima211[i] <- mean((pred_arima211[,i] -test[,i])^2)
}


# Ahora para ARIMA (1,1,1)
pred_arima111 <- read.csv("data/df_ECM_pred_arima111.csv",  header = TRUE)

ECM_arima111 <- c()
for (i in 1:10){
  ECM_arima111[i] <- mean((pred_arima111[,i] -test[,i])^2)
}


# Ahora para ARIMA (2,0,1)
pred_arima201 <- read.csv("data/df_ECM_pred_arima201.csv",  header = TRUE)
# Ponco na.rm=TRUE ya que hay NA en el df
ECM_arima201 <- c()
for (i in 1:10){
  ECM_arima201[i] <- mean((pred_arima201[,i] -test[,i])^2, na.rm = TRUE)
}


# Ahora preparo el gráfico:

# Junto en un df los ECM para t1 a t10 calculados para los dos modelos

tiempos <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred_ARIMA <- data.frame("filtro"= c(rep(1:10,3)),
                            "tiempos"=c(rep(tiempos,3)), 
                            "ECM"= c(ECM_arima211, ECM_arima111,ECM_arima201),
                            "ID"= c(rep("ARIMA(2,1,1)",10),rep("AR!MA(1,1,1)",10),rep("ARIMA(2,0,1)",10)))

#write.csv(ECM_pred_ARIMA, file = "data/ECM_pred_ARIMA.csv", row.names = FALSE)


ECM_pred_ARIMA <- read.csv("data/ECM_pred_ARIMA.csv",  header = TRUE)


# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_ARIMA$tiempos <- factor(ECM_pred_ARIMA$tiempos, levels = c(paste0("t", 1:9), "t10"))

tamanio_letras <- 14 
espaciado <- 0.5 
leyenda_4 <- c("ARIMA(1,1,1)", "ARIMA(2,0,1)","ARIMA(2,1,1)")
colores_2 <- c("orange", "tomato","#26A63A" )

ECM_pred_ARIMA %>% 
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 150)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero ARIMA(2,1,1)") +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),  # Cambia la posición de la leyenda
        legend.justification = c(0, 1),   # Alinea la leyenda a la izquierda y arriba
        legend.box.margin = margin(0, 0, 0, 0),  # Ajusta los márgenes de la leyenda
        panel.grid.major = element_blank(),
        axis.text = element_text(size = tamanio_letras),  
        axis.title = element_text(size = tamanio_letras), 
        legend.text = element_text(size = tamanio_letras),
        plot.margin = margin(0.1, 0.5, 0.5, 1, "cm"),
        plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores_2, labels=leyenda_4) +
  scale_fill_manual(values = colores_2)
