
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

ECM_arima211 

# Ahora para ARIMA (1,1,1)

pred_arima111 <- read.csv("data/df_ECM_pred_arima111.csv",  header = TRUE)
dim(pred_arima111)

ECM_arima111 <- c()
for (i in 1:10){
  ECM_arima111[i] <- mean((pred_arima111[,i] -test[,i])^2)
}

ECM_arima111 

# Ahora para ARIMA (2,0,1)

pred_arima201 <- read.csv("data/df_ECM_pred_arima201.csv",  header = TRUE)
dim(pred_arima201)
# Ponco na.rm=TRUE ya que hay NA en el df
ECM_arima201 <- c()
for (i in 1:10){
  ECM_arima201[i] <- mean((pred_arima201[,i] -test[,i])^2, na.rm = TRUE)
}

ECM_arima201 




# Ahora preparo el gráfico:

# Junto en un df los ECM para t1 a t10 calculados para los dos modelos

tiempos <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred_ARIMA <- data.frame("filtro"= c(rep(1:10,3)),
                            "tiempos"=c(rep(tiempos,3)), 
                            "ECM"= c(ECM_arima211, ECM_arima111,ECM_arima201),
                            "ID"= c(rep("ARIMA(2,1,1)",10),rep("AR!MA(1,1,1)",10),rep("ARIMA(2,0,1)",10)))

write.csv(ECM_pred_ARIMA, file = "data/ECM_pred_ARIMA.csv", row.names = FALSE)


ECM_pred_ARIMA <- read.csv("data/ECM_pred_ARIMA.csv",  header = TRUE)
# No saco la primera columna porque la voy a usar para filtrar en ggplot
#View(ECM_pred_ARMA)


# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_ARIMA$tiempos <- factor(ECM_pred_ARIMA$tiempos, levels = c(paste0("t", 1:9), "t10"))

g1 <- ECM_pred_ARIMA %>% 
  ggplot(aes(x = tiempos, y = ECM, color = ID)) + 
  geom_point(size = 3, shape = 21, fill = "lightgray", position = position_dodge(width = 0.7)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), position = position_dodge(width = 0.7), size = 0.5) +
  labs(x = "tiempos futuros", y = "ECM", color="") + 
  ggtitle("ECM de las predicciones a tiempos futuros",
          subtitle = "Ajustes con el modelo correcto ARIMA(2,1,1) y con dos modelos incorrecto ARIMA(1,1,1) y ARIMA(2,0,1") +
  theme_bw() + 
  scale_y_continuous(expand = expansion(0), limits = c(0,150)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10"))



g2 <- ECM_pred_ARIMA %>%
  filter(tiempos %in% c("t1", "t2", "t3")) %>%
  ggplot(aes(x = tiempos, y = ECM, color = ID)) +
  geom_point(size = 3, shape = 21, fill = "lightgray", position = position_dodge(width = 0.7)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), position = position_dodge(width = 0.7), size = 0.5) +
  labs(x = "tiempos futuros", y = "ECM", color="") +
  ggtitle("ECM de las predicciones a tiempos futuros",
          subtitle = "Ajustes con el modelo correcto ARIMA(2,1,1) y con dos modelos incorrecto ARIMA(1,1,1) y ARIMA(2,0,1") +
  theme_bw() +
  scale_y_continuous(expand = expansion(0), limits = c(0,15)) +
  scale_x_discrete(limits = c("t1", "t2", "t3"))




