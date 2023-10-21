library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)

#########################################################
#pronostico <- forecast(modelo, h=6)# h: cantidad de periodos a pronosticar
#pronostico
#plot(pronostico)
# Otra forma de plotear
#modelo %>% forecast(h=6) %>% autoplot(include=100)
#pronostico$mean # da el resultado de las predicciones
#########################################################

# Leo los datos de las series generadas con MA(3)
# Dejo afuera los primeros 100 datos para asegurar la estabilización
# de la serie 

series_ma3 <- read.csv("data/muestra_ma3_0.7_0.2_-0.3.csv")
series_ma3 <- series_ma3[,101:600] # Saco los primeros 100 datos
dim(series_ma3)

# Armo un conjunto de entrenamiento sacando los últimos 10  datos
# para luego hacer predicciones y comparar con los valores reales.

train <- series_ma3[,1:490]

# Creo un df vacio el cual voy a llenar con las predicciones
pred <- data.frame(matrix(ncol = 10, nrow = 1000))
nombres_columnas <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
colnames(pred) <- nombres_columnas # asigno nombre a las columnas del df pred

###############################################################################
# Hago un ajuste de las series con MA(3) y hago las predicciones para 
# los 10 períodos siguientes

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(0, 0, 3), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/df_ECM_pred_ma3.csv", row.names = FALSE)

#############################################################################
# Ahora hago un ajuste de las series con un modelo incorrecto MA(2) y hago las
# predicciones para los 10 períodos siguientes

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final])
  ajuste <- Arima(serie, order = c(0, 0, 2), method="ML")
  pronostico <- forecast(ajuste, h=10)
  pred[i,] <- pronostico$mean 
}

# Guardo las predicciones en csv
write.csv(pred, file = "data/df_ECM_pred_ma2.csv", row.names = FALSE)


#############################################################################
# Ahora hago un ajuste de las series con un modelo incorrecto MA(1) y hago las
# predicciones para los 10 períodos siguientes

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final])
  ajuste <- Arima(serie, order = c(0, 0, 1), method="ML")
  pronostico <- forecast(ajuste, h=10)
  pred[i,] <- pronostico$mean
 
}

# Guardo las predicciones en csv
write.csv(pred, file = "data/df_ECM_pred_ma1.csv", row.names = FALSE)


################################################################################

