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

# Leo los datos de las series generadas con AR(3)
# Dejo afuera los primeros 100 datos para asegurar 
# la estabilización de la serie 

series_ar3 <- read.csv("data/muestra_ar3_0.7_0.2_-0.3.csv")
series_ar3 <- series_ar3[,101:600] # Saco los primeros 100 datos
dim(series_ar3)

# Armo un conjunto de entrenamiento sacando los últimos 10  datos
# para luego hacer predicciones y comparar con los valores reales.
train <- series_ar3[,1:490]

# Creo un df vacio el cual voy a llenar con las predicciones
pred <- data.frame(matrix(ncol = 10, nrow = 1000))
nombres_columnas <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
colnames(pred) <- nombres_columnas # asigno nombre a las columnas del df pred


# Hago un ajuste de las series con AR(3) y hago las predicciones para 
# los 10 períodos siguientes

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

for(i in 1:N_rep){
  
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(3, 0, 0), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/df_ECM_pred_ar3.csv", row.names = FALSE)

#############################################################################
# Ahora hago un ajuste de las series con un modelo incorrecto AR(2) y hago las
# predicciones para los 10 períodos siguientes

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final])
  ajuste <- Arima(serie, order = c(2, 0, 0), method="ML")
  pronostico <- forecast(ajuste, h=10)
  pred[i,] <- pronostico$mean 
}

# Guardo las predicciones en csv
write.csv(pred, file = "data/df_ECM_pred_ar2.csv", row.names = FALSE)


#############################################################################
# Ahora hago un ajuste de las series con un modelo incorrecto AR(1) y hago las
# predicciones para los 10 períodos siguientes

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final])
  ajuste <- Arima(serie, order = c(1, 0, 0), method="ML")
  pronostico <- forecast(ajuste, h=10)
  pred[i,] <- pronostico$mean
 
}

# Guardo las predicciones en csv
write.csv(pred, file = "data/df_ECM_pred_ar1.csv", row.names = FALSE)

