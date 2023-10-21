library(forecast)


#########################################################
#pronostico <- forecast(modelo, h=6)# h: cantidad de periodos a pronosticar
#pronostico
#plot(pronostico)
# Otra forma de plotear
#modelo %>% forecast(h=6) %>% autoplot(include=100)
#pronostico$mean # da el resultado de las predicciones
#########################################################

# Leo los datos de las series generadas con sarima_111_101_12
# Dejo afuera los primeros 100 datos para asegurar la estabilización
# de la serie 

sarima_series <- read.csv("data/muestra_sarima_111_101_12.csv")
sarima_series <- sarima_series[,101:600] # Saco los primeros 100 datos
dim(sarima_series)

# Armo un conjunto de entrenamiento sacando los últimos 10  datos
# para luego hacer predicciones y comparar con los valores reales.

train <- sarima_series[,1:490]

# Creo un df vacio el cual voy a llenar con las predicciones
pred <- data.frame(matrix(ncol = 10, nrow = 1000))
nombres_columnas <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
colnames(pred) <- nombres_columnas # asigno nombre a las columnas del df pred

###############################################################################
# Hago un ajuste de las series con sarima_111_101_12  y hago las predicciones para 
# los 10 períodos siguientes

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  print(paste(i, "a "))
  ajuste <- try( Arima(serie, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 0, 1), period = 12),
                       method = "ML"), silent=TRUE)
  print(paste(i, "b "))
  if(!inherits(ajuste, "try-error")){
    pronostico <- try(forecast(ajuste, h=10), silent=TRUE)
    print(paste(i, "c "))
    if(!inherits(pronostico, "try-error") && length(pronostico)>0){
      pred[i,] <- pronostico$mean
      print(i)
    }
  }
}
  

# View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/ECM_pred_sarima_111_101_12.csv", row.names = FALSE)

################################################################################
# Hago lo mismo, pero ajustando con un modelo incorrecto: sarima_101_101_12

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  print(paste(i, "a "))
  ajuste <- try( Arima(serie, order = c(1, 0, 1),
                       seasonal = list(order = c(1, 0, 1), period = 12),
                       method = "ML"), silent=TRUE)
  print(paste(i, "b "))
  if(!inherits(ajuste, "try-error")){
    pronostico <- try(forecast(ajuste, h=10), silent=TRUE)
    print(paste(i, "c "))
    if(!inherits(pronostico, "try-error") && length(pronostico)>0){
      pred[i,] <- pronostico$mean
    }
  }
}


# View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/ECM_pred_sarima_101_101_12_fuera_mod.csv", row.names = FALSE)

################################################################################
# Ahora con otro incorrecto: sarima_111_001_12

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  print(paste(i, "a "))
  ajuste <- try(Arima(serie, order = c(1, 1, 1),
                       seasonal = list(order = c(0, 0, 1), period = 12),
                       method = "ML"), silent=TRUE)
  print(paste(i, "b "))
  if(!inherits(ajuste, "try-error")){
    pronostico <- try(forecast(ajuste, h=10), silent=TRUE)
    print(paste(i, "c "))
    if(!inherits(pronostico, "try-error") && length(pronostico)>0){
      pred[i,] <- pronostico$mean
    }
  }
}


# View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/ECM_pred_sarima_111_001_12_fuera_mod.csv", row.names = FALSE)

