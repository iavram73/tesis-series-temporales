library(forecast)


# Leo los datos de las series generadas con ARMA (2,1)
# Dejo afuera los primeros 100 datos para asegurar la estabilización
# de la serie 

series_arma21 <- read.csv("data/muestra_arma21_0.7_0.2_-0.3.csv")
series_arma21 <- series_arma21[,101:600] # Saco los primeros 100 datos
dim(series_arma21)

# Armo un conjunto de entrenamiento sacando los últimos 10  datos
# para luego hacer predicciones y comparar con los valores reales.

train <- series_arma21[,1:490]
test <- data.frame(series_arma21[,491:500]) # dim(test) 1000   10
dim(test)

# Creo un df vacio el cual voy a llenar con las predicciones
pred <- data.frame(matrix(ncol = 10, nrow = 1000))
nombres_columnas <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
colnames(pred) <- nombres_columnas # asigno nombre a las columnas del df pred

###############################################################################
# Hago un ajuste de las series con ARMA(2,1) y hago las predicciones para 
# los 10 períodos siguientes

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  
  serie <- as.numeric(train[i,1:t_final])
  ajuste <- Arima(serie, order = c(2, 0, 1), method="ML")
  pronostico <- forecast(ajuste, h=10)
  
  for(j in 1:10){
    pred[i,j] <- pronostico$mean[j]
  }
}

dim(pred) # 1000 10

ECM_arma21 <- c()
for (i in 1:10){
  ECM_arma21[i] <- mean((pred[,i] -test[,i])^2)
}

ECM_arma21 # 1.059291 1.229574 1.405484 1.608136 1.781656 1.959185
           # 2.023579 2.197352 2.393870 2.313184

################################################################################
# Hago lo mismo, pero ajustando con un modelo incorrecto: ARMA(1,1)

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(1, 0, 1), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

ECM_arma11 <- c()
for (i in 1:10){
  ECM_arma11[i] <- mean((pred[,i] -test[,i])^2)
}

ECM_arma11

################################################################################
# Hago lo mismo, pero ajustando con un modelo incorrecto: ARMA(0,1)

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(0, 0, 1), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

ECM_arma01 <- c()
for (i in 1:10){
  ECM_arma01[i] <- mean((pred[,i] -test[,i])^2)
}

ECM_arma01


# Junto en un df los ECM para t1 a t10 calculados para los dos modelos

tiempos <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred_arma <-  data.frame("filtro"= c(rep(1:10,3)),
                             "tiempos"=c(rep(tiempos,3)), 
                             "ECM"= c(ECM_arma21, ECM_arma11,ECM_arma01),
                             "ID"= c(rep("ARMA(2,1)",10),rep("ARMA(1,1)",10),rep("ARMA(0,1)",10)))
#write.csv(ECM_pred_arma, file = "data/ECM_pred_arma.csv", row.names = FALSE)







