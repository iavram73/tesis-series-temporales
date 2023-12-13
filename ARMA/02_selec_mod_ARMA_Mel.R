library(forecast)

# Leo ls datos de las series simuladas arma21
series_arma21 <- read.csv("data/muestra_arma21_0.7_0.2_-0.3.csv")
# saco los primeros 100 datos de cada serie.
series_arma21 <- series_arma21[,101:600]
dim(series_arma21)

fit <- Arima(as.numeric(series_arma21[10,]), order = c(1, 0, 1), method = "ML")$fitted

ECM <- sqrt((fit - as.numeric(series_arma21[10,]))^2) 


# Cargo los datos de seleccion de modelo para arma 21 y t final 500
selec_mod <- read.csv("data/selec_modelo_arma21_500.csv")

# Veo en qué fila estan las series elegidas con el modelo 11 y 21

fila_11 <- which(selec_mod$ID_bic=="(1,1)") # Hay 458 filas
# fila_21 <- which(selec_mod$ID_bic=="(2,1)") # Hay 73 filas

# Ajusto las 458 series con el modelo 11 y calculo el ECM
RMSE_11 <- c()
for(posicion in fila_11){
  serie <- as.numeric(series_arma21[posicion, ])
  ajuste_11 <- Arima(serie, order = c(1, 0, 1), method = "ML")
  ajustados <- ajuste_11$fitted
  RMSE <- sqrt(mean((ajustados - serie)^2))
  RMSE_11 <- c(RMSE_11, RMSE)
  print(posicion)
}

# Hago el promedio de RMSE para las 458 series
mean(RMSE_11) # 0.9983106

# Ajusto ahora las mismas 458 series con el modelo 21
# (el correcto) y calculo el ECM

RMSE_21 <- c()
for(posicion in fila_11){
  serie <- as.numeric(series_arma21[posicion, ])
  ajuste_21 <- Arima(serie, order = c(2, 0, 1), method = "ML")
  ajustados <- ajuste_21$fitted
  RMSE <- sqrt(mean((ajustados - serie)^2))
  RMSE_21 <- c(RMSE_21, RMSE)
  print(posicion)
}
mean(RMSE_21) # 0.995763

#################################################

# A pedido de MEL trato de calcular otros errores

rmse <- c()
mae <- c()
mpe <- c()
mape <- c()
for(posicion in fila_11){
  serie <- as.numeric(series_arma21[posicion, ])
  # cambia segun el ajuste arma(2,1) 0 (1,1). Cambiar en "order":
  ajuste <- Arima(serie, order = c(1, 0, 1), method = "ML") 
  ajustados <- ajuste$fitted
  acc <- accuracy(ajustados, serie)
  rmse <- c(rmse, acc[2])
  mae <- c(mae, acc[3])
  mpe <- c(mpe, acc[4])
  mape <- c(mape, acc[5])
  print(posicion)
}


#e_t = Yhat_t -Y_t
#p_t = 100*(e_t/Yt)
# Valores para ajuste con ARMA (2,1)
mean(rmse) # 0.995763 Root Mean Squared Error: sqrt(mean(e_t^2))
mean(mae) # 0.7950054 Mean Absolute Error: mean(abs(e_t))
mean(mpe) # 474.6332 Mean Percentage Error: mean(p_t)
mean(mape) # 893.1389 Mean Absolute Percentage Error: mean(abs(p_t))

# Valores para ajuste con ARMA (1,1) 
mean(rmse) # 0.9983106
mean(mae) # 0.7969823
mean(mpe) # 381.1852
mean(mape) # 795.7595




