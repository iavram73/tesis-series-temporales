library(forecast)


#########################################################
#pronostico <- forecast(modelo, h=6)# h: cantidad de periodos a pronosticar
#pronostico
#plot(pronostico)
# Otra forma de plotear
#modelo %>% forecast(h=6) %>% autoplot(include=100)
#pronostico$mean # da el resultado de las predicciones
#########################################################

# Leo los datos de las series generadas con ARIMA (2,1,1)
# Dejo afuera los primeros 100 datos para asegurar la estabilización
# de la serie 

series_arima211 <- read.csv("data/muestra_arima211_0.7_0.2_-0.3.csv")
series_arima211 <- series_arima211[,101:600] # Saco los primeros 100 datos
dim(series_arima211)

# Armo un conjunto de entrenamiento sacando los últimos 10  datos
# para luego hacer predicciones y comparar con los valores reales.

train <- series_arima211[,1:490]

# Creo un df vacio el cual voy a llenar con las predicciones
pred <- data.frame(matrix(ncol = 10, nrow = 1000))
nombres_columnas <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")
colnames(pred) <- nombres_columnas # asigno nombre a las columnas del df pred

###############################################################################
# Hago un ajuste de las series con ARIMA(2,1,1) y hago las predicciones para 
# los 10 períodos siguientes

N_rep <- 1000
t_final <- 490 # dejo afuera los ultimos 10 datos

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(2, 1, 1), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

# View(pred)
dim(pred)  # 1000   10

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/df_ECM_pred_arima211.csv", row.names = FALSE)

################################################################################
# Hago lo mismo, pero ajustando con un modelo incorrecto: ARIMA(1,1,1)

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(train[i,1:t_final]) # recorro cada serie
  ajuste <- Arima(serie, order = c(1, 1, 1), method="ML") # ajusto
  pronostico <- forecast(ajuste, h=10) # hago el pronóstico
  # lleno cada fila de pred con el pronostico de t1 a y10:
  pred[i,] <- pronostico$mean 
  
}

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/df_ECM_pred_arima111.csv", row.names = FALSE)


################################################################################
# Hago lo mismo, pero ajustando con un modelo incorrecto: ARIMA(2,0,1)

pred <- data.frame(matrix(ncol = 10, nrow = 1000))
colnames(pred) <- nombres_columnas


# Voy a hacer un cambio en el código para que siga con el ciclo
# en el caso de que encuentre alguna serie para la cual no puede calcular.
set.seed(123)
for(i in 1:N_rep){
  serie <- ts(as.numeric(train[i,1:t_final]))
  
  ajuste <- try(Arima(serie, order = c(2, 0, 1), method="ML"), silent=TRUE)
  
  if(!inherits(ajuste, "try-error")){
    pronostico <- try(forecast(ajuste, h=10), silent=TRUE)
    
    if(!inherits(pronostico, "try-error") && length(pronostico)>0){
      pred[i,] <- pronostico$mean
    }
  }
}

n_rows_with_na <- sum(apply(pred, 1, anyNA))
n_rows_with_na # hay 122 filas con NA.

# Guardo las predicciones en csv en la carpeta "data" del proyecto
write.csv(pred, file = "data/df_ECM_pred_arima201.csv", row.names = FALSE)

