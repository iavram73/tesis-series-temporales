
library(forecast)
library(tidyverse)
library(stats)
library(tseries)
library(hydroGOF)
library(astsa)

### Leo los datos de la serie completa, desde 1975 a 2021
tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)

### Convierto los datos de la columna 4 a serie temporal
tunel <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
head(tunel)
tail(tunel)

### Usamos el suguiente cinjunto de datos
tunel_6 <- window(tunel, start = c(1977, 1),end = c(2016, 6)) 
head(tunel_6)
tail(tunel_6)

# Criterio de seleccion por AIC y BIC. Se usa todo el conjunto de datos.
# Se usa d=o ya que la serie es estacionaria.
p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)

combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
df = matrix(NA,64,3) # hay 64 combinaciones, y voy a usar 3 métricas
dim(df)

set.seed(123)
for (i in 1:64) { # En un minuto termina de correr
  print(i)
  p <- combinaciones[i, 1]
  q <- combinaciones[i, 2]
  P <- combinaciones[i, 3]
  Q <- combinaciones[i, 4]
  
  # Usamos tryCatch para capturar errores en la función Arima
  tryCatch({
    ajuste1 <- Arima(tunel_6, order = c(p, 0, q),
                     seasonal = list(order = c(P, 1, Q), period = 12),
                     include.drift = FALSE)
    
    b <- c(ajuste1$bic, ajuste1$aic, NSE(ajuste1$fitted, as.vector(tunel_6)))
    df[i, ] <- b
    }, error = function(e) {
    # En caso de error, mostramos el mensaje y continuamos con la siguiente iteración
    cat("Error en iteración", i, ":", conditionMessage(e), "\n")
  })
}

# El NSE es un índice hidrológico, cuanto más cercano a 1 mejor.
# No lo usé en la tesis.

colnames(df)<- c("BIC","AIC","NSE")
metricas_6m_aic_bic <- cbind(combinaciones,df) 

which.min(metricas_6m_aic_bic[,5]) # Modelo 36
which.min(metricas_6m_aic_bic[,6]) # Modelo 36
metricas_6m_aic_bic[36,]
# Var1 Var2 Var3 Var4     BIC      AIC       NSE
#   3    0    0    1    8436.077 8415.399 0.6967502
# Modelo elegido (3,0,0)x(0,1,1)_12

# write.csv(metricas_6m_aic_bic, file = "data/metricas_6m_aic_bic.csv", row.names = FALSE)


###############################################################################

# Ahora uso CV

p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)

combinaciones<-expand.grid(p,q,P,Q)
# hay 64 combinaciones, y voy a usar 1 métrica: RMSE de prediccion
dim(combinaciones)
df = matrix(NA,64,1) 
dim(df)

horizonte <- 6
win <- 120 # window en tsCV
set.seed(123)
for(i in 1:nrow(combinaciones)){
  p<-combinaciones[i,1]
  q<-combinaciones[i,2]
  P<-combinaciones[i,3]
  Q<-combinaciones[i,4]
  func_for <- function(x, h){forecast(Arima(x, order = c(p, 0, q),
                                seasonal = list(order = c(P, 1,Q), period = 12),
                                include.drift=FALSE), h=h)}
  e <- tsCV(tunel_6, func_for, h=horizonte, window = win)
  b <- sqrt(mean(e^2, na.rm=TRUE))
  df[i,] = b 
  print(i)
}

colnames(df)<- c("RMSE")
rmse_CV <- cbind(combinaciones,df)


# Horizonte de pronóstico de 6, window = 120 para tunel_m
which.min(rmse_CV[,5]) # Modelo 40
rmse_CV[40,]
# Var1 Var2 Var3 Var4     RMSE
#   3    1    0    1    3048.996
#  Modelo elegido (3,0,1)(0,1,1)[12] 
# tiempo de proceso: 1 hora 
write.csv(rmse_CV, file = "data/metricas_6m_CV.csv", row.names = FALSE)



##################################################################################
# Ahora criterio de seleccion con train_test

# Conjuntos de train/test a 6 meses
train_6 <- window(tunel_6, start = c(1977, 1),end = c(2015, 12)) 
test_6 <- window(tunel_6, start = c(2016, 1),end = c(2016, 6)) 

p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)
h <- 6 # horizonte de prediccion, para el forecast
serie_train <- train_6
serie_test <- test_6
combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
df = matrix(NA,64,1) # hay 64 combinaciones, y voy a usar 1 métrica (RMSE)
dim(df)

set.seed(123)
for (i in 1:nrow(combinaciones)) { # En un minuto termina de correr
  print(i)
  p <- combinaciones[i, 1]
  q <- combinaciones[i, 2]
  P <- combinaciones[i, 3]
  Q <- combinaciones[i, 4]
  
  # Usamos tryCatch para capturar errores en la función Arima
  tryCatch({
    ajuste <- Arima(serie_train, order = c(p, 0, q),
                     seasonal = list(order = c(P, 1, Q), period = 12),
                     include.drift = FALSE)
    pron <- forecast(ajuste, h=6) # funcion para el pronostico
    
    e_t <- c()
    for (j in 1:h){
      e_t[j] <- ((pron$mean[j] - serie_test[j])^2)
    }
    RMSE <- sqrt(mean(e_t))
    df[i, ] <- RMSE
    
  }, error = function(e) {
    # En caso de error, mostramos el mensaje y continuamos con la siguiente iteración
    cat("Error en iteración", i, ":", conditionMessage(e), "\n")
  })
}


colnames(df)<- c("RMSE_tt")
metricas_6m_TT <- cbind(combinaciones,df) 

write.csv(metricas_6m_TT, file = "data/metricas_6m_TT.csv", row.names = FALSE)


# A 6 meses, usando train_6/test_6
which.min(metricas_6m_TT[,5]) # Modelo 56 
metricas_6m_TT[56,]
# Var1 Var2 Var3 Var4     RMSE
#   3    1    1    1    3573.113
#  Modelo elegido (3,0,1)(1,1,1)[12] 


