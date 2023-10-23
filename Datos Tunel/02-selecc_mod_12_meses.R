
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

### Usamos el siguiente conjunto de datos
tunel_12 <- window(tunel, start = c(1977, 1),end = c(2015, 12)) 
head(tunel_12)
tail(tunel_12)

# Criterio de seleccion por AIC y BIC. Se usa todo el conjunto de datos.
# Se usa d=o ya que la serie es estacionaria.
p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)

combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
df = matrix(NA,64,3) # hay 36 combinaciones, y voy a usar 3 métricas
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
    ajuste1 <- Arima(tunel_12, order = c(p, 0, q),
                     seasonal = list(order = c(P, 1, Q), period = 12),
                     include.drift = FALSE)
    
    b <- c(ajuste1$bic, ajuste1$aic, NSE(ajuste1$fitted, as.vector(tunel_12)))
    df[i, ] <- b
    }, error = function(e) {
    # En caso de error, mostramos el mensaje y continuamos con la siguiente iteración
    cat("Error en iteración", i, ":", conditionMessage(e), "\n")
  })
}


colnames(df)<- c("BIC","AIC","NSE")
metricas_12m_aic_bic <- cbind(combinaciones,df) 

write.csv(metricas_12m_aic_bic, file = "data/metricas_12m_aic_bic.csv", row.names = FALSE)


which.min(metricas[,5]) # Modelo 36
which.min(metricas[,6]) # Modelo 36

metricas[36,]
# Var1 Var2 Var3 Var4     BIC      AIC       NSE
#   3    0    0    1    8317.66 8297.047 0.6993385

# Modelo elegido (3,0,0)x(0,1,1)_12


# Ahora uso CV

p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)

combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
# hay 64 combinaciones, y voy a usar 1 métrica: RMSE de prediccion
df = matrix(NA,64,1) 
dim(df)

horizonte <- 12
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
  e <- tsCV(tunel_12, func_for, h=horizonte, window = win)
  b <- sqrt(mean(e^2, na.rm=TRUE))
  df[i,] = b 
  print(i)
}

colnames(df)<- c("RMSE")
rmse_CV <- cbind(combinaciones,df)


# Horizonte de pronóstico de 12, window = 120 para tunel_12
which.min(rmse_CV[,5]) # Modelo 54
rmse_CV[54,]
# Var1 Var2 Var3 Var4     RMSE
#   1    1    1    1    3132.259
#  Modelo elegido (1,0,1)(1,1,1)[12] 
# tiempo de proceso: 1 hora

# write.csv(rmse_CV, file = "data/metricas_12m_CV.csv", row.names = FALSE)


##################################################################################
# Ahora criterio de seleccion con train_test

# Conjuntos de train/test a 12 meses
train_12 <- window(tunel_12, start = c(1977, 1),end = c(2014, 12)) 
test_12 <- window(tunel_12, start = c(2015, 1),end = c(2015, 12)) 

p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)
h <- 12 # horizonte de prediccion
serie_train <- train_12
serie_test <- test_12
combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
df = matrix(NA,64,1) # hay 64 combinaciones, y voy a usar 1 métrica
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
    pron <- forecast(ajuste, h=12) # funcion para el pronostico
    
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


colnames(df)<- c("RMSE (TT)")

metricas_12m_TT <- cbind(combinaciones,df) 

write.csv(metricas_12m_TT, file = "data/metricas_12m_TT.csv", row.names = FALSE)



# A 12 meses, usando train_12/test_12
which.min(metricas_12m_TT[,5]) # Modelo 53 a 12 meses
RMSE_tt[53,]
# Var1 Var2 Var3 Var4     RMSE
#   0    1    1    1    1829.67
#  Modelo elegido (0,0,1)(1,1,1)[12] 


#####################################################