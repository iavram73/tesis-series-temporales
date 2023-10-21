library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Usamos las 1000 series generadas según MA(3).
# Parametros: tita1=0.7, tita2=0.2, tita3= -0.3
# Ajustamos cada una de esas series con orden 1 hasta 7 y verificamos qué orden fue 
# seleccionado según AIC y BIC (el menor valor de AIC/BIC corresponde al orden elegido). 
# Guardamos el orden seleccionado y graficamos la frecuencia con que se eligió 
# cada orden.
# El procedimiento se realiza para t_final 50, 200 y 500.

# Recupero los datos de las series MA(3)
series_ma3 <- read.csv("data/muestra_ma3_0.7_0.2_-0.3.csv") # 1000 series

# saco los primeros 100 datos de cada serie y traspongo para que cada fila sea una observacion, una serie.
series_ma3 <- series_ma3[,101:600]
dim(series_ma3)


N_rep <- 1000
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)
q <- 7 # orden máximo a testear
t_final <- 50 # esto lo voy cambiando (50, 200 y 500)

set.seed(123)
for (i in 1:N_rep){
  ma_3 <- ts(as.numeric(series_ma3[i,1:t_final]))# Tomo una serie
  aic_s <- c() # Aca voy a guardar los aic
  bic_s <- c() # Aca voy a guardar los aic
  for(j in 1:q){
    aic_s[j] <- Arima(ma_3, order = c(0, 0, j), method="ML")$aic
    bic_s[j] <- Arima(ma_3, order = c(0, 0, j), method="ML")$bic
  }
  orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
  orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
}

orden_50_aic <- orden_elegido_aic
orden_50_bic <- orden_elegido_bic
# Lo corro de nuevo cambiando t final y guardo los nuevos datos
orden_200_aic <- orden_elegido_aic 
orden_200_bic <- orden_elegido_bic
# Lo corro de nuevo cambiando t final y guardo los nuevos datos
orden_500_aic <- orden_elegido_aic
orden_500_bic <- orden_elegido_bic

# Guardo los 1000 ordenes elegidos para cada serie, 
# con 50, 200 y 500 datos cada una

df_aic_bic <- data.frame("aic_50"= orden_50_aic, "bic_50"= orden_50_bic,
                         "aic_200"= orden_200_aic, "bic_200"= orden_200_bic,
                         "aic_500"= orden_500_aic, "bic_500"= orden_500_bic)

write.csv(df_aic_bic, file = "data/df_aic_bic_ma.csv", row.names = FALSE)

#######################################################################

