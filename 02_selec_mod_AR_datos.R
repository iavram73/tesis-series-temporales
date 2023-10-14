library(forecast)
#library(ggplot2)
#library(tidyverse)
#library(gridExtra)

# Usamos las 1000 series generadas según AR(3).
# Ajustamos cada una de esas series con orden 1 hasta 7.
# verificamos qué orden fue seleccionado según AIC y BIC
# (el menor valor de AIC/BIC corresponde al orden elegido). 
# Guardamos el orden seleccionado.
# Luego graficamos la frecuencia con que se eligió cada orden.

# Recupero los datos de las series AR(3)
series_ar3 <- read.csv("data/muestra_ar3_0.7_0.2_-0.3.csv") # 1000 series

# saco los primeros 100 datos de cada serie de manera de asegurr la
# estabilidad del proceso
series_ar3 <- (series_ar3[,101:600])
dim(series_ar3)


N_rep <- 1000
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)
p <- 7 # orden máximo al cual vamos a probar
t_final <- 500 # Esto lo voy cambiando (50, 200 y 500 observaciones por serie)

set.seed(123)
for (i in 1:N_rep){
  ar_3 <- ts(as.numeric(series_ar3[i,1:t_final])) # Tomo una serie
  aic_s <- c() # Aca voy a guardar los aic
  bic_s <- c() # Aca voy a guardar los aic
  for(j in 1:p){
    # ajusto y guardo aic
    aic_s[j] <- Arima(ar_3, order = c(j, 0, 0), method="ML")$aic
    # ajusto y guardo bic
    bic_s[j] <- Arima(ar_3, order = c(j, 0, 0), method="ML")$bic
  }
  orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
  orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
}

# Lo corro con t final 50 y guardo los datos
orden_50_aic <- orden_elegido_aic
orden_50_bic <- orden_elegido_bic
# Lo corro con t final 200 y guardo los datos
orden_200_aic <- orden_elegido_aic 
orden_200_bic <- orden_elegido_bic
# Lo corro con t final 500 y guardo los datos
orden_500_aic <- orden_elegido_aic
orden_500_bic <- orden_elegido_bic

# Guardo los 1000 ordenes elegidos para cada serie, 
# con 50, 200 y 500 datos cada una en un df
df_aic_bic <- data.frame("aic_50"= orden_50_aic, "bic_50"= orden_50_bic,
                     "aic_200"= orden_200_aic, "bic_200"= orden_200_bic,
                     "aic_500"= orden_500_aic, "bic_500"= orden_500_bic)

write.csv(df_aic_bic, file = "data/df_aic_bic.csv", row.names = FALSE)

############################################################################
# Veo la fracción de veces que se eligio el orden 3 en cada caso:
# LOs gráficos están en el script de gráficos
# Con aic
mean(orden$aic_50==3) # 0.44
mean(orden$aic_200==3) # 0.735
mean(orden$aic_500==3) # 0.752
# Con bic
mean(orden$bic_50==3) # 0.303
mean(orden$bic_200==3) # 0.866
mean(orden$bic_500==3) # 0.981





