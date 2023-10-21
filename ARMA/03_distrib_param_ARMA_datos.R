library(forecast)
#library(ggplot2)
#library(tidyverse)
#library(gridExtra)

# Simular 100 observaciones un proceso ARMA(2,1) y repetirlo 1000 veces.
# Ajustar un modelo ARMA(2,1) con la funcion Arima para cada siumacion
# Guardar los parámetros phi1, phi2, tita1, sigma^2 y c en vectores.
# Hacer histogarmas para ver las distribuciones de los parametros.
# Repetir lo anterior, pero con 500 observaciones en cada serie.


######################################################################
# Código para generar los parámetros de ARMA(2,1)

#Leo los datos
series_arma21 <- read.csv("data/muestra_arma21_0.7_0.2_-0.3.csv") # 1000 series
series_arma21 <- series_arma21[,101:600] # saco los 100 primeros datos
dim(series_arma21)


t_final <- 500 # esto es 100 o 500
N_rep <- 1000
sigmas_2 <- c()
phis1 <- c()
phis2 <- c()
titas1 <- c()
ces <- c()

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(series_arma21[i,1:t_final])
  ajuste_ml <- Arima(serie, order = c(2, 0, 1), method="ML")
  phis1[i] <- ajuste_ml$coef[1]
  phis2[i] <- ajuste_ml$coef[2]
  titas1[i] <- ajuste_ml$coef[3]
  ces[i] <- ajuste_ml$coef[4]
  sigmas_2[i] <- ajuste_ml$sigma
}


phis1_100 <- phis1
phis2_100 <- phis2
titas1_100 <- titas1
sigmas_2_100 <- sigmas_2
ces_100 <- ces

# Vuelvo a corre con t_final 500
phis1_500 <- phis1
phis2_500 <- phis2
titas1_500 <- titas1
sigmas_2_500 <- sigmas_2
ces_500 <- ces


param_arma21 <- data.frame("phis1_100"= phis1_100,"phis2_100"= phis2_100,
                              "titas1_100"=titas1_100, "ces_100" = ces_100, "sigmas_2_100"=sigmas_2_100,
                              "phis1_500"= phis1_500,"phis2_500"= phis2_500,
                              "titas1_500"=titas1_500, "ces_500" = ces_500, "sigmas_2_500"=sigmas_2_500)

write.csv(param_arma21, file = "data/dist_param_arma21.csv", row.names = TRUE)

