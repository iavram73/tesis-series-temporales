library(forecast)
library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia del phi1 y tita1 ajustado con ARMA(1,1)
# cuando los datos fueron generados por un ARMA(2,1).

 
# Generacion de phis1
series_arma21 <- read.csv("data/muestra_arma21_0.7_0.2_-0.3.csv", header = TRUE)
series_arma21 <- series_arma21[,101:600] # Saco los primeros 100 datos
dim(series_arma21)


# Explorar, bajo el modelo normal, cual es la convergencia del phi 1 y tita 1
# ajustado con ARMA(1,1) cuando los datos fueron generados por un ARMA(2,1).

N_rep <- 1000
t_final <- 500
phis1 <- c()
titas1 <- c()

set.seed(123)
for(i in 1:N_rep){
  serie <-  as.numeric(series_arma21[i, 1:t_final])
  ajuste <- Arima(serie, order = c(1, 0, 1), method="ML")
  phis1[i] <- ajuste$coef[1]
  titas1[i] <- ajuste$coef[2]
}

# Guardo los datos en un df
dist_param_arma11 <- data.frame("phis1"= phis1, "titas1"= titas1)
# Guardo el df en un csv
# write.csv(dist_param_arma11, file = "data/dist_param_arma11_fuera_mod.csv", row.names = TRUE)



