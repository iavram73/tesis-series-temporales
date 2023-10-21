library(forecast)
library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)


#######################################################################
# Explorar cual es la convergencia del phi1 y tita1 ajustado con ARIMA(1,1,1)
# cuando los datos fueron generados por un ARIMA(2,1,1).

 
# Generacion de phis1
#Leo los datos
series_arima211 <- read.csv("data/muestra_arima211_0.7_0.2_-0.3.csv") # 1000 series
series_arima211 <- series_arima211[,101:600] # saco los 100 primeros datos
dim(series_arima211)


# Explorar cual es la convergencia del phi 1 y tita 1
# ajustado con ARIMA(1,1,1) cuando los datos fueron generados por un ARMA(2,1,1).

N_rep <- 1000
t_final <- 500
phis1 <- c()
titas1 <- c()

set.seed(123)
for(i in 1:N_rep){
  serie <-  as.numeric(series_arima211[i, 1:t_final])
  ajuste <- Arima(serie, order = c(1, 1, 1), method="ML")
  phis1[i] <- ajuste$coef[1]
  titas1[i] <- ajuste$coef[2]
}

# Guardo los datos en un df
dist_param_arima111 <- data.frame("phis1"= phis1, "titas1"= titas1)
# Guardo el df en un csv
write.csv(dist_param_arima111, file = "data/dist_param_arima111.csv", row.names = TRUE)



