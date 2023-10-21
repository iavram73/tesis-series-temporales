library(forecast)
library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia de tita 1 ajustado con MA(1)
# cuando los datos fueron generados por un MA(3).


series_ma3 <- read.csv("data/muestra_ma3_0.7_0.2_-0.3.csv", header = TRUE)
series_ma3 <- series_ma3[,101:600] # Saco los primeros 100 datos
dim(series_ma3)


N_rep <- 1000
t_final <- 500
titas1_ma1 <- c()

set.seed(123)
for(i in 1:N_rep){
  serie <- as.numeric(series_ma3[i, 1:t_final]) # Leo la serie
  ajuste <- Arima(serie, order = c(0, 0, 1), method="ML") # Hago el ajuste
  titas1_ma1[i] <- ajuste$coef[1] # Guardo parámetro tita1
}


# Explorar, cual es la convergencia del phi 1 y 2 ajustado con MA(2)
# cuando los datos fueron generados por un MA(3).

N_rep <- 1000
t_final <- 500
titas1_ma2 <- c()
titas2_ma2 <- c()

set.seed(123)
for(i in 1:N_rep){
  serie <-  as.numeric(series_ma3[i, 1:t_final])
  ajuste <- Arima(serie, order = c(0, 0, 2), method="ML")
  titas1_ma2[i] <- ajuste$coef[1]
  titas2_ma2[i] <- ajuste$coef[2]
  
}
titas_conv_ma <- data.frame("titas1_ma1"= titas1_ma1, "titas1_ma2"= titas1_ma2,
                           "titas2_ma2"=titas2_ma2)
#write.csv(titas_conv_ma, file = "data/titas_conv_ma.csv", row.names = TRUE)



