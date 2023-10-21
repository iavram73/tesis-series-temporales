
#library(tidyverse)
#library(dplyr)
#library(gridExtra)
#library(tsm)
library(forecast)


# Voy a simular 1000 series MA(1), MA(2) y MA(3) las guardo en un txt

sigma <- 1
tita1 <- 0.7 
tita2 <- 0.2
tita3 <- -0.3
# c <- 0
t_final <- 600
n_rep <- 1000

series_ma1 <- replicate(n_rep, 
                    arima.sim(n = t_final, 
                              model = list(ma = 0.7),sd = sigma))
series_ma2 <- replicate(n_rep,
                        arima.sim(n = t_final, 
                                  model = list(ma = c(0.7, 0.2)),sd = sigma))
series_ma3 <- replicate(n_rep,
                        arima.sim(n = t_final, 
                                  model = list(ma = c(0.7, 0.2, -0.3)),sd = sigma))


# Traspongo los datos para que quede una observación en cada fila, cada fila 
# será una serie. Y convierto a data frame (inicialmente "array" o "matrix")
series_ma1 <- data.frame(t(series_ma1))
series_ma2 <- data.frame(t(series_ma2))
series_ma3 <- data.frame(t(series_ma3))

# Guardo cada df en la carpeta "data" del proyecto.
write.csv(series_ma1, file = "data/muestra_ma1_0.7.csv", row.names = FALSE)
write.csv(series_ma2, file = "data/muestra_ma2_0.7_0.2.csv", row.names = FALSE)
write.csv(series_ma3, file = "data/muestra_ma3_0.7_0.2_-0.3.csv", row.names = FALSE)


