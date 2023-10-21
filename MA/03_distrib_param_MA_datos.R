library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Usamos las series generadas según MA(3)
# Ajustar las series de 500 observaciones con la funcion Arima 
# y guardar los parámetros tita1, tita2,tita3, sigma^2 y c.
# Hacer histogarmas para ver las distribuciones de los parametros.
# Repetir lo anterior, pero con 100 observaciones en cada serie.

# Recuoero las series MA(3)
series_ma3 <- read.csv("data/muestra_ma3_0.7_0.2_-0.3.csv")
series_ma3 <- (series_ma3[,101:600])
dim(series_ma3)

N_rep <- 1000
sigma2 <- c()
titas1 <- c()
titas2 <- c()
titas3 <- c()
ces <- c()
t_final <- 500 # esto es 100 o 500


for(i in 1:N_rep){
  serie <- as.numeric(series_ma3[i, 1:t_final]) # Recorro cada serie
  fit <- Arima(serie, order = c(0, 0, 3), method="ML") # ajusto
  titas1[i] <- fit$coef[1]
  titas2[i] <- fit$coef[2]
  titas3[i] <- fit$coef[3]
  sigma2[i] <- fit$sigma
  ces[i] <- fit$coef[4]
}


#titas1_100 <- titas1
#titas2_100 <- titas2
#titas3_100 <- titas3
#sigma2_100 <- sigma2
#ces_100 <- ces
# Vuelvo a correr a t_final 500 
titas1_500 <- titas1
titas2_500 <- titas2
titas3_500 <- titas3
sigma2_500 <- sigma2
ces_500 <- ces

df <- data.frame("titas1_500"=titas1_500,"titas2_500"=titas2_500,"titas3_500"=titas3_500,
                 "sigma2_500"=sigma2_500,"ces_500"= ces_500,
                 "titas1_100"=titas1_100,"titas2_100"=titas2_100,"titas3_100", titas3_100,
                 "sigma2_100"=sigma2_100,"ces_100"= ces_100) 

# Guardo los parametros en un csv en la carpeta "data" del proyecto 
write.csv(df, file = "data/dist_param_ma3.csv", row.names = FALSE)









