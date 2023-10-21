library(forecast)
#library(tidyverse)
#library(ggthemes)
#library(readr)
#library(ggplot2)
#library(gridExtra)

#######################################################################
# Explorar cual es la convergencia de los parametros ajustados con 
# sarima_101_101_12 y por un sarima_111_001_12 cuando los datos fueron generados 
# por un sarima_111_101_12.


# Generacion de phis1
#Leo los datos
series_sarima <- read.csv("data/muestra_sarima_111_101_12.csv") # 1000 series
series_sarima <- series_sarima[,101:600] # saco los 100 primeros datos
dim(series_sarima)

# Parametros reales: # ar= c(0.7), ma=c(0.8), # sar= c(0.6), sma=c(0.7)
# d=1, D=0

# Genero parametros ajustados con sarima_101_101_12, es decir, ajusto 
# con un modelo que supone estacionariedad (d=0)

# Vuelvo a correr para otro incorrecto: sarima_111_001_12

t_final <- 500 
N_rep <- 1000
phis1 <- c()
titas1 <- c()
Phis1 <- c()
Titas1 <- c()
sigmas_2 <- c()

set.seed(123)
for (i in 1:N_rep) {
  tryCatch({
    serie <- as.numeric(series_sarima[i, 1:t_final])
    ajuste <- Arima(serie, order = c(1, 1, 1),
                    seasonal = list(order = c(0, 0, 1), period = 12),
                    method = "ML")
    phis1[i] <- ajuste$coef[1]
    titas1[i] <- ajuste$coef[2]
    Phis1[i] <- ajuste$coef[3]
    Titas1[i] <- ajuste$coef[4]
    sigmas_2[i] <- ajuste$sigma
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores 
    # predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
}


phis1_500 <- phis1
titas1_500 <- titas1
Phis1_500 <- Phis1
Titas1_500 <- Titas1
sigmas_2_500 <- sigmas_2


param_sarima <- data.frame("phis1_500"= phis1_500,
                           "titas1_500"=titas1_500,
                           "Phis1_500"= Phis1_500,
                           "Titas1_500"=Titas1_500,
                           "sigmas_2_500"=sigmas_2_500)

#write.csv(param_sarima, file = "data/dist_param_sarima_101_101_12_fuera_mod.csv", row.names = TRUE)

write.csv(param_sarima, file = "data/dist_param_sarima_111_001_12_fuera_mod.csv", row.names = TRUE)



#######################################################################
# Explorar cual es la convergencia de los parametros ajustados con 
# sarima_202_202_12 y por un sarima_010_202_12 cuando los datos fueron generados 
# por un sarima_212_202_12.

 
# Generacion de phis1
#Leo los datos
series_sarima <- read.csv("data/muestra_sarima_212_202_12.csv") # 1000 series
series_sarima <- series_sarima[,101:600] # saco los 100 primeros datos
dim(series_sarima)

# Parametros reales: 
# ar= c(0.7,0.2), ma=c(0.8,0.4), 
# sar= c(0.6,0.3), sma=c(0.7,0.5)
# d=1, D=0


# Genero parametros ajustados con sarima_202_202_12, es decir, ajusto con un modelo
# que supone estacionariedad (d=0)

t_final <- 500 
N_rep <- 1000
phis1 <- c()
phis2 <- c()
titas1 <- c()
titas2 <- c()
Phis1 <- c()
Phis2 <- c()
Titas1 <- c()
Titas2 <- c()
sigmas_2 <- c()
#ces <- c() # No devuelve la constante...

set.seed(123)
for (i in 1:N_rep) {
  tryCatch({
    serie <- as.numeric(series_sarima[i, 1:t_final])
    ajuste <- Arima(serie, order = c(2, 0, 2),
                    seasonal = list(order = c(2, 0, 2), period = 12),
                    method = "ML")
    phis1[i] <- ajuste$coef[1]
    phis2[i] <- ajuste$coef[2]
    titas1[i] <- ajuste$coef[3]
    titas2[i] <- ajuste$coef[4]
    Phis1[i] <- ajuste$coef[5]
    Phis2[i] <- ajuste$coef[6]
    Titas1[i] <- ajuste$coef[7]
    Titas2[i] <- ajuste$coef[8]
    sigmas_2[i] <- ajuste$sigma
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
}


phis1_500 <- phis1
phis2_500 <- phis2
titas1_500 <- titas1
titas2_500 <- titas2

Phis1_500 <- Phis1
Phis2_500 <- Phis2
Titas1_500 <- Titas1
Titas2_500 <- Titas2

sigmas_2_500 <- sigmas_2


param_sarima <- data.frame("phis1_500"= phis1_500,"phis2_500"= phis2_500,
                           "titas1_500"=titas1_500,"titas2_500"=titas2_500,
                           "Phis1_500"= Phis1_500,"Phis2_500"= Phis2_500,
                           "Titas1_500"=Titas1_500,"Titas2_500"=Titas2_500,
                           "sigmas_2_500"=sigmas_2_500)

write.csv(param_sarima, file = "data/dist_param_sarima_202_202_12_fuera_mod.csv", row.names = TRUE)

