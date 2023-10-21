library(forecast)
#library(ggplot2)
#library(tidyverse)
#library(gridExtra)

# Ajustar un modelo SARIMA(2,1,2)x(1,0,1)_12 con la funcion Arima para cada serie 
# simulada con el mismo modelo.
# Guardar los parámetros en vectores.
# Hacr este proceso para series con 100 y 500 observaciones.
# Hacer histogarmas para ver las distribuciones de los parametros.


######################################################################
# Código para generar los parámetros de SARIMA(2,1,2)x(1,0,1)_12

#Leo los datos
series_sarima <- read.csv("data/muestra_sarima_212_202_12.csv") # 1000 series
series_sarima <- series_sarima[,101:600] # saco los 100 primeros datos
dim(series_sarima)

serie_prueba <- as.numeric(series_sarima[13,])
fit <- Arima(serie_prueba,order = c(2, 1, 2),
             seasonal = list(order = c(2, 0, 2), period = 12),  method="ML")


t_final <- 500 # esto es 100 o 500
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
    ajuste <- Arima(serie, order = c(2, 1, 2),
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



phis1_100 <- phis1
phis2_100 <- phis2
titas1_100 <- titas1
titas2_100 <- titas2

Phis1_100 <- Phis1
Phis2_100 <- Phis2
Titas1_100 <- Titas1
Titas2_100 <- Titas2

sigmas_2_100 <- sigmas_2

# Vuelvo a correr con t_final 500
phis1_500 <- phis1
phis2_500 <- phis2
titas1_500 <- titas1
titas2_500 <- titas2

Phis1_500 <- Phis1
Phis2_500 <- Phis2
Titas1_500 <- Titas1
Titas2_500 <- Titas2

sigmas_2_500 <- sigmas_2


param_sarima <- data.frame("phis1_100"= phis1_100,"phis2_100"= phis2_100,
                           "titas1_100"=titas1_100,"titas2_100"=titas2_100,
                           "Phis1_100"= Phis1_100,"Phis2_100"= Phis2_100,
                           "Titas1_100"=Titas1_100,"Titas2_100"=Titas2_100,
                           "phis1_500"= phis1_500,"phis2_500"= phis2_500,
                           "titas1_500"=titas1_500,"titas2_500"=titas2_500,
                           "Phis1_500"= Phis1_500,"Phis2_500"= Phis2_500,
                           "Titas1_500"=Titas1_500,"Titas2_500"=Titas2_500,
                           "sigmas_2_100"=sigmas_2_100,"sigmas_2_500"=sigmas_2_500)

write.csv(param_sarima, file = "data/dist_param_sarima_212_202_12.csv", row.names = TRUE)

######################################################################
# Código para generar los parámetros de SARIMA(1,1,1)x(1,0,1)_12

#Leo los datos
series_sarima <- read.csv("data/muestra_sarima_111_101_12.csv") # 1000 series
series_sarima <- series_sarima[,101:600] # saco los 100 primeros datos
dim(series_sarima)

serie_prueba <- as.numeric(series_sarima[13,])
fit <- Arima(serie_prueba,order = c(1, 1, 1),
             seasonal = list(order = c(1, 0, 1), period = 12),  method="ML")


t_final <- 500 # esto es 100 o 500
N_rep <- 1000
phis1 <- c()
titas1 <- c()
Phis1 <- c()
Titas1 <- c()
sigmas_2 <- c()
#ces <- c() # No devuelve la constante...

set.seed(123)
for (i in 1:N_rep) {
  tryCatch({
    print(i)
    serie <- as.numeric(series_sarima[i, 1:t_final])
    ajuste <- Arima(serie, order = c(1, 1, 1),
                    seasonal = list(order = c(1, 0, 1), period = 12),
                    method = "ML")
    phis1[i] <- ajuste$coef[1]
    titas1[i] <- ajuste$coef[2]
    Phis1[i] <- ajuste$coef[3]
    Titas1[i] <- ajuste$coef[4]
    sigmas_2[i] <- ajuste$sigma2
    
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
}



phis1_100 <- phis1
titas1_100 <- titas1
Phis1_100 <- Phis1
Titas1_100 <- Titas1
sigmas_2_100 <- sigmas_2

# Vuelvo a correr con t_final 500
phis1_500 <- phis1
titas1_500 <- titas1
Phis1_500 <- Phis1
Titas1_500 <- Titas1
sigmas_2_500 <- sigmas_2


param_sarima <- data.frame("phis1_100"= phis1_100, "titas1_100"=titas1_100,
                           "Phis1_100"= Phis1_100, "Titas1_100"=Titas1_100,
                           "phis1_500"= phis1_500, "titas1_500"=titas1_500,
                           "Phis1_500"= Phis1_500, "Titas1_500"=Titas1_500,
                           "sigmas_2_100"=sigmas_2_100,"sigmas_2_500"=sigmas_2_500)

write.csv(param_sarima, file = "data/dist_param_sarima_111_101_12.csv", row.names = TRUE)

