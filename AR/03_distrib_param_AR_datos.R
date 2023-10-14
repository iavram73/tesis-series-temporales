library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)

# Usamos las series generadas según AR(3)
# Ajustar las 1000 series con 500 observaciones cada una con la funcion Arima 
# Guardar los parámetros phi1, phi2,phi3, 
# Guardamos tambien sigma^2 y c ya que estamos.
# Luego: Hacemos histogarmas para ver las distribuciones de los parametros.
# Repetir lo anterior, pero con 100 observaciones en cada serie.


# Leo los datos de AR(3) ya generados
series_ar3 <- read.csv("data/muestra_ar3_0.7_0.2_-0.3.csv")
series_ar3 <- (series_ar3[,101:600])
dim(series_ar3)

sigma2 <- c() # defino los vectores vacios
phis1 <- c()
phis2 <- c()
phis3 <- c()
ces <- c()
n_rep <- 1000
t_final <- 500 # 100/500 observaciones por serie

set.seed(123)
for(i in 1:n_rep){
  serie <- as.numeric(series_ar3[i, 1:t_final]) # Recorro cada serie
  fit <- Arima(serie, order = c(3, 0, 0), include.mean = TRUE) # ajusto
  sigma2[i] <- fit$sigma2 # estimación de sigma^2
  phis1[i] <- fit$coef[1] # estimacion de phi1
  phis2[i] <- fit$coef[2] # estimacion de phi2
  phis3[i] <- fit$coef[3] # estimacion de phi3
  ces[i] <- fit$coef[4] # estimacion de c
}

#phis1_100 <- phis1
#phis2_100 <- phis2
#phis3_100 <- phis3
#sigma2_100 <- sigma2
#ces_100 <- ces

# Vuelvo a correr a t_final 500 
phis1_500 <- phis1
phis2_500 <- phis2
phis3_500 <- phis3
sigma2_500 <- sigma2
ces_500 <- ces

df <- data.frame("phis1_500"=phis1_500,"phis2_500"=phis2_500,"phis3_500"=phis3_500,
                 "sigma2_500"=sigma2_500,"ces_500"= ces_500,
                 "phis1_100"=phis1_100,"phis2_100"=phis2_100,"phis3_100"=phis3_100,
                 "sigma2_500"=sigma2_100,"ces_500"= ces_100) 

# Guardo los parametros en un csv en la carpeta "data" del proyecto 
write.csv(df, file = "data/dist_param_ar3.csv", row.names = FALSE)
