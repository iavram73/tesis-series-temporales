
library(forecast)


# Voy a simular 1000 series ARMA(2,1), las guardo en un csv

sigma <- 1
t_final <- 600
n_rep <- 1000

modelo <- list(ar= c(0.7,0.2),ma = -0.3)

set.seed(123)
series_arma21 <- replicate(n_rep, 
                           arima.sim(n = t_final, model = modelo,sd = sigma))

# Traspongo los datos para que quede una observación en cada fila, 
# cada fila será una serie. 
#Y convierto a data frame (inicialmente "array" o "matrix")
series_arma21 <- data.frame(t(series_arma21))
View(series_arma21)
dim(series_arma21)

# Guardo cada df en la carpeta "data" del proyecto.
write.csv(series_arma21, file = "data/muestra_arma21_0.7_0.2_-0.3.csv", row.names = FALSE)



# Voy a simular 1000 series ARMA(2,2), las guardo en un csv

sigma <- 1
t_final <- 600
n_rep <- 1000

modelo <- list(ar= c(0.7,0.2),ma = c(0.8,0.4))

set.seed(123)
series_arma22 <- replicate(n_rep, 
                           arima.sim(n = t_final, model = modelo,sd = sigma))

# Traspongo los datos para que quede una observación en cada fila, cada fila 
# será una serie. Y convierto a data frame (inicialmente "array" o "matrix")
series_arma22 <- data.frame(t(series_arma22))
View(series_arma22)
dim(series_arma22)

# Guardo cada df en la carpeta "data" del proyecto.
write.csv(series_arma22, file = "data/muestra_arma22_0.7_0.2_0.8_0.4.csv", row.names = FALSE)
