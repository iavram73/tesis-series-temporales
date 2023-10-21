
library(forecast)


# Voy a generar series ARIMA (2,1,1)
# Fijo los parámetros del modelo
phis <- c(0.7, 0.2)
theta <- -0.3
arima_order <- c(2, 1, 1)
# Crear una lista que contiene el orden y los coeficientes del modelo ARIMA
model <- list(order = arima_order, ar = phis, ma = theta)


# Simular UNA serie temporal con el modelo ARIMA especificado
set.seed(123)
simulated_data <- arima.sim(model = model, n = 1000)
# Visualizar la serie temporal simulada
plot(simulated_data, type = "l")


# Ahora simulo 1000 series

# Armo una matriz vacía
arima_series <- data.frame(matrix(nrow = 1000, ncol = 600))

# Voy llenando las filas con cada serie simulada
set.seed(123)
for(i in 1:1000){
  serie <- arima.sim(model = model, n = 1000)
  arima_series[i,] <- serie
}

plot(as.numeric(arima_series[1,]), type="l")
lines(as.numeric(arima_series[2,]), col="red")


n_rows_with_na <- sum(apply(arima_series, 1, anyNA))
n_rows_with_na # hay 0 filas con NA.

dim(arima_series)

# Guardo las series en un csv
write.csv(arima_series, file = "data/muestra_arima211_0.7_0.2_-0.3.csv", row.names = FALSE)


#######################################################################
# Voy a generar series ARIMA (2,1,2)
# Fijo los parámetros del modelo
phis <- c(0.7, 0.2)
titas <- c(0.8,0.4)
arima_order <- c(2, 1, 2)
# Crear una lista que contiene el orden y los coeficientes del modelo ARIMA
model <- list(order = arima_order, ar = phis, ma = titas)


# Ahora simulo 1000 series
# Armo una matriz vacía
arima_series <- data.frame(matrix(nrow = 1000, ncol = 600))

# Voy llenando las filas con cada serie simulada
set.seed(123)
for(i in 1:1000){
  serie <- arima.sim(model = model, n = 1000)
  arima_series[i,] <- serie
}

View(arima_series)

plot(as.numeric(arima_series[1,]), type="l")
lines(as.numeric(arima_series[2,]), col="red")

anyNA(arima_series) # FALSE
dim(arima_series)

# Guardo las series en un csv
write.csv(arima_series, file = "data/muestra_arima212_0.7_0.2_0.8_0.4.csv", row.names = FALSE)

