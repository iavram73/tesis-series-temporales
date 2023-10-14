# Probando subir un pquete git

library(forecast)

# Voy a simular 1000 series AR(1), AR(2) y AR (3)

# Uso arima.sim

sigma <- 1
phi1 <- 0.7 
phi2 <- 0.2
phi3 <- -0.3
c <- 0
t_final <- 600
n_rep <- 1000
sd <- 1

set.seed(123) # Simulo AR(1)
series_ar1 <- replicate(n_rep, arima.sim(n = t_final, 
                        model = list(ar = phi1),sd = sd))
set.seed(123) # Simulo AR(2)
series_ar2 <- replicate(n_rep, arima.sim(n = t_final, 
                        model = list(ar = c(phi1, phi2)),sd = sd))
set.seed(123) # Simulo AR(3)
series_ar3 <- replicate(n_rep, arima.sim(n = t_final, 
                        model = list(ar = c(phi1, phi2, phi3)),sd = sd))

dim(series_ar1)

# Traspongo los datos para que quede una observación en cada fila, cada fila 
# será una serie. Y convierto a data frame (inicialmente "array" o "matrix")
series_ar1 <- data.frame(t(series_ar1))
series_ar2 <- data.frame(t(series_ar2))
series_ar3 <- data.frame(t(series_ar3))

# Guardo cada df en la carpeta "data" del proyecto como csv
write.csv(series_ar1, file = "data/muestra_ar1_0.7.csv", row.names = FALSE)
write.csv(series_ar2, file = "data/muestra_ar2_0.7_0.2.csv", row.names = FALSE)
write.csv(series_ar3, file = "data/muestra_ar3_0.7_0.2_-0.3.csv", row.names = FALSE)


