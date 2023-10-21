
library(forecast) 
library(astsa) # sarima.sim


# ALGUNAS PRUEBITAS INICIALES

## SARIMA(0,1,1)x(0,1,1)_12 - B&J's favorite
set.seed(123)
sarima.sim(d=1, ma=-.4, D=1, sma=-.6, S=12, n=120)
tsplot(sarima.sim(d=1, ma=-.4, D=1, sma=-.6, S=12, n=120))  

## Pruebo simular una serie SARIMA(1,1,1)x(2,1,0)_4
tsplot(sarima.sim(d=1, ar= 0.6, ma=-.4, D=1, sar= 0.8, sma=-.6, S=4, n=600))

# Grafico ACF y PACF de una serie
serie_1 <- sarima.sim(d=1, ar= 0.3, ma=-0.2, D=0, sar= 0.8, sma=-0.1, S=12, n=600)
acf(serie_1, lag.max = 100)
pacf(serie_1, lag.max = 100)
plot(serie_1)

# Quiero graficar varias trayectorias

plot(serie_1, ylim=c(-200,200))
subserie_1 <- window(serie_1, start=0, end=20)
plot(subserie_1)
acf(subserie_1, lag.max = 100)
pacf(subserie_1, lag.max = 100)

plot(serie_1, ylim=c(-200,200))
for(i in 1:20){
  serie <- sarima.sim(d=1, ar= 0.3, ma=-0.2, D=0, sar= 0.8, sma=-0.1, S=12, n=600)
  #subserie <- window(serie, start=0, end=20)
  lines(serie, col=i)
}

##############################################################################
  
# Simulo 1000 series SARIMA SARIMA(2,1,2)x(1,0,1)_12

# Armo una matriz vacía
sarima_series <- data.frame(matrix(nrow = 1000, ncol = 600))
dim(sarima_series)
# Voy llenando las filas con cada serie generada con sarima.sim
set.seed(123)
for(i in 1:1000){
  # Generar la serie anual
  serie <- sarima.sim(d=1, ar= c(0.7,0.2), ma=c(0.8,0.4), D=0, sar= 0.8, sma=0.6, S=12, n=600)
  # Convertir la serie en un vector
  serie_vector <- as.vector(serie)
  sarima_series[i,]<- serie_vector
}

dim(sarima_series)
View(sarima_series)

# Guardo las series en un csv
write.csv(sarima_series, file = "data/muestra_sarima_212_101_12.csv", row.names = FALSE)


##############################################################################

# Simulo 1000 series SARIMA SARIMA(2,1,2)x(2,0,2)_12

# Armo una matriz vacía
sarima_series <- data.frame(matrix(nrow = 1000, ncol = 600))
dim(sarima_series)
# Voy llenando las filas con cada serie generada con sarima.sim
set.seed(123)
for(i in 1:1000){
  # Generar la serie anual
  serie <- sarima.sim(d=1, ar= c(0.7,0.2), ma=c(0.8,0.4), 
                      D=0, sar= c(0.6,0.3), sma=c(0.7,0.5), S=12, n=600)
  # Convertir la serie en un vector
  serie_vector <- as.vector(serie)
  sarima_series[i,]<- serie_vector
}

dim(sarima_series)
View(sarima_series)

# Guardo las series en un csv
write.csv(sarima_series, file = "data/muestra_sarima_212_202_12.csv", row.names = FALSE)
