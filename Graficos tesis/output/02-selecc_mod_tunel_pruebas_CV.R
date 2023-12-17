
library(forecast)
library(tidyverse)
library(stats)
library(tseries)
library(astsa)


tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)
tail(tunel_full)

tunel_ts <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
tunel_corto_ts <- ts(tunel_full[,4], start = c(2000,1), end = c(2021,12), frequency = 12)
tunel_6 <- ts(tunel_full[,4], start = c(2000,1), end = c(2021,6), frequency = 12)
tunel_desde_7_2021 <- ts(tunel_full[,4], start = c(2021,7), end = c(2021,12), frequency = 12)


# Se usa d=o ya que la serie es estacionaria
p<-c(0,1,2,3)
q<-c(0,1,2,3)
P<-c(0,1)
Q<-c(0,1)

combinaciones<-expand.grid(p,q,P,Q)
dim(combinaciones)
df = matrix(NA,64,1) # hay 64 combinaciones, y voy a usar 1 métrica: RMSE
dim(df)


horizonte <- 1
# win <- 30 # wndow en tsCV
set.seed(123)
for(i in 1:100){
  p<-combinaciones[i,1]
  q<-combinaciones[i,2]
  P<-combinaciones[i,3]
  Q<-combinaciones[i,4]
  func_for <- function(x, h){forecast(Arima(x, order = c(p, 0, q),
                              seasonal = list(order = c(P, 1,Q), period = 12),
                                            include.drift=FALSE), h=h)}
  e <- tsCV(tunel_corto_ts, func_for, h=horizonte)
  b <- sqrt(mean(e^2, na.rm=TRUE))
  print(i)
  df[i,] = b 
}

colnames(df)<- c("RMSE")
rmse_CV <- cbind(combinaciones,df)

which.min(rmse_CV[,5])
rmse_CV[55,] # 2    1    1    1 2543.43

# Modelo elegido el de la fila 55 con RMSE 2543.43 (2,0,1)(1,1,1)[12] (h = 1)
# tiempo de proceso: 40 min
write.csv(rmse_CV, file = "data/rmse_CV_tunel_corto_h1.csv", row.names = FALSE)


# Tomo el modelo elegido

mod_elegido <- Arima(tunel_6, order = c(2, 0, 1),
      seasonal = list(order = c(0, 1, 1), period = 12), include.drift=FALSE)
 

forecast::ggtsdisplay(residuals(mod_elegido), lag.max=10, main='')
autoplot(mod_elegido)

residuos <- mod_elegido$residuals
Box.test(residuos, type = "Ljung-Box")
#H0: no hay coorelacion hasta lag 10
#H0: e los datos considerados se distribuyen de forma independiente (5 % de nivel de significancia).
# p-value 0.815 > 0.05. NO Rechazo H0.

tsdiag(mod_elegido)

################################################################################

# Pronosticando
library(astsa)



# A 6 mesees
pron_6 <- sarima.for(tunel_6, 2, 0, 1, P=0, D=1, Q=1, S=12, n.ahead = 6, 
                     tol = sqrt(.Machine$double.eps), no.constant = FALSE)
lines(tunel_desde_7_2021, col="black")
points(tunel_desde_7_2021, col="black")





pron_todos <- cbind(tunel_desde_7_2021,pron_6$pred)
colnames(pron_todos) <- c("verd", "6 meses")
dim(pron_todos)
pron_todos <- data.frame(pron_todos)

# Guardo los pronosticos en un csv
write.csv(pron_todos, file = "data/pron_todos_tunel.csv", row.names = FALSE)

# Graficos

library(forecast)
pron <- forecast(fit, h=6)
plot(pron)
lines(tunel, col="black")
pron$mean # devuelve los valores pronosticados
# Otra forma de plotear
fit %>% forecast(h=6) %>% autoplot(include=100) 




grafico <- pron_12$pred
plot(grafico)



tiempo<-seq(as.Date("2014/1/1"), by = "month", length.out =120 )
class(tiempo)
length(tiempo)
length(Forecast[469:498])
plot(tiempo,c(Forecast[469:498],datos$protimb),type='l',ylim=c(10000,30000), xlab='Time(months)',ylab='Q[m^3/s]',cex.main=1,xaxt="n")
lines(tiempo,c(rep(NA,30),unabanda),lwd=2,lty=2,col='blue')
lines(tiempo,c(rep(NA,30),unabanda1[1:6]),lwd=2,lty=2,col='blue')
lines(tiempo,c(rep(NA,30),predecir[1:6]),lwd=2,lty=1,col='red')











################################################################################
# Para graficar p-valore vs.lag a mano
# Paso 1: Calcular los residuos
residuos <- residuals(mod_elegido)

# Paso 2: Calcular los p-valores de la prueba Ljung-Box para diferentes lags
lags <- 1:12  # Números de retrasos a considerar
pvalores <- numeric(length(lags))

for (i in 1:length(lags)) {
  pvalores[i] <- Box.test(residuos, lag = lags[i])$p.value
}


# Paso 3: Graficar los p-valores vs. lag
plot(lags, pvalores, type = "b", xlab = "Lag", ylab = "P-valor", 
     main = "P-valores de la prueba Ljung-Box", ylim=c(0.04,1))
#abline(h = 0.05, col = "red")  # Nivel de significancia 0.05
abline(h = 0.05, lty = "dashed", col="blue")  # Línea punteada en y = 0.05

