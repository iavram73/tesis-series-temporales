library(forecast)


# Seleccion de modelos.
# Usamos las series generadas SARIMA(1,1,1)x(1,0,1)_12 y probamos posibles combunaciones de orden.
# Seleccionamos el orden con el BIC/AIC. 
# Reptimos Nrep veces y guardamos el orden seleccionado.
# Lo hacemos para 3 valores de t_final

# Trabajo con las series ya generadas
sarima111 <- read.csv("data/muestra_sarima_111_101_12.csv") # 1000 series

# saco los primeros 100 datos de cada serie.
sarima111 <- sarima111[,101:600]

dim(sarima111) # 1000  500
#View(sarima111)

plot(as.numeric(sarima111[10,]))

###########################################

# Modelos a probar, todos con periodo estacional = 12
# 111 101 correcto (modelo 1)
# 101 101 le saco la tendencia (modelo 2)
# 111 001 le saco el P (modelo 3)
# 111 100 le saco el Q (modelo 4)

t_final <- 50 # 50, 200 y 500

N_rep <- 1000
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)


set.seed(123)
for(i in 1:N_rep){
  tryCatch({
    serie <- as.numeric(sarima111[i, 1:t_final])
    aic_s <- c() # Aca voy a guardar los aic
    bic_s <- c() # Aca voy a guardar los aic
    print(i)
      
      ajuste1 <- Arima(serie, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 0, 1), period = 12),
                        method = "ML")
      ajuste2 <- Arima(serie, order = c(1, 0, 1),
                       seasonal = list(order = c(1, 0, 1), period = 12),
                       method = "ML")
      ajuste3 <- Arima(serie, order = c(1, 1, 1),
                       seasonal = list(order = c(0, 0, 1), period = 12),
                       method = "ML")
      ajuste4 <- Arima(serie, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      
      aic_s <- c(ajuste1$aic, ajuste2$aic,ajuste3$aic,ajuste4$aic)
      bic_s <- c(ajuste1$bic, ajuste2$bic,ajuste3$bic,ajuste4$bic)
      
      orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
      orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
      
    
      }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
  
  
}

table(orden_elegido_aic) 
table(orden_elegido_bic)


# Para t= 500 le quedan 765 series de las 1000
# siempre elige el orden correcto. 

# Para t= 200 le quedan 767 series de las 1000
# # siempre elige el orden correcto.

# Para t= 50 le quedan 924 series de las 1000
# orden_elegido_aic
# 1     3   4 
# 776  19 129 
#orden_elegido_bic
# 1     3   4 
# 580  59 285 


frec_selec_sarima_50 <- data.frame("Modelo"= c("A", "B", "C", "D"),
                            "Frec_AIC"=c(776,0,19,129),
                            "Frec_BIC"= c(580,0,59,285))

# Guardo el data frame en un csv
write.csv(frec_selec_sarima_50, file = "data/frec_selec_sarima_50.csv", row.names = TRUE)








