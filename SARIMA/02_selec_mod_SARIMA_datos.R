library(forecast)
#library(ggplot2)
#library(tidyverse)
#library(gridExtra)


# Seleccion de modelos.
# Usamos las series generadas SARIMA(1,1,1)x(1,0,1)_12 y probamos posibles combunaciones de orden.
# Seleccionamos el orden con el BIC. 
# Reptimos Nrep veces y guardamos el orden seleccionado.
# Lo hacemos para 3 valores de t_final

# Trabajo con las series ya generadas en txt
sarima111 <- read.csv("data/muestra_sarima_111_101_12.csv") # 1000 series

# saco los primeros 100 datos de cada serie.
sarima111 <- sarima111[,101:600]

dim(sarima111) # 1000  500
#View(sarima111)

plot(as.numeric(sarima111[10,]))

###########################################

# Modelos a probar, todos con periodo estacional = 12
# 111 101 correcto
# 100 100
# 110 100
# 101 100
# 011 100
# 001 100

t_final <- 50 # 50, 200 y 500

N_rep <- 10
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)


set.seed(123)
for(i in 1:N_rep){
  tryCatch({
    serie <- as.numeric(sarima111[i, 1:t_final])
    aic_s <- c() # Aca voy a guardar los aic
    bic_s <- c() # Aca voy a guardar los aic
    
      ajuste1 <- Arima(serie, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 0, 1), period = 12),
                        method = "ML")
      ajuste2 <- Arima(serie, order = c(1, 0, 0),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      ajuste3 <- Arima(serie, order = c(1, 1, 0),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      ajuste4 <- Arima(serie, order = c(1, 0, 1),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      ajuste5 <- Arima(serie, order = c(0, 1, 1),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      ajuste6 <- Arima(serie, order = c(0, 0, 1),
                       seasonal = list(order = c(1, 0, 0), period = 12),
                       method = "ML")
      ajuste7 <- Arima(serie, order = c(2, 1, 1),
                       seasonal = list(order = c(1, 0, 1), period = 12),
                       method = "ML")
      
      aic_s <- c(ajuste1$aic, ajuste2$aic,ajuste3$aic,ajuste4$aic,ajuste5$aic,ajuste6$aic, ajuste7$aic)
      bic_s <- c(ajuste1$bic, ajuste2$bic,ajuste3$bic,ajuste4$bic,ajuste5$bic,ajuste6$bic, ajuste7$bic)
      
      orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
      orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
      
    
      }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
  
  
}

table(orden_elegido_aic) # siempre elige el orden correcto...
table(orden_elegido_bic)

# de 100 series le quedaron 39 y elije siempre el ajuste 6 (111 111)
#table(orden_elegido_aic) 
#table(orden_elegido_bic)

# Cambio el ajuste 6 por este:(111 101) y vuelvo a correr
#table(orden_elegido_aic) 
# NA NA NA  1 NA  6  2  4  6 NA
#table(orden_elegido_bic)
#  NA NA NA  1 NA  6  6  6  6 NA

# Cuardo los archivos
#write.csv(modelos, file = "data/selec_modelo_arima212_500.csv", row.names = TRUE)
#write.csv(modelos, file = "data/selec_modelo_arima212_200.csv", row.names = TRUE)
#write.csv(modelos, file = "data/selec_modelo_arima212_50.csv", row.names = TRUE)




################################################################
# Pruebo arrancar con un modelo más complejo
sarima212 <- read.csv("data/muestra_sarima_212_202_12.csv")
sarima212 <- sarima212[,101:600]

# Modelos a probar, todos con periodo estacional = 12
# 212 202 correcto
# 202 202
# 211 102
# 212 201
# 111 202
# 111 111



t_final <- 500 # 50, 200 y 500

N_rep <- 1000
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)


set.seed(123)
for(i in 1:N_rep){
  tryCatch({
    serie <- as.numeric(sarima212[i, 1:t_final])
    aic_s <- c() # Aca voy a guardar los aic
    bic_s <- c() # Aca voy a guardar los aic
    
    ajuste1 <- Arima(serie, order = c(2, 1, 2),
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste2 <- Arima(serie, order = c(2, 0, 2),
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste3 <- Arima(serie, order = c(2, 1, 2),
                     seasonal = list(order = c(1, 0, 2), period = 12),
                     method = "ML")
    ajuste4 <- Arima(serie, order = c(2, 1, 2),
                     seasonal = list(order = c(2, 0, 1), period = 12),
                     method = "ML")
    ajuste5 <- Arima(serie, order = c(1, 1, 1),
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste6 <- Arima(serie, order = c(1, 1, 1),
                     seasonal = list(order = c(1, 0, 1), period = 12),
                     method = "ML")
    
    aic_s <- c(ajuste1$aic, ajuste2$aic,ajuste3$aic,ajuste4$aic,ajuste5$aic,ajuste6$aic)
    bic_s <- c(ajuste1$bic, ajuste2$bic,ajuste3$bic,ajuste4$bic,ajuste5$bic,ajuste6$bic)
    
    orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
    orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
    
    
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
  
  
}

# Guardo en dos vectores los ordenes elegidos por cada metrica
orden_aic <- orden_elegido_aic
orden_bic <- orden_elegido_bic 

# Armo un data frame
orden_elegido_sarima <- data.frame("orden_aic"= orden_aic,
                       "orden_bic"=orden_bic)

# Guardo el data frame en un csv
write.csv(orden_elegido_sarima, file = "data/orden_elegido_sarima_500.csv", row.names = TRUE)

orden_sarima_500 <- read.csv("data/orden_elegido_sarima_500.csv")

head(orden_sarima_500)

table(orden_sarima_500$orden_aic)
# 1   2   3   5 
# 241  76  62   8 

table(orden_sarima_500$orden_bic)
# 1   2   3   5 
# 212  47 114  14

# Propuesta: correr de nuevo el código, pero sacando ajuste 3.

###############################################################################

# Pruebo arrancar con un modelo más complejo
sarima212 <- read.csv("data/muestra_sarima_212_202_12.csv")
sarima212 <- sarima212[,101:600]

# 212 202 correcto
# modelo incorrecto: sarima_202_202_12, 
# modelo incorrecto: sarima_010_202_12



t_final <- 500 # 50, 200 y 500

N_rep <- 1000
orden_elegido_aic <- rep(NA, N_rep)
orden_elegido_bic <- rep(NA, N_rep)


set.seed(123)
for(i in 1:N_rep){
  tryCatch({
    serie <- as.numeric(sarima212[i, 1:t_final])
    aic_s <- c() # Aca voy a guardar los aic
    bic_s <- c() # Aca voy a guardar los aic
    
    print(i)
    
    ajuste1 <- Arima(serie, order = c(2, 1, 2), # 212 202 correcto
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste2 <- Arima(serie, order = c(2, 0, 2), 
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste3 <- Arima(serie, order = c(0, 1, 0), 
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    
    aic_s <- c(ajuste1$aic, ajuste2$aic,ajuste3$aic)
    bic_s <- c(ajuste1$bic, ajuste2$bic,ajuste3$bic)
    
    orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
    orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
    
    
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
  
  
}

# Guardo en dos vectores los ordenes elegidos por cada metrica
orden_aic <- orden_elegido_aic
orden_bic <- orden_elegido_bic 

# Armo un data frame
orden_elegido <- data.frame("orden_aic"= orden_aic,
                            "orden_bic"=orden_bic)

# Guardo el data frame en un csv
write.csv(orden_elegido, file = "data/orden_elegido_500.csv", row.names = TRUE)

orden_500 <- read.csv("data/orden_elegido_500.csv")

head(orden_500)

table(orden_500$orden_aic)
# 1     2    
# 374  150   


table(orden_500$orden_bic)
# 1     2    
# 394  130  

##############################################################################
# Otra opcion con modelo mas simple:

sarima111 <- read.csv("data/muestra_sarima_111_101_12.csv") # 1000 series

# saco los primeros 100 datos de cada serie.
sarima111 <- sarima111[,101:600]

dim(sarima111) # 1000  500
#View(sarima111)


# 111 101_12 correcto
# modelo incorrecto: sarima_101_202_12, 
# modelo incorrecto: sarima_010_202_12



t_final <- 500 # 50, 200 y 500
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
    
    ajuste1 <- Arima(serie, order = c(1, 1, 1), # 212 202 correcto
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste2 <- Arima(serie, order = c(1, 0, 1), 
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    ajuste3 <- Arima(serie, order = c(0, 1, 0), 
                     seasonal = list(order = c(2, 0, 2), period = 12),
                     method = "ML")
    
    aic_s <- c(ajuste1$aic, ajuste2$aic,ajuste3$aic)
    bic_s <- c(ajuste1$bic, ajuste2$bic,ajuste3$bic)
    
    orden_elegido_aic[i] <- which.min(aic_s) # Elijo el minimo valor de aic para cada serie
    orden_elegido_bic[i] <- which.min(bic_s) # Elijo el minimo valor de bic para cada serie
    
    
  }, error = function(e) {
    # Manejo del error
    # Puedes imprimir un mensaje de error, asignar valores predeterminados a las variables o hacer lo que consideres apropiado.
    print(paste("Error en iteración", i, ": ", conditionMessage(e)))
  })
  
  
}

# Guardo en dos vectores los ordenes elegidos por cada metrica
orden_aic <- orden_elegido_aic
orden_bic <- orden_elegido_bic 

# Armo un data frame
orden_elegido <- data.frame("orden_aic"= orden_aic,
                            "orden_bic"=orden_bic)

# Guardo el data frame en un csv
write.csv(orden_elegido, file = "data/orden_elegido_mod_simple_500.csv", row.names = TRUE)

orden_500 <- read.csv("data/orden_elegido_mod_simple_500.csv")

head(orden_500)

table(orden_500$orden_aic)
# 1        
# 965     
table(orden_500$orden_bic)
# 1         
# 965    



