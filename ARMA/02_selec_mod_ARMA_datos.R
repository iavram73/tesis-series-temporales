library(forecast)


# Seleccion de modelos.
# Usamos las series generadas ARMA (2,1) y probamos posibles combunaciones de orden.
# Seleccionamos el orden con el BIC/AIC
# Reptimos Nrep veces y guardamos el orden seleccionado.
# Lo hacemos para 3 valores de t_final

# Trabajo con las series ya generadas en csv
series_arma21 <- read.csv("data/muestra_arma21_0.7_0.2_-0.3.csv") # 1000 series

#fit <- Arima(as.numeric(series_arma21[10,]), order = c(1, 0, 1), method = "ML")$fitted

# saco los primeros 100 datos de cada serie.
series_arma21 <- series_arma21[,101:600]
series_arma21_sem45 <- series_arma21_sem45[,101:600]
dim(series_arma21_sem45) # 1000  500


# Pruebo primero con una sola serie, hago una funcion para ARMA (p,q):
# Argumentos: una serie y los vectores de posibles p y q.
# Devuelve un vector con p y q elegidos y los corresp bic, aic e ID.

selec_modelo_try <- function(serie, p_s, q_s, fila){
  
  combinaciones <- expand.grid(p_s,q_s) # tabla con la comb posibles de p_s y q_s
  combinaciones$bic <- rep(NA,nrow(combinaciones)) # agrega columna vacia para llenar con los BIC
  combinaciones$aic <- rep(NA,nrow(combinaciones)) # agrega columna vacia para llenar con los AIC
  combinaciones$id <- paste0("(",combinaciones$Var1,",",combinaciones$Var2,")") # agrego columna de ID
  
  p_elegido_bic <- 0
  q_elegido_bic <- 0
  mod_elegido_bic <- c()
  mod_elegido_aic <- c()
  p_elegido_aic <- 0
  q_elegido_aic <- 0
  mod_elegido <- c()
  
  set.seed(123)
  
  for(i in 1:9) {
    p <- combinaciones[i, 1]
    q <- combinaciones[i, 2]
    
    tryCatch({
      ajuste <- Arima(ts(serie), order = c(p, 0, q), method = "ML")
      combinaciones[i, 3] <- ajuste$bic
      combinaciones[i, 4] <- ajuste$aic
    }, error = function(e) {
      print(paste("encontramos un error ", i, " fila: ", fila))
      combinaciones[i, 3] <- NA
      combinaciones[i, 4] <- NA
    })
  }
  # Si encuentra error en alguna combinacion de p y q, sigue adelante 
  # y elige modelo entre las combinaciones para las cuales puede calcular.
  
  posición_bic <- which.min(combinaciones$bic)
  bic_elegido <- combinaciones$bic[posición_bic]
  p_elegido_bic <- combinaciones$Var1[posición_bic]
  q_elegido_bic <- combinaciones$Var2[posición_bic]
  id_elegido_bic <- combinaciones$id[posición_bic]
  
  posición_aic <- which.min(combinaciones$aic)
  aic_elegido <- combinaciones$aic[posición_aic]
  p_elegido_aic <- combinaciones$Var1[posición_aic]
  q_elegido_aic <- combinaciones$Var2[posición_aic]
  id_elegido_aic <- combinaciones$id[posición_aic]
  
  
  mod_elegido <-c(p_elegido_bic,q_elegido_bic,round(bic_elegido,2),id_elegido_bic,
                  p_elegido_aic,q_elegido_aic,round(aic_elegido,2),id_elegido_aic)
  
  mod_elegido
}

p<-c(0,1,2)
q<-c(0,1,2)
t_final <-500 # (Esto lo cambio 500, 200, 50)

# Armo una matriz para ir rellenando por filas con los vectores
# que devuelve la funcion creada, en cada una de las 1000 repeticiones.

modelos <- data.frame(matrix(NA,1000,8)) 

for(i in 1:1000){
  modelos[i,] <- selec_modelo_try(as.numeric(series_arma21_sem45[i,1:t_final]),p,q,i)
  colnames(modelos) <- c("p_bic", "q_bic", "BIC", "ID_bic",
                          "p_aic", "q_aic", "AIC", "ID_aic")
}


View(modelos)
anyNA(modelos)

#############################################################
# - Cambiar la semilla dentro de la función pareceria ser lo mismo. 
# O sea, dada una serie, siempre va a ajustar del mismo modo.
# - Al generar series nuevas da ligeramente diferente pero se sigue 
# con la misma tendencia, no es clara la elección del modelo correcto.
##############################################################

# Decidimos entonces probar con las series generadas ARMA (2,2)
# Corro la funcion para series generadas con ARMA (2,2)

series_arma22 <- read.csv("data/muestra_arma22_0.7_0.2_0.8_0.4.csv") # 1000 series

# saco los primeros 100 datos de cada serie.
series_arma22 <- series_arma22[,101:600]
dim(series_arma22) # 1000  500


p<-c(0,1,2)
q<-c(0,1,2)
t_final <-50 # (Esto lo cambio 500, 200, 50)

# Armo una matriz para ir rellenando por filas con los vectores
# que devuelve la funcion creada, en cada una de las 1000 repeticiones.

modelos <- data.frame(matrix(NA,1000,8)) 

for(i in 1:1000){
  modelos[i,] <- selec_modelo_try(as.numeric(series_arma22[i,1:t_final]),p,q,i)
  colnames(modelos) <- c("p_bic", "q_bic", "BIC", "ID_bic",
                         "p_aic", "q_aic", "AIC", "ID_aic")
}


View(modelos)
anyNA(modelos)

write.csv(modelos, file = "data/selec_modelo_arma22_500.csv", row.names = TRUE)
write.csv(modelos, file = "data/selec_modelo_arma22_200.csv", row.names = TRUE)
write.csv(modelos, file = "data/selec_modelo_arma22_50.csv", row.names = TRUE)

