
library(forecast)
library(tidyverse)
library(stats)
library(tseries)
library(astsa)
library(gridExtra)


### Leo los datos de la serie completa, desde 1975 a 2021
tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)
### Convierto los datos de la columna 4 a serie temporal
tunel <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
head(tunel)
tail(tunel)
### Usamos el siguiente conjunto de datos 
tunel_6 <- window(tunel, start = c(1977, 1),end = c(2016, 6)) 


# Modelo seleccionado por AIC/BIC
mod_AB <- Arima(tunel_6, order = c(3, 0, 0),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

# Prueba de Ljung-Box
Box.test(residuals(mod_AB), type = "Ljung-Box") # p-value = 0.9004
# H0: se comoporta como ruido blanco (se distribuyen de forma independiente)
# H1: no es ruido blanco
# p-value > 0.05, no rechazo H0.

res_AB <- mod_AB$residuals

autoplot(res_AB)+ theme_bw()
forecast::ggtsdisplay(residuals(mod_AB), lag.max=48, main='')

autoplot(mod_AB) # Veo las raices

# diagnóstico de los residuos
tsdiag(mod_AB) 
# Los p-valore son menores a 0.05...se rechaza H0 que dice 
# que los valores de los residuos corresponden a ruido blanco.
#########################################################################
autoplot(res_AB)+ 
  labs(x="Fecha", y="residuos estandarizados")+
  theme_bw()

tamanio_letra <- 44

g1 <- ggAcf(res_AB, lag.max = 48) + 
  theme(axis.text.x = element_text(size = tamanio_letra), 
        axis.text.y = element_text(size = tamanio_letra),
        axis.title = element_text(size = tamanio_letra)) +
  labs(title = NULL) +  # Eliminar el título del gráfico
  theme_bw()

g2 <-ggPacf(res_AB, lag.max = 48) + 
  theme(axis.text.x = element_text(size = tamanio_letra), 
        axis.text.y = element_text(size = tamanio_letra),
        axis.title = element_text(size = tamanio_letra),
        plot.subtitle = element_text(size = 42)) +
  labs(title = NULL) +  # Eliminar el título del gráfico
  theme_bw()

# p-valores vs. lag
lag_max <- 24
ljung_box_test <- sapply(1:lag_max, function(lag) {
  Box.test(res_AB, lag = lag, type = "Ljung-Box")$p.value
})

# Crear un data frame con los valores
data <- data.frame(lag = 1:lag_max, p_valor = ljung_box_test)

# Crear el gráfico con ggplot2

g3 <- ggplot(data, aes(x = lag, y = p_valor)) +
  geom_point(shape = 16, size = 2, color = "#6F6F6F") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(size = tamanio_letra), 
        axis.text.y = element_text(size = tamanio_letra),
        axis.title = element_text(size = tamanio_letra)) +
  theme_bw() +
  labs(x= "Lag")+
  #theme(plot.margin = margin(10, 10, 30, 10)) +  # Ajusta los márgenes para dar espacio al título
  annotate(geom = "text",
           x = lag_max + 0.5, y = 0.055,  # Ajusta las coordenadas para el título
           label = "Prueba de Ljung-Box",
           color ="black", size = 6,
           angle = 0,  # Mantén el ángulo en 0 para que el título esté horizontal
           hjust = 1, vjust = -0.2) 


res_6m_aic_bic <- grid.arrange(g1,g2,g3, nrow=1)

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/res_6m_aic_bic.png", res_6m_aic_bic,dpi = 300, width = 18, height = 4, units = "in")




#########################################################################

# Modelo seleccionado por CV
mod_CV <- Arima(tunel_6, order = c(3, 0, 1),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

res_CV <- mod_CV$residuals
autoplot(mod_CV)
tsdiag(mod_CV)

# Modelo seleccionado por train test
mod_TT <- Arima(tunel_6, order = c(3, 0, 1),
                seasonal = list(order = c(1, 1, 1), period = 12),
                include.drift = FALSE)
res_TT <- mod_TT$residuals
autoplot(mod_TT)
tsdiag(mod_TT)



########################################################

ggAcf(res_CV, lag.max = 48, theme = theme_bw())


ggAcf(res_CV, lag.max = 48) + 
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.subtitle = element_text(size = 16))+
  theme_bw()
