
library(forecast)
library(tidyverse)
library(stats)
library(tseries)
library(astsa)
library(cowplot)
library(ggplot2)
library(gridExtra)

### Leo los datos de la serie completa, desde 1975 a 2021
tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)

### Convierto los datos de la columna 4 a serie temporal
tunel <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
head(tunel)
tail(tunel)

tunel_6 <- window(tunel, start = c(1977, 1),end = c(2016, 6)) 
tunel_x_6 <- window(tunel, start = c(2016, 7),end = c(2016, 12)) # datos secretos
tunel_12 <- window(tunel, start = c(1977, 1),end = c(2015, 12)) 
tunel_x_12 <- window(tunel, start = c(2016, 1),end = c(2016, 12)) # datos secretos

tunel_corto <- window(tunel, start = c(2014, 1),end = c(2016, 12))


# Modelo seleccionado para 6 meses
mod_6m <- Arima(tunel_6, order = c(3, 0, 0),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

# Modelo seleccionado para 12 meses
mod_12m <- Arima(tunel_12, order = c(3, 0, 1),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)


# Pronósticando
mod_6m <- forecast(mod_6m, h=6)
mod_12m <- forecast(mod_12m, h=12)

tamanio_letras <- 16  # Ajusta este valor según tu preferencia

# Grafico de las series y los pronosticos

g1 <-autoplot(tunel_corto) +
  autolayer(mod_3m, series = "CV", show.legend = FALSE) +
  autolayer(tunel_corto, color = "black", series = "real") +
  labs(y = expression(paste("caudal (", m^3/seg,")")), x = "Tiempo (años)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = tamanio_letras),      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = tamanio_letras),       # Tamaño de las etiquetas de los ejes
    axis.title.x = element_text(size = tamanio_letras),    # Tamaño del título del eje x
    axis.title.y = element_text(size = tamanio_letras)     # Tamaño del título del eje y
  )


g2 <-autoplot(tunel_corto) +
  autolayer(mod_6m,  show.legend = FALSE) +
  autolayer(tunel_corto, color = "black", series = "real") +
  labs(y = expression(paste("caudal (", m^3/seg,")")), x = "Tiempo (años)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = tamanio_letras),      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = tamanio_letras),       # Tamaño de las etiquetas de los ejes
    axis.title.x = element_text(size = tamanio_letras),    # Tamaño del título del eje x
    axis.title.y = element_text(size = tamanio_letras)     # Tamaño del título del eje y
  )
#series = "CV",

g3 <-autoplot(tunel_corto) +
  autolayer(mod_12m, show.legend = FALSE) +
  autolayer(tunel_corto, color = "black", series = "real") +
  labs(y = expression(paste("caudal (", m^3/seg,")")), x = "Tiempo (años)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = tamanio_letras),      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = tamanio_letras),       # Tamaño de las etiquetas de los ejes
    axis.title.x = element_text(size = tamanio_letras),    # Tamaño del título del eje x
    axis.title.y = element_text(size = tamanio_letras)     # Tamaño del título del eje y
  )


pron_todos <- grid.arrange(g2,g3, nrow=2)


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/pron_todos.png", pron_todos, dpi = 300, width = 14, height = 10, units = "in")

