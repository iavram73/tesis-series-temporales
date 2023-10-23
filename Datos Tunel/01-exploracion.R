library(forecast)
library(tidyverse)
library(dplyr)
library(stats)
library(tseries)
library(gridExtra)
library(cowplot)
library(ggplot2)

# par(mfrow=c(1,1))

# Leo los datos de la serie completa, desde 1975 a 2021
tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)

# Convierto los datos de la columna 4 a serie temporal
tunel <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
head(tunel)
tail(tunel)

### Usamos el mismo conjunto de datos que usó Melanie
tunel_m <- window(tunel, start = c(1977, 1),end = c(2016, 12)) 
head(tunel_m)
tail(tunel_m)

g1 <- autoplot(tunel_m, col = "#1C5F9E") +
  labs(y = expression(paste("caudal en  ", m^3/seg)), x = "tiempo (años)") +
  ggtitle("", subtitle = "") + # Caudal medio mensual
  theme_bw() +
  scale_x_continuous(breaks = seq(1977, 2016, by = 3))+
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20))


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/tunel_m.png", g1, dpi = 300, width = 14, height = 6, units = "in")


# Funcion de autocorrelacion ACF
g2 <- ggAcf(tunel_m, lag.max = 48) + theme_bw() +
  ggtitle("", subtitle = "") +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 48, by = 6))

# Funcion de autocorrelacion parcial PACF
g3 <- ggPacf(tunel_m, lag.max = 48) + theme_bw() +
  ggtitle("", subtitle = "") +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 48, by = 6))

acf_pacf <- grid.arrange(g2,g3,nrow=1)

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/acf_pacf.png", acf_pacf,dpi = 300, width = 14, height = 6, units = "in")

# Hago la primera diferencia con periodo 1, y vuelvo a graficar la ACF.
dif_1_m = diff(tunel_m,differences=1,main=NA)

g4 <- ggAcf(dif_1_m, lag.max = 48) + theme_bw() +
  ggtitle("", subtitle = "a)") +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 48, by = 6))
# Seliminó la memoria de la parte AR, y se ve la estacionalidad... 

# Hago la primera diferencia con periodo 12, para verificar 
# si eliminamos la estacionalidad, y vuelvo a graficar la ACF.

dif_12_m = diff(tunel_m,differences=1,lag=12,main=NA)

g5 <- ggAcf(dif_12_m, lag.max = 48) + theme_bw() +
  ggtitle("", subtitle = "b)") +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(0, 48, by = 6))


acf_pacf_dif <- grid.arrange(g4,g5,nrow=1)

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/acf_pacf_dif.png", acf_pacf_dif,dpi = 300, width = 12, height = 6, units = "in")


###############################################################################

# Para ver si el proceso es estacionario: 
# Dickey Fuler
adf.test(tunel_m)
# alternative hypothesis: stationary
# Con p-valor = 0.01 < 0.05 rechazo H0 (el proceso es estacionario).
