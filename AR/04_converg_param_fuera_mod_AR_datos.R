library(forecast)
library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar, cual es la convergencia del phi 1 ajustado con AR(1)
# cuando los datos fueron generados por un AR(3).

# Leo los datos
series_ar3 <- read.csv("data/muestra_ar3_0.7_0.2_-0.3.csv", header = TRUE)
series_ar3 <- series_ar3[,101:600] # Saco los primeros 100 datos
dim(series_ar3)
#View(series_ar3)

# Ajusto cada una de las series con AR(1) y guardo
# las estimaciones de phi1.
N_rep <- 1000
t_final <- 500
phi1_ar1 <- c()
sd_phi1_ar1 <- c()
set.seed(123)
for(i in 1:N_rep){
  print(i)
  serie <- as.numeric(series_ar3[i, 1:t_final])
  ajuste <- Arima(serie, order = c(1, 0, 0), method="ML")
  phi1_ar1[i] <- ajuste$coef[1]
  sd_phi1_ar1[i] <- sqrt(vcov(ajuste)[1, 1]) # solo para ver qué da el desvio
}

# hist(sd_phi1_ar1, freq = FALSE)

phis_conv_ar <- data.frame("phi_1_AR1"= phi1_ar1, "phi_1_AR2"= phi_1_ar2,"phi_2_AR2"=phi_2_ar2)
write.csv(phis_conv_ar, file = "phis_conv_ar.csv", row.names = TRUE)

