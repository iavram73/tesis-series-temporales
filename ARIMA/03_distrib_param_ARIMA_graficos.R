
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param_arima211 <- read.csv("data/dist_param_arima211.csv", header = TRUE)
dim(param_arima211)

# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 100
g1 <- ggplot(param_arima211, aes(x = phis1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1) ","\u03C6"[1]," = 0.7")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()


# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 500
g2 <- ggplot(param_arima211, aes(x = phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1) ","\u03C6"[1]," = 0.7")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()

grid.arrange(g1,g2) # Guardo grafico tamaño 600 x 500

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 100

g3 <- ggplot(param_arima211, aes(x = phis2_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1)) ","\u03C6"[2]," = 0.2")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 500

g4 <- ggplot(param_arima211, aes(x = phis2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1) ","\u03C6"[2]," = 0.2")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()

grid.arrange(g3,g4)


# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 100

g5 <- ggplot(param_arima211, aes(x = titas1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1) ","\u03B8"[1]," = -0.3")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,1.8))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()

# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 500

g6 <- ggplot(param_arima211, aes(x = titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("ARIMA(2,1,1) ","\u03B8"[1]," = -0.3")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()

grid.arrange(g1,g3,g5,g2,g4,g6, nrow=2)


