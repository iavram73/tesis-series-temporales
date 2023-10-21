
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param_arima211 <- read.csv("data/dist_param_arima211.csv", header = TRUE)
dim(param_arima211)

# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 100
g1 <- ggplot(param_arima211, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 500
g2 <- ggplot(param_arima211, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 100
g3 <- ggplot(param_arima211, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 500
g4 <- ggplot(param_arima211, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 100
g5 <- ggplot(param_arima211, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,1.8))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 500
g6 <- ggplot(param_arima211, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_param_arima21 <-grid.arrange(g1,g3,g5,g2,g4,g6, nrow=2)
