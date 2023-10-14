#library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param_ar3 <- read.csv("data/dist_param_ar3.csv", header = TRUE)
colnames(param_ar1)


# Histogramas de phi1, phi2 y phi3 para AR(3)

g1 <- ggplot(param_ar3, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A")+
  xlim(c(0.3, 1.1)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# expression(paste("\u03d5"[1]," = 0.7")),

g2 <- ggplot(param_ar3, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A")+
  xlim(c(0.3, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[1]," = 0.7")), 

g3 <- ggplot(param_ar3, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[2]," = 0.2")),

g4 <- ggplot(param_ar3, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 19, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[2]," = 0.2")),

g5 <- ggplot(param_ar3, aes(x = phis3_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[3]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4.5))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[3]," = -0.3")),

g6 <- ggplot(param_ar3, aes(x = phis3_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[3]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[3]," = -0.3")),

dist_param_ar3 <-grid.arrange(g1,g3,g5,g2,g4,g6,ncol=3,
                              widths = c(1, 1, 1))  # Ajusta los anchos de las columnas para aumentar el espacio
