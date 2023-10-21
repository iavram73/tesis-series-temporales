library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia de los param 
# ajustado con ARMA(1,1), cuando los datos fueron generados por un ARMA(2,1).

dist_param_arma11 <- read.csv("data/dist_param_arma11_fuera_mod.csv",  header = TRUE)
colnames(dist_param_arma11)

g5 <- ggplot(dist_param_arma11, aes(x = phis1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "a)", y= "densidad",x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g6 <- ggplot(dist_param_arma11, aes(x = titas1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "b)", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(-0.8, -0.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dist_param_arma11_fuera_mod <- grid.arrange(g5,g6,nrow=1)
