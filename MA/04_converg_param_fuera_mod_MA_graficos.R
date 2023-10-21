library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia del tita 1 
# ajustado con MA(1), cuando los datos fueron generados por un MA(3).

titas_conv_ma <- read.csv("data/titas_conv_ma.csv",  header = TRUE)
colnames(titas_conv_ma)

ggplot(titas_conv_ma, aes(x = titas1_ma1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.35,0.72)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Explorar, cual es la convergencia del tita 1 y 2 ajustado con MA(2)
#  cuando los datos fueron generados por un MA(3).

# grafico para titas 1 y 2 MA(2)
g3 <- ggplot(titas_conv_ma, aes(x = titas1_ma2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "b)", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.5, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g4 <- ggplot(titas_conv_ma, aes(x = titas2_ma2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 2, colour = "tomato")+
  labs(title = "", y= "densidad", x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.1, 0.8)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid.arrange(g3,g4,nrow=2)
