library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia de phi 1 ajustado con AR(1), 
# cuando los datos fueron generados por un AR(3).

df_phis_conv_ar <- read.csv("data/phis_conv_ar.csv",  header = TRUE)
colnames(df_phis_conv_ar)

ggplot(df_phis_conv_ar, aes(x = phi_1_AR1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "",  y= "densidad", x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.58, 0.8)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



####################################################################
# Explorar, bajo el modelo normal, cual es la convergencia del phi 1 y 2
# ajustado con AR(2) cuando los datos fueron generados por un AR(3).

g1 <- ggplot(df_phis_conv_ar, aes(x = phi_1_AR2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "a)", y= "densidad",x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.4, 0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g2 <- ggplot(df_phis_conv_ar, aes(x = phi_2_AR2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 2, colour = "tomato")+
  labs(subtitle = "", y= "densidad",x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(-0.3, 0.3)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid.arrange(g1,g2, nrow=1)
