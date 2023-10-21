#library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param_ma3 <- read.csv("data/dist_param_ma3.csv", header = TRUE)
colnames(param_ma3)

g7 <- ggplot(param_ma3, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(0.3, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#title = expression(paste("MA(3) ","\u03B8"[1]," = 0.7")),


g8 <- ggplot(param_ma3, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","500 observaciones")),  
       y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(0.3, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g9 <- ggplot(param_ma3, aes(x = titas2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[2]," - ","100 observaciones")),  
       y= "densidad",x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g10 <- ggplot(param_ma3, aes(x = titas2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 19, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[2]," - ","500 observaciones")),
       x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g11 <- ggplot(param_ma3, aes(x = titas3_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[3]," - ","100 observaciones")),
       x= expression("\u03B8"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g12 <- ggplot(param_ma3, aes(x = titas3_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[3]," - ","500 observaciones")),
       x= expression("\u03B8"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dist_param_ma3 <-grid.arrange(g7,g9,g11,g8,g10,g12,nrow=2) # Tamaño figura 700 x 500
