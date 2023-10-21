
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param_arma21 <- read.csv("data/dist_param_arma21.csv", header = TRUE)
View(param_arma21)

g13 <- ggplot(param_arma21, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2, 1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g14 <- ggplot(param_arma21, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2, 1.3)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g15 <- ggplot(param_arma21, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g16 <- ggplot(param_arma21, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g17 <- ggplot(param_arma21, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.1, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g18 <- ggplot(param_arma21, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.1, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_param_arma21 <-grid.arrange(g13,g15,g17,g14,g16,g18,nrow=2)

