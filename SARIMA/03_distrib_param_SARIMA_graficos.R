library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param <- read.csv("data/dist_param_sarima_111_101_12.csv", header = TRUE)
colnames(param)
param_omit_na <- na.omit(param)
#View(param_omit_na)
#dim(param_omit_na) # 1000   11

# ar= c(0.7), ma=c(0.8), 
# sar= c(0.6), sma=c(0.7)

# Histograma para phi1 y tita 1 parte regular
g17 <- ggplot(param_omit_na, aes(x = phis1_100)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ", "100 observaciones")), 
       y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.35, 0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g18 <- ggplot(param_omit_na, aes(x = phis1_500)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ", "500 observaciones")), 
       y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.35,0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g19 <- ggplot(param_omit_na, aes(x = titas1_100)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ", "100 observaciones")), 
       y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.5, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g20 <- ggplot(param_omit_na, aes(x = titas1_500)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ", "500 observaciones")), 
       y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.5,1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_param_sarima_reg <- grid.arrange(g17,g19,g18,g20, nrow=2)


# Histograma para phi1 y tita 1 parte estacional

g21 <- ggplot(param_omit_na, aes(x = Phis1_100)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03a6"[1]," - ", "100 observaciones")), 
       y= "densidad",
       x= expression("\u03a6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.35, 0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g22 <- ggplot(param_omit_na, aes(x = Phis1_500)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03a6"[1]," - ", "500 observaciones")), 
       y= "densidad",
       x= expression("\u03a6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.35,0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g23 <- ggplot(param_omit_na, aes(x = Titas1_100)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u0398"[1]," - ", "100 observaciones")), 
       y= "densidad",
       x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.4, 0.95)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g24 <- ggplot(param_omit_na, aes(x = Titas1_500)) + 
  geom_histogram(colour = "gray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u0398"[1]," - ", "500 observaciones")), 
       y= "densidad",
       x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.7)+
  xlim(c(0.4,0.95)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


dist_param_sarima_est <- grid.arrange(g21,g23,g22,g24, nrow=2)

