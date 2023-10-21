library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia de los parametros ajustados con 
# sarima_202_202_12 cuando los datos fueron generados 
# por un sarima_212_202_12.

dist_param <- read.csv("data/dist_param_sarima_202_202_12_fuera_mod.csv",  header = TRUE)

dist_param_na_omit <- na.omit(dist_param)
dim(dist_param_na_omit) # 596  10
colnames(dist_param_na_omit)
View(dist_param_na_omit)

# Parametros reales: 
# ar= c(0.7,0.2), ma=c(0.8,0.4), 
# sar= c(0.6,0.3), sma=c(0.7,0.5)
# d=1, D=0

# histogramas parte regular

g1 <- ggplot(dist_param_na_omit, aes(x = phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 30, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-0.2,2.2))+ theme_bw()

# 
# expression(paste("Histograma de ","\u03C6"[1]," ajustado con ARIMA(1,1,1). 
# Porceso real ARIMA(2,1,1).")), y= "densidad",

g2 <- ggplot(dist_param_na_omit, aes(x = phis2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 40, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03C6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,35))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(0.2,1.1))+ theme_bw()


g3 <- ggplot(dist_param_na_omit, aes(x = titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 40, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,7))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(0,2))+ theme_bw()


g4 <- ggplot(dist_param_na_omit, aes(x = titas2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 40, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,7))+
  geom_vline(xintercept = 0.4, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-0.2,1.2))+ theme_bw()


grid.arrange(g1,g2, nrow=1)

grid.arrange(g3,g4, nrow=1) # Tamaño figura 500 x 400


# histogramas parte estacional

g5 <- ggplot(dist_param_na_omit, aes(x = Phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 30, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03A6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-0.2,2.2))+ theme_bw()

g6 <- ggplot(dist_param_na_omit, aes(x = Phis2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 28, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03A6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.8))+
  geom_vline(xintercept = 0.3, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-1,1))+ theme_bw()

g7 <- ggplot(dist_param_na_omit, aes(x = Titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 36, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,3.5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-0.8,1.4))+ theme_bw()


g8 <- ggplot(dist_param_na_omit, aes(x = Titas2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 35, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u0398"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,6))+
  geom_vline(xintercept = 0.5, linetype="dashed", color="#2678B3",lwd=0.8)+
  xlim(c(-0.3,0.9))+ theme_bw()

grid.arrange(g5,g5, nrow=1)

grid.arrange(g7,g8, nrow=1)



