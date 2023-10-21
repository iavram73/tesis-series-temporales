library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)

# Explorar cual es la convergencia de los parametros ajustados con 
# sarima_101_101_12 cuando los datos fueron generados 
# por un sarima_111_101_12.

dist_param <- read.csv("data/dist_param_sarima_101_101_12_fuera_mod.csv",  header = TRUE)

dist_param_na_omit <- na.omit(dist_param)
dim(dist_param_na_omit) # 765   6
colnames(dist_param_na_omit)
#View(dist_param_na_omit)

# Parametros reales: 
# ar= c(0.7), ma=c(0.8), 
# sar= c(0.6), sma=c(0.7)
# d=1, D=0

hist(dist_param_na_omit$Phis1_500)

# histogramas parte regular

g1 <- ggplot(dist_param_na_omit, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 60, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,250))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.69,1.01))+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g2 <- ggplot(dist_param_na_omit, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 40, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,28))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.78,1.05))+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# histogramas parte estacional

g3 <- ggplot(dist_param_na_omit, aes(x = Phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u03A6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.3,1.3))+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g4 <- ggplot(dist_param_na_omit, aes(x = Titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = "",  subtitle = "", x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,6))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.2,1))+ theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_sarima_fuera_mod_reg <- grid.arrange(g1,g2, nrow=1)

dist_sarima_fuera_mod_est <- grid.arrange(g3,g4, nrow=1)
