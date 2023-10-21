#library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Leo los datos c=guardados en csv
param <- read.csv("data/dist_param_sarima_212_202_12.csv", header = TRUE)
colnames(param)
param_omit_na <- na.omit(param)
View(param_omit_na)
dim(param_omit_na) # 744  19

# ar= c(0.7,0.2), ma=c(0.8,0.4), 
# sar= c(0.6,0.3), sma=c(0.7,0.5)

# Histogramas de phi1 y phi2 (de la parte AR)
g1 <- ggplot(na.omit(param), aes(x = phis1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[1]," = 0.7")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.1)) + theme_bw()

g2 <- ggplot(param, aes(x = phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[1],"verdadero"," = 0.7")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.1)) + theme_bw()

g3 <- ggplot(param, aes(x = phis2_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[2],"verdadero"," = 0.2")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.1)) + theme_bw()

g4 <- ggplot(param, aes(x = phis2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[2],"verdadero"," = 0.2")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.1)) + theme_bw()


grid.arrange(g1,g3,g2,g4,nrow=2)



# Histogramas de titas1 y titas2 (de la parte MA)

g5 <- ggplot(na.omit(param), aes(x = titas1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[1]," = 0.8")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.8)) + theme_bw()

g6 <- ggplot(param, aes(x = titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[1],"verdadero"," = 0.8")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1.8)) + theme_bw()

g7 <- ggplot(param, aes(x = titas2_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[2],"verdadero"," = 0.4")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))+
  geom_vline(xintercept = 0.4, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1)) + theme_bw()

g8 <- ggplot(param, aes(x = titas2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[2],"verdadero"," = 0.4")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9))+
  geom_vline(xintercept = 0.4, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.2, 1)) + theme_bw()

grid.arrange(g5,g7,g6,g8,nrow=2)

######################################################################

# Ahora con la parte estacional

######################################################################

# Histogramas de Phi1 y Phi2 (de la parte SAR)

g9 <- ggplot(na.omit(param), aes(x = Phis1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03A6"[1]," = 0.6")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03A6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.3, 2)) + theme_bw()

g10 <- ggplot(param, aes(x = Phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03A6"[1],"verdadero"," = 0.6")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03A6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))+
  geom_vline(xintercept = 0.6, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.3, 1.2)) + theme_bw()

g11 <- ggplot(param, aes(x = Phis2_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03A6"[2],"verdadero"," = 0.3")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03A6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5))+
  geom_vline(xintercept = 0.3, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.9, 1.3)) + theme_bw()

g12 <- ggplot(param, aes(x = Phis2_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03A6"[2],"verdadero"," = 0.3")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u03A6"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))+
  geom_vline(xintercept = 0.3, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.9, 1.3)) + theme_bw()


grid.arrange(g9,g11,g10,g12,nrow=2)




# Histogramas de Titas1 y Titas2 (de la parte SMA)

g13 <- ggplot(na.omit(param), aes(x = Titas1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u0398"[1]," = 0.7")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.8, 2)) + theme_bw()

g14 <- ggplot(param, aes(x = Titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u0398"[1],"verdadero"," = 0.7")), 
       subtitle = "500 observaciones por serie", y= "densidad",
       x= expression("\u0398"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.8, 2)) + theme_bw()

g15 <- ggplot(param, aes(x = Titas2_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u0398"[2],"verdadero"," = 0.5")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u0398"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5))+
  geom_vline(xintercept = 0.5, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(-0.3, 1.5)) + theme_bw()

g16 <- 
  

grid.arrange(g13,g15,g14,g16,nrow=2)



#http://127.0.0.1:39881/graphics/71f7d24c-18fb-443a-8d55-3b3a4714e87b.png

########################################################################

# Leo los datos c=guardados en csv
param <- read.csv("data/dist_param_sarima_111_101_12.csv", header = TRUE)
colnames(param)
param_omit_na <- na.omit(param)
View(param_omit_na)
dim(param_omit_na) # 1000   11

# ar= c(0.7), ma=c(0.8), 
# sar= c(0.6), sma=c(0.7)

# Histograma para phi1
g17 <- ggplot(param_omit_na, aes(x = phis1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[1]," = 0.7")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(0.35, 0.9)) + theme_bw()

g18 <- ggplot(param_omit_na, aes(x = phis1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03C6"[1]," = 0.7")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03C6"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(0.35,0.9)) + theme_bw()

g19 <- ggplot(param_omit_na, aes(x = titas1_100)) + 
  geom_histogram(colour = 1, fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[1]," = 0.8")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(0.5, 1.1)) + theme_bw()

g20 <- ggplot(param_omit_na, aes(x = titas1_500)) + 
  geom_histogram(colour = 1, fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(title = expression(paste("SARIMA ","\u03B8"[1]," = 0.8")), 
       subtitle = "100 observaciones por serie", y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14))+
  geom_vline(xintercept = 0.8, linetype="dashed", color="#78AE6D",lwd=0.8)+
  xlim(c(0.5,1.1)) + theme_bw()



grid.arrange(g17,g19,g18,g20, nrow=2)
