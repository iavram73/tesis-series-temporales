library(tidyverse)
library(ggthemes)
library(readr)
library(ggplot2)
library(gridExtra)
library(patchwork)

# Explorar, bajo el modelo normal, cual es la convergencia del phi 1 
# ajustado con AR(1), cuando los datos fueron generados por un AR(3).

df_phis_conv_ar <- read.csv("data/phis_conv_ar.csv",  header = TRUE)
colnames(df_phis_conv_ar)

ga <- ggplot(df_phis_conv_ar, aes(x = phi_1_AR1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "",  y= "densidad", x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.58, 0.8)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/conv-phi1-fuera-mod.png", ga,
       dpi = 300, width = 5, height = 3, units = "in")
  

# title = expression(paste("Histograma de ","\u03d5"[1]," ajustado con AR(1). Porceso real AR(3).")),

# Agrego los dats de MA para ponerlos en un mismo gráfico
titas_conv_ma <- read.csv("data/titas_conv_ma.csv",  header = TRUE)
colnames(titas_conv_ma)

gb <- ggplot(titas_conv_ma, aes(x = titas1_ma1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.35,0.72)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/conv-tita1-fuera-mod.png", gb,
       dpi = 300, width = 5, height = 3, units = "in")

#conv_phi1_tita1_fuera_mod <- grid.arrange(ga,gb, nrow=1)

#ggsave("output/conv_phi1_tita1_fuera_mod.png", 
#       conv_phi1_tita1_fuera_mod,dpi = 300, width = 6, height = 2.5, units = "in")


# ga+gb + plot_annotation(tag_levels = "a")



####################################################################
# Explorar, bajo el modelo normal, cual es la convergencia del phi 1 y 2
# ajustado con AR(2) cuando los datos fueron generados por un AR(3).

# grafico para phis1 y phis2 AR(2)
g1 <- ggplot(df_phis_conv_ar, aes(x = phi_1_AR2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad",x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.4, 0.9)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/conv-phi1-fuera-mod-ar2.png", g1,
       dpi = 300, width = 5, height = 3, units = "in")

#title = expression(paste("Histograma de ","\u03C6"[1]," ajustado con AR(2). Porceso real AR(3)."))

g2 <- ggplot(df_phis_conv_ar, aes(x = phi_2_AR2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 2, colour = "tomato")+
  labs(subtitle = "", y= "densidad",x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(-0.3, 0.3)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# title = expression(paste("Histograma de ","\u03C6"[2]," ajustado con AR(2). Porceso real AR(3).")),

ggsave("output/conv-phi2-fuera-mod-ar2.png", g2,
       dpi = 300, width = 5, height = 3, units = "in")

grid.arrange(g1,g2, nrow=1)


# Agrego los datos de MA

# grafico para titas 1 y 2 
g3 <- ggplot(titas_conv_ma, aes(x = titas1_ma2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.5, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/conv-tita1-fuera-mod-ma2.png", g3,
       dpi = 300, width = 5, height = 3, units = "in")

# subtitle = expression(paste("Histograma de ","\u03B8"[1]," ajustado con MA(2). Proceso real MA(3)."))


g4 <- ggplot(titas_conv_ma, aes(x = titas2_ma2)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 2, colour = "tomato")+
  labs(title = "", y= "densidad", x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.1, 0.8)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave("output/conv-tita2-fuera-mod-ma2.png", g4,
       dpi = 300, width = 5, height = 3, units = "in")


# subtitle = expression(paste("Histograma de ","\u03B8"[2]," ajustado con MA(2). Proceso real MA(3)."))

#conv_phis12_titas12_fuera_mod <- grid.arrange(g1,g2,g3,g4,nrow=2)
#ggsave("output/conv-phis12-titas12-fuera-mod.png", conv_phis12_titas12_fuera_mod,dpi = 300, width = 7, height =4.5, units = "in")




#######################################################################

dist_param_arma11 <- read.csv("data/dist_param_arma11_fuera_mod.csv",  header = TRUE)
colnames(dist_param_arma11)

g5 <- ggplot(dist_param_arma11, aes(x = phis1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad",x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g6 <- ggplot(dist_param_arma11, aes(x = titas1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(-0.8, -0.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#grid.arrange(g5,g6,nrow=1)

ggsave("output/conv-phi1-fuera-mod-arma.png", g5,
       dpi = 300, width = 5, height = 3, units = "in")
ggsave("output/conv-tita1-fuera-mod-arma.png", g6,
       dpi = 300, width = 5, height = 3, units = "in")

#######################################################################
# ARIMA

dist_param <- read.csv("data/dist_param_arima111.csv",  header = TRUE)
colnames(dist_param)


g1 <- ggplot(dist_param, aes(x = phis1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(0.65,1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g2 <- ggplot(dist_param, aes(x = titas1)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 22, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "tomato")+
  labs(subtitle = "", y= "densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A",lwd=0.8)+
  xlim(c(-0.7,-0.25)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/conv-phi1-fuera-mod-arima.png", g1,
       dpi = 300, width = 5, height = 3, units = "in")
ggsave("output/conv-tita1-fuera-mod-arima.png", g2,
       dpi = 300, width = 5, height = 3, units = "in")




#######################################################################
# SARIMA

dist_param <- read.csv("data/dist_param_sarima_101_101_12_fuera_mod.csv",  header = TRUE)

dist_param_na_omit <- na.omit(dist_param)
dim(dist_param_na_omit) # 765   6
colnames(dist_param_na_omit)
View(dist_param_na_omit)

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

ggsave("output/dist-sarima-fuera-mod-reg.png", 
       dist_sarima_fuera_mod_reg,dpi = 300, width = 6, height = 2.5, units = "in")

ggsave("output/dist-sarima-fuera-mod-est.png", 
       dist_sarima_fuera_mod_est,dpi = 300, width = 6, height = 2.5, units = "in")

