#library(forecast)
library(ggplot2)
library(tidyverse)
library(gridExtra)


# Histogramas de phi1, phi2 y phi3 para AR(3)

# Leo los datos guardados en csv
param_ar3 <- read.csv("data/dist_param_ar3.csv", header = TRUE)
colnames(param_ar3)

colores <- c("tomato", "orange","#26A63A", "#1C65A3" )
 

g1 <- ggplot(param_ar3, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A")+
  xlim(c(0.3, 1.1)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# expression(paste("\u03d5"[1]," = 0.7")),
  
g2 <- ggplot(param_ar3, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#26A63A")+
  xlim(c(0.3, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[1]," = 0.7")), 

g3 <- ggplot(param_ar3, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[2]," = 0.2")),

g4 <- ggplot(param_ar3, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 19, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#26A63A")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[2]," = 0.2")),

g5 <- ggplot(param_ar3, aes(x = phis3_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[3]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4.5))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[3]," = -0.3")),

g6 <- ggplot(param_ar3, aes(x = phis3_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[3]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[3]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#26A63A")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# title = expression(paste("\u03d5"[3]," = -0.3")),

dist_param_ar3 <-grid.arrange(g1,g3,g5,g2,g4,g6,ncol=3,
                              widths = c(1, 1, 1))  # Ajusta los anchos de las columnas para aumentar el espacio

#library(patchwork)
#g1+g3+g5+g2+g4+g6 + plot_annotation(tag_levels = "A")


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/dist-param-ar3-b.png", dist_param_ar3,dpi = 300, width = 8, height = 4, units = "in")





############################################################

# Histogramas de titas para MA(3)

param_ma3 <- read.csv("data/dist_param_ma3.csv", header = TRUE)


g7 <- ggplot(param_ma3, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "Densidad",
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
       y= "Densidad", x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(0.3, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g9 <- ggplot(param_ma3, aes(x = titas2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[2]," - ","100 observaciones")),  
       y= "Densidad",x= expression("\u03B8"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g10 <- ggplot(param_ma3, aes(x = titas2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 19, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[2]," - ","500 observaciones")),
       x= expression("\u03B8"[2]),y= "Densidad" )+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g11 <- ggplot(param_ma3, aes(x = titas3_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[3]," - ","100 observaciones")),
       x= expression("\u03B8"[3]), y= "Densidad")+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g12 <- ggplot(param_ma3, aes(x = titas3_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[3]," - ","500 observaciones")),
       x= expression("\u03B8"[3]), y= "Densidad")+
  scale_y_continuous(expand = c(0, 0), limits = c(0,10))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.65, 0.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dist_param_ma3 <-grid.arrange(g7,g9,g11,g8,g10,g12,nrow=2) # Tamaño figura 700 x 500


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/dist-param-ma3.png", dist_param_ma3,dpi = 300, width = 8, height = 4, units = "in")




#####################################################################
# Histogramas de parám para ARMA (2,1)

param_arma21 <- read.csv("data/dist_param_arma21.csv", header = TRUE)
View(param_arma21)

g13 <- ggplot(param_arma21, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2, 1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g14 <- ggplot(param_arma21, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2, 1.3)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g15 <- ggplot(param_arma21, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g16 <- ggplot(param_arma21, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.3, 0.6)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g17 <- ggplot(param_arma21, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "Densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.1, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


g18 <- ggplot(param_arma21, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 18, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","500 observaciones")),  y= "Densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.1, 1.1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_param_arma21 <-grid.arrange(g13,g15,g17,g14,g16,g18,nrow=2)


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/dist-param-arma21.png", dist_param_arma21,dpi = 300, width = 8, height = 4, units = "in")

#################################################
# ARIMA

# Leo los datos c=guardados en csv
param_arima211 <- read.csv("data/dist_param_arima211.csv", header = TRUE)
dim(param_arima211)

# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 100
g1 <- ggplot(param_arima211, aes(x = phis1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Histograma de distribucion de phis1 en ARIMA(2,1,1) con t_final 500
g2 <- ggplot(param_arima211, aes(x = phis1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = 0.7, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.2,1.4)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid.arrange(g1,g2) # Guardo grafico tamaño 600 x 500

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 100

g3 <- ggplot(param_arima211, aes(x = phis2_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,2.5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Histograma de distribucion de phis2 en ARIMA(2,1,1) con t_final 500

g4 <- ggplot(param_arima211, aes(x = phis2_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03d5"[2]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03d5"[2]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,5))+
  geom_vline(xintercept = 0.2, linetype="dashed", color="#78AE6D")+
  xlim(c(-0.7,1)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grid.arrange(g3,g4)


# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 100

g5 <- ggplot(param_arima211, aes(x = titas1_100)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","100 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,1.8))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Histograma de distribucion de titas1 en ARIMA(2,1,1) con t_final 500

g6 <- ggplot(param_arima211, aes(x = titas1_500)) + 
  geom_histogram(colour = "darkgray", fill = "white", bins = 20, aes(y = ..density..))+
  geom_density(lwd = 0.5,linetype = 1, colour = "#1C65A3")+
  labs(subtitle = expression(paste("\u03B8"[1]," - ","500 observaciones")),  y= "densidad",
       x= expression("\u03B8"[1]))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,4))+
  geom_vline(xintercept = -0.3, linetype="dashed", color="#78AE6D")+
  xlim(c(-1.2,1.2)) + theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



dist_param_arima21 <-grid.arrange(g1,g3,g5,g2,g4,g6, nrow=2)


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/dist-param-arima211.png", dist_param_arima21,dpi = 300, width = 8, height = 4, units = "in")


#################################################

# SARIMA

param <- read.csv("data/dist_param_sarima_111_101_12.csv", header = TRUE)
colnames(param)
param_omit_na <- na.omit(param)
View(param_omit_na)
dim(param_omit_na) # 1000   11

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

ggsave("output/dist_param_sarima_reg.png", dist_param_sarima_reg,dpi = 300, 
       width = 7, height =4.5, units = "in")

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

ggsave("output/dist_param_sarima_est.png", dist_param_sarima_est,dpi = 300, 
       width = 7, height =4.5, units = "in")

