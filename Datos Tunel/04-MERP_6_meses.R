library(forecast)
library(tidyverse)
library(stats)
library(tseries)
library(astsa)
library(cowplot)
library(ggplot2)
library(gridExtra)

### Leo los datos de la serie completa, desde 1975 a 2021
tunel_full <- read.csv("data/tunel_full.csv", header = TRUE)
head(tunel_full)

### Convierto los datos de la columna 4 a serie temporal
tunel <- ts(tunel_full[,4], start = c(1975,1), end = c(2021,12), frequency = 12)
head(tunel)
tail(tunel)

### Usamos el mismo conjunto de datos que usó Melanie
tunel_6 <- window(tunel, start = c(1977, 1),end = c(2016, 6)) 
tunel_x_6 <- window(tunel, start = c(2016, 7),end = c(2016, 12)) # datos secretos

# Con todo el conjunto tunel_m vamos a ajustar con los modelos 
# elegidos y vamos a pronosticar y comparar contra los datos de 2016


# PRONOSTICO A 6 MESES
# VAMOS A PONER A PRUEBA LOS 3 MODELOS SELECCIONADOS 
# CON LOS DIFERENTES PROCEDIMINTOS:

# Por AIC/BIC, modelo 36: (3,0,0)x(0,1,1)_12
# Por CV, modelo 40: (3,0,1)(0,1,1)[12] (window 120, horizonte 12)
# Por train/test, modelo 56: (3,0,1)(1,1,1)[12] 

# Modelo seleccionado por AIC/BIC
mod_AB <- Arima(tunel_6, order = c(3, 0, 0),
                seasonal = list(order = c(0, 1, 1), 
                period = 12),
                include.drift = FALSE)
mod_AB

# Modelo seleccionado por CV
mod_CV <- Arima(tunel_6, order = c(3, 0, 1),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

# Modelo seleccionado por train test
mod_TT <- Arima(tunel_6, order = c(3, 0, 1),
                seasonal = list(order = c(1, 1, 1), period = 12),
                include.drift = FALSE)


# Pronósticando
pron_AB <- forecast(mod_AB, h=6)
pron_CV <- forecast(mod_CV, h=6)
pron_TT <- forecast(mod_TT, h=6)


# Calculando ERP y MERP

ERP_AB_6 <- c()
for (i in 1:6){
  ERP_AB_6[i] <- ((tunel_x_6[i]- pron_AB$mean[i])/tunel_x_6[i])*100
}
MERP_AB_6 <-mean(abs(ERP_AB_6)) # 5.681667
ERP_AB_6
MERP_AB_6

ERP_CV_6 <- c()
for (i in 1:6){
  ERP_CV_6[i] <- ((tunel_x_6[i]-pron_CV$mean[i])/tunel_x_6[i])*100
}
MERP_CV_6 <-mean(abs(ERP_CV_6)) 
ERP_CV_6
MERP_CV_6 # 6.314182


ERP_TT_6 <- c()
for (i in 1:6){
  ERP_TT_6[i] <- ((tunel_x_6[i]-pron_TT$mean[i])/tunel_x_6[i])*100
}
MERP_TT_6 <-mean(abs(ERP_TT_6)) 
ERP_TT_6
MERP_TT_6 # 6.574727

# Grafico ahora los errores de prediccion para los tres modelos

ERP_6 <- data.frame(h = 1:6, 
                     ERP_AB_6 = ERP_AB_6, 
                     ERP_CV_6 = ERP_CV_6,
                     ERP_TT_6 = ERP_TT_6)



tiempos <-  c("Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

ERP_pred_6 <- data.frame("filtro"= rep(1:6,3),
                          "tiempos"=rep(tiempos,3), 
                          "ERP"= c(ERP_AB_6, ERP_CV_6,ERP_TT_6),
                                                  "ID"= c(rep("AB",6),rep("CV",6), rep("TT",6)))

colores <- c("#36C86E", "#0F6B99","#4CC4FF" )
tamanio_letras <- 14 # Ajusta este valor según tu preferencia
espaciado <- 0.6  # Ajusta este valor para controlar el espaciado entre conjuntos
leyenda <- c("M1", "M2","M3")

# Convertir tiempos en un factor ordenado
tiempos <- factor(tiempos, levels = tiempos)

ERP_6m <- ERP_pred_6 %>%
  ggplot(aes(x = tiempos, y = ERP, color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ERP, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(-20, 10)) +
  labs(x = "", y = "ERP", color = "") + 
  ggtitle("", subtitle = "") +
  theme_bw() +
  theme(
    legend.position = c(0.92, 0.2),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras),  # Tamaño de los títulos de los ejes
    legend.text = element_text(size = tamanio_letras),  # Tamaño de las letras de la leyenda
    plot.margin = margin(0.5, 0.5, 0.5, 1, "cm"),
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ERP, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels = c("M1", "M2", "M3")) +
  scale_fill_manual(values = colores, labels = c("M1", "M2", "M3"))+
  scale_x_discrete(limits = tiempos)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


# Grafico con los promedios

MERP_6m <- data.frame("ID" = c(rep("M1",1),rep("M2",1), rep("M3",1)),
                      "prom"=c(MERP_AB_6, MERP_CV_6, MERP_TT_6))

MERP_6m$ID <- as.factor(MERP_6m$ID)

p6m <- MERP_6m %>% 
  ggplot(aes(x = ID, y = prom, fill = factor(ID))) + 
  geom_col(width = 0.2) +
  labs(x = "", y = "MERP") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_fill_manual(values = colores) +
  labs(fill = NULL) +
  theme_bw() +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        plot.margin = margin(1.5, 0.5, 0.5, 0.5, "cm"),  # Ajusta los márgenes aquí
        axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
        axis.title = element_text(size = tamanio_letras))  # Tamaño de los títulos de los ejes



MERP_6m_fig <- grid.arrange(ERP_6m, p6m, nrow=1, widths = c(2.5, 1))

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/merp_6m.png", MERP_6m_fig,dpi = 300, width = 12, height = 4.5, units = "in")


#####################################

tunel_corto <- window(tunel, start = c(2014, 1),end = c(2016, 12))
tamanio_letras <- 14

autoplot(tunel_corto) +
  autolayer(pron_AB,  show.legend = FALSE) +
  autolayer(tunel_corto, color = "black", series = "real") +
  labs(y = expression(paste("caudal (", m^3/seg,")")), x = "Tiempo (años)") +
  theme_bw() +
  theme(
    axis.title = element_text(size = tamanio_letras),      # Tamaño de los títulos de los ejes
    axis.text = element_text(size = tamanio_letras),       # Tamaño de las etiquetas de los ejes
    axis.title.x = element_text(size = tamanio_letras),    # Tamaño del título del eje x
    axis.title.y = element_text(size = tamanio_letras)     # Tamaño del título del eje y
  )



