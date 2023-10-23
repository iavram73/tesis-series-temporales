
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

### Usamos el suguiente conjunto de datos 
tunel_12 <- window(tunel, start = c(1977, 1),end = c(2015, 12)) 
tunel_x_12 <- window(tunel, start = c(2016, 1),end = c(2016, 12)) # datos secretos
#tunel_corto <- window(tunel, start = c(2014, 1),end = c(2016, 12))

# Con todo el conjunto tunel_12 vamos a ajustar con los modelos 
# elegidos y vamos a pronosticar y comparar contra los datos de 2016


# PRONOSTICO A 12 MESES
# VAMOS A PONER A PRUEBA LOS 2 MODELOS SELECCIONADOS 
# CON LOS DIFERENTES PROCEDIMINTOS:

# Por AIC/BIC, modelo 36: (3,0,0)x(0,1,1)_12
# Por CV, modelo 40: (1,0,1)(0,1,1)[12] (window 120, horizonte 12)
# Por train/test, dan mal los residuos, descartamos el modelo,
# igual lo hacemos

# Modelo seleccionado por AIC/BIC
mod_AB <- Arima(tunel_12, order = c(3, 0, 0),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

# Modelo seleccionado por CV
mod_CV <- Arima(tunel_12, order = c(1, 0, 1),
                seasonal = list(order = c(0, 1, 1), period = 12),
                include.drift = FALSE)

# Modelo seleccionado por train test
mod_TT <- Arima(tunel_12, order = c(0, 0, 1),
                seasonal = list(order = c(1, 1, 1), period = 12),
                include.drift = FALSE)



# Pronósticando
pron_AB <- forecast(mod_AB, h=12)
pron_CV <- forecast(mod_CV, h=12)
pron_TT <- forecast(mod_TT, h=12)
# Calculando los errores para cada uno de los 6 tiempos futuros

ERP_AB_12 <- c()
for (i in 1:12){
  ERP_AB_12[i] <- ((tunel_x_12[i]-pron_AB$mean[i])/tunel_x_12[i])*100
}
MERP_AB_12 <-mean(abs(ERP_AB_12)) # 8.695282
ERP_AB_12


ERP_CV_12 <- c()
for (i in 1:12){
  ERP_CV_12[i] <- ((tunel_x_12[i]-pron_CV$mean[i])/tunel_x_12[i])*100
}
MERP_CV_12 <-mean(abs(ERP_CV_12)) # 9.223334
ERP_CV_12

ERP_TT_12 <- c()
for (i in 1:12){
  ERP_TT_12[i] <- ((tunel_x_12[i]-pron_TT$mean[i])/tunel_x_12[i])*100
}
MERP_TT_12 <-mean(abs(ERP_TT_12)) # 13.79848
ERP_TT_12


# Grafico ahora los errores de prediccion para los tres modelos

ERP_12 <- data.frame(h = 1:12, 
                    ERP_AB_12 = ERP_AB_12, 
                    ERP_CV_12 = ERP_CV_12,
                    ERP_TT_12 = ERP_TT_12)

tiempos <-  c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

ERP_pred_12 <- data.frame("filtro"= rep(1:12,3),
                          "tiempos"=rep(tiempos,3), 
                          "ERP"= c(ERP_AB_12, ERP_CV_12, ERP_TT_12),
                          "ID"= c(rep("AB",12),rep("CV",12), rep("TT",12)))
colores <- c("#36C86E", "#8775B8","#F9A5CD" )
tamanio_letras <- 14  # Ajusta este valor según tu preferencia
espaciado <- 0.5  # Ajusta este valor para controlar el espaciado entre conjuntos

# Convertir tiempos en un factor ordenado
tiempos <- factor(tiempos, levels = tiempos)


ERP_12m <- ERP_pred_12 %>% 
  ggplot(aes(x = tiempos, y = ERP, color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ERP, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(-10, 35)) +
  labs(x = "", y = "ERP", color = "") + 
  ggtitle("", subtitle = "") +
  theme_bw() +
  theme(
    legend.position = c(0.92, 0.82),
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
  scale_color_manual(values = colores, labels = c("M1", "M4", "M5")) +
  scale_fill_manual(values = colores, labels = c("M1", "M4", "M5"))+
  scale_x_discrete(limits = tiempos) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")  # Línea horizontal en y = 0# Define el símbolo como un círculo


# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
#ggsave("output/RMSE_12m.png", RMSE_12m,dpi = 300, width = 14, height = 6, units = "in")


# Grafico con los promedios

MERP_12m <- data.frame("ID" = c(rep("M1",1),rep("M4",1), rep("M5",1)),
                      "prom"=c(MERP_AB_12, MERP_CV_12, MERP_TT_12))

MERP_12m$ID <- as.factor(MERP_12m$ID)


p12m <- MERP_12m %>% 
  ggplot(aes(x = ID, y = prom, fill = factor(ID))) + 
  geom_col(width = 0.2) +
  labs(x = "", y = "MERP") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_fill_manual(values = colores) +
  labs(fill = NULL) +
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(1.5, 0.5, 0.5, 0.5, "cm"),  # Ajusta los márgenes aquí
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras))+
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray")  # Tamaño de los títulos de los ejes
    
  


MERP_12m_fig <- grid.arrange(ERP_12m, p12m, nrow = 1,
                              widths = c(3.5, 1) # Ajusta los valores de widths según tus necesidades
                              ) # Misma altura para los gráficos
#heights = unit(1, "null")

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
# ggsave("output/merp_12m.png", MERP_12m_fig,dpi = 300, width = 12, height = 4.5, units = "in")









