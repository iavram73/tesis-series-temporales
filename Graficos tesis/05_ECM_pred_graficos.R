library(ggplot2)
library(tidyverse)
library(gridExtra)
library(dplyr)

# Quiero comparar el ECM de las predicciones cuando las series son ajustadas
# con el modelo con el cual fueron generadas (AR(3)) y con dos modelos 
# incorrectos (AR(1) y AR(2)).

colores <- c("tomato", "orange","#26A63A" )
tamanio_letras <- 14 
espaciado <- 0.5  # controlar el espaciado entre conjuntos
leyenda_1 <- c("AR(1)", "AR(2)","AR(3)")

# Proceso AR
ECM_pred <- read.csv("data/ECM_pred.csv",  header = TRUE)
head(ECM_pred)

g1 <- ECM_pred %>%
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 3)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero AR(3)") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras),  # Tamaño de los títulos de los ejes
    legend.text = element_text(size = tamanio_letras),  # Tamaño de las letras de la leyenda
    plot.margin = margin(0.5, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels=leyenda_1) +
  scale_fill_manual(values = colores)


# Proceso MA
ECM_pred_ma <- read.csv("data/ECM_pred_ma.csv",  header = TRUE)

leyenda_2 <- c("MA(1)", "MA(2)","MA(3)")

g2 <- ECM_pred_ma %>% ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 2)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero MA(3)") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  # Tamaño de las letras de los ejes
    axis.title = element_text(size = tamanio_letras),  # Tamaño de los títulos de los ejes
    legend.text = element_text(size = tamanio_letras),  # Tamaño de las letras de la leyenda
    plot.margin = margin(0.1, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels=leyenda_2) +
  scale_fill_manual(values = colores)


ECM_pred_ar_ma <- grid.arrange(g1,g2,nrow=2)
# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/ECM-pred-ar-ma.png", ECM_pred_ar_ma,dpi = 300, width = 10, height = 6, units = "in")


# Proceso ARMA
ECM_pred_arma <- read.csv("data/ECM_pred_arma.csv",  header = TRUE)

# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_arma$tiempos <- factor(ECM_pred_arma$tiempos, levels = c(paste0("t", 1:9), "t10"))

leyenda_3 <- c("ARMA(0,1)", "ARMA(1,1)","ARMA(2,1)")

g3 <- ECM_pred_arma %>% 
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 3)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero ARMA(2,1)") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  
    axis.title = element_text(size = tamanio_letras), 
    legend.text = element_text(size = tamanio_letras),
    plot.margin = margin(0.1, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores, labels=leyenda_3) +
  scale_fill_manual(values = colores)

ECM_pred_arma <- g3

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/ECM_pred_arma.png", ECM_pred_arma,dpi = 300, width = 10, height = 4, units = "in")

###############################################################################
# Proceso ARIMA
ECM_pred_ARIMA <- read.csv("data/ECM_pred_ARIMA.csv",  header = TRUE)

# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_ARIMA$tiempos <- factor(ECM_pred_ARIMA$tiempos, levels = c(paste0("t", 1:9), "t10"))

leyenda_4 <- c("ARIMA(1,1,1)", "ARIMA(2,0,1)","ARIMA(2,1,1)")
colores_2 <- c("orange", "tomato","#26A63A" )

g4 <- ECM_pred_ARIMA %>% 
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 150)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero ARIMA(2,1,1)") +
  theme_bw() +
  theme(legend.position = c(0.01, 0.98),  # Cambia la posición de la leyenda
        legend.justification = c(0, 1),   # Alinea la leyenda a la izquierda y arriba
        legend.box.margin = margin(0, 0, 0, 0),  # Ajusta los márgenes de la leyenda
       panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  
    axis.title = element_text(size = tamanio_letras), 
    legend.text = element_text(size = tamanio_letras),
    plot.margin = margin(0.1, 0.5, 0.5, 1, "cm"),
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores_2, labels=leyenda_4) +
  scale_fill_manual(values = colores_2)

ECM_pred_arima <- g4

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/ECM_pred_arima.png", ECM_pred_arima,dpi = 300, width = 10, height = 4, units = "in")


################################################################################
# SARIMA_b
ECM_pred_sarima_b <- read.csv("data/ECM_pred_sarima_b.csv",  header = TRUE)

# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_sarima_b$tiempos <- factor(ECM_pred_sarima_b$tiempos, levels = c(paste0("t", 1:9), "t10"))

leyenda_5 <- c("SARIMA(1,1,1)(1,0,1)x12", 
               "SARIMA(1,0,1)(1,0,1)x12",
               "SARIMA(1,1,1)(0,0,1)x12")

colores_2 <- c("#26A63A", "orange","tomato")
tamanio_letras <- 14 
espaciado <- 0.5  # controlar el espaciado entre conjuntos


g5 <- ECM_pred_sarima_b %>% 
  ggplot(aes(x = tiempos, y = ECM , color = ID)) +
  geom_point(size = 2, shape = 21, fill = "lightgray", 
             position = position_dodge(width = espaciado)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = espaciado), size = 0.2) +
  scale_y_continuous(expand = expansion(0), limits = c(0, 300)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM", color = "") + 
  ggtitle("", subtitle = "Modelo verdadero SARIMA(1,1,1)(1,0,1)x12") +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.98),  # Cambia la posición de la leyenda
    legend.justification = c(0, 1),   # Alinea la leyenda a la izquierda y arriba
    legend.box.margin = margin(0, 0, 0, 0),  # Ajusta los márgenes de la leyenda
    panel.grid.major = element_blank(),
    axis.text = element_text(size = tamanio_letras),  
    axis.title = element_text(size = tamanio_letras), 
    legend.text = element_text(size = tamanio_letras),
    plot.margin = margin(0.1, 0.5, 0.5, 0.5, "cm"),  # Ajusta los márgenes del gráfico
    plot.subtitle = element_text(size = tamanio_letras)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.2))) +
  geom_point(size = 2, shape = 21, position = position_dodge(width = espaciado), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colores_2, labels=leyenda_5) +
  scale_fill_manual(values = colores_2)


ECM_pred_sarima <- g5

# Guarda el gráfico en formato PNG con alta resolución (300 dpi)
ggsave("output/ECM_pred_sarima.png", ECM_pred_sarima,dpi = 300, width = 10, height = 4, units = "in")




################################################################################
# SARIMA
ECM_pred_sarima <- read.csv("data/ECM_pred_sarima.csv",  header = TRUE)

# Convertir "tiempos" en un factor y reordenar los niveles
ECM_pred_sarima$tiempos <- factor(ECM_pred_sarima$tiempos, levels = c(paste0("t", 1:9), "t10"))

ECM_pred_sarima %>% 
  ggplot(aes(x = tiempos, y = ECM, color = ID)) + 
  geom_point(size = 3, shape = 21, fill = "lightgray", position = position_dodge(width = 0.7)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), position = position_dodge(width = 0.7), size = 0.5) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = 0.7), 
                 size = ifelse(ECM_pred_sarima$ID == "AR(3)", 1, 0.5)) + # aumentar tamaño de línea para AR(3)
  scale_y_continuous(expand = expansion(0), limits = c(0,1500)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM de las predicciones", color="",
       subtitle = "Modelo verdadero (212)") + 
  theme_bw() +
  theme(legend.position = "right") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
  guides(color = guide_legend(override.aes = list(size=1.2))) +
  geom_point(size = 3, shape = 21, position = position_dodge(width = 0.7), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off")










##############

g1 <- ECM_pred %>% 
  ggplot(aes(x = tiempos, y = ECM, color = ID)) + 
  geom_point(size = 3, shape = 21, fill = "lightgray", position = position_dodge(width = 0.7)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), position = position_dodge(width = 0.7), size = 0.5) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = 0.7), 
                 size = ifelse(ECM_pred$ID == "AR(3)", 1, 0.5)) + # aumentar tamaño de línea para AR(3)
  scale_y_continuous(expand = expansion(0), limits = c(0,2.5)) +
  scale_x_discrete(limits = c(paste0("t", 1:9), "t10")) +
  labs(x = "tiempos futuros", y = "ECM de las predicciones", color="",
       subtitle = "Modelo verdadero AR(3). Modelos incorrectos AR(2) y AR(1)") + 
  theme_bw() +
  theme(legend.position = "right") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 1, "cm")) +
  guides(color = guide_legend(override.aes = list(size=1.2))) +
  geom_point(size = 3, shape = 21, position = position_dodge(width = 0.7), 
             aes(y = ECM, color = ID, fill = ID), show.legend = FALSE, stroke = 1.2) +
  coord_cartesian(clip = "off")