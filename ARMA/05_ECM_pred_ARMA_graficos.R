
library(ggplot2)
library(tidyverse)
library(gridExtra)


ECM_pred_arma <- read.csv("data/ECM_pred_arma.csv",  header = TRUE)

ECM_pred_arma$tiempos <- factor(ECM_pred_arma$tiempos, levels = c(paste0("t", 1:9), "t10"))

leyenda_3 <- c("ARMA(0,1)", "ARMA(1,1)","ARMA(2,1)")
colores <- c("tomato", "orange","#26A63A" )
tamanio_letras <- 14 
espaciado <- 0.5

ECM_pred_arma %>% 
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
