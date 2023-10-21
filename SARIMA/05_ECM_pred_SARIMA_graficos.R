library(ggplot2)
library(tidyverse)
library(gridExtra)

# Quiero comparar el ECM de las predicciones cuando las series son ajustadas
# con el modelo con el cual fueron generadas con modelos 
# incorrectos

# Leo los datos
sarima_series <- read.csv("data/muestra_sarima_212_202_12.csv")
sarima_series <- sarima_series[,101:600] # Saco los primeros 100 datos de cada serie
# para asegurar estabilidad

# Hago un df con el conjunto de test
test <- data.frame(sarima_series[,491:500])  
dim(test) 

# Quiero calcular el ECM de la prediccion a t1. Para eso hago el cuadrado 
# de la diferencia entre el valor predicho y el observado a t1 
# para las 1000 series, y saco el promedio. Repito para las predicciones
# t2 a t10.


# Para el modelo correcto pred_sarima_212_202_12
df1 <- read.csv("data/df_ECM_pred_sarima_212_202_12.csv",  header = TRUE)

dim(df1)

ECM_212<- c()
for (i in 1:10){
  ECM_212[i] <- mean((df1[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_212 

###############################################################################

# Para el modelo incorrecto sarima_202_202_12

df2 <- read.csv("data/df_ECM_pred_sarima_202_202_12_fuera_mod.csv",  header = TRUE)

ECM_202 <- c()
for (i in 1:10){
  ECM_202[i] <- mean((df2[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_202


###############################################################################

# Para el modelo incorrecto sarima_010_202_12

df3 <- read.csv("data/df_ECM_pred_sarima_010_202_12_fuera_mod.csv",  header = TRUE)

ECM_010 <- c()
for (i in 1:10){
  ECM_010[i] <- mean((df3[,i] - test[,i])^2, na.rm=TRUE)
}

ECM_010

###########################################################################

# Junto en un df los ECM para t1 a t1 calculados para los tres modelos

tiempos <-  c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")

ECM_pred <- data.frame("filtro"= c(rep(1:10,3)),
                       "tiempos"=c(rep(tiempos,3)), 
                       "ECM"= c(ECM_212, ECM_202,ECM_010),
                       "ID"= c(rep("(212)",10),rep("(202)",10), rep("(010)",10)))


write.csv(ECM_pred, file = "data/ECM_pred_sarima.csv", row.names = FALSE)

ECM_pred %>% 
  ggplot(aes(x = tiempos, y = ECM, color = ID)) + 
  geom_point(size = 3, shape = 21, fill = "lightgray", position = position_dodge(width = 0.7)) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), position = position_dodge(width = 0.7), size = 0.5) +
  geom_linerange(aes(ymin = 0, ymax = ECM, color = ID), 
                 position = position_dodge(width = 0.7), 
                 size = ifelse(ECM_pred$ID == "AR(3)", 1, 0.5)) + # aumentar tamaño de línea para AR(3)
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

###
ECM_pred_sarima <- read.csv("data/ECM_pred_sarima.csv")



