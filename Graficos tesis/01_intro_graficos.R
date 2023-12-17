library(tidyverse)
library(lubridate)
library(car)
library(tseries)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(gridExtra)
library(readr)
library(grid)
library(ggthemes)

# Figura para introducción

autoplot(a10,  col="tomato") +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +  xlab("Year") +
  theme_bw()

aelec <- window(elec, start=1980)

g <- autoplot(aelec, col = "#1C5F9E") +
  labs(y = "Consumo eléctrico GW/h", x = "tiempo (años)") +
  ggtitle("", subtitle = "") + # Caudal medio mensual
  theme_bw() +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18))


ggsave("output/elect_aust.png", g, dpi = 300, width = 10, height = 4.5, units = "in")





# Conceptos fundamntales- figuras de ejemplo

# Ejemplo 1: tendencia
# Total international visitors to Australia (in millions). 1980-2015.

data(austa)	
g1 <- autoplot(austa, col="tomato")+
  labs(y = "millones", x= "tiempo (años)")+
  ggtitle(label = "a) Visitantes extranjeros a Australia", 
          subtitle = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))

# Ejemplo 2: tendencia y estacionaldad 
data(co2)
# Conc. atmosférica en ppm de CO2
g2 <- autoplot(co2, col="#497AA7")+
  labs(y = expression("ppm de CO"[2]), x= "tiempo (años)")+
  ggtitle(label = "b) Concentración de dióxido de carbono", 
          subtitle = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))


# Ejemplo 3: Sales of new one-family houses in the USA since 1973.
g3 <- autoplot(hsales, col="#358747") +
  labs(y = "millones", x = "tiempo (años)") +
  ggtitle(label = "c) Ventas de casas unifamiliares nuevas en USA", subtitle = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))



# Ejemplo 4 ppm CO estacion centenario CABA

aire_2019 <- read.csv("data/calidad-de-aire-2019.csv")
View(aire_2019)

# Reviso y acomodo el data set 

aire_2019$co_centenario[aire_2019$co_centenario=="s/d"] <- NA
aire_2019$no2_centenario[aire_2019$no2_centenario=="s/d"] <- NA
aire_2019$pm10_centenario[aire_2019$pm10_centenario=="s/d"] <- NA

aire_2019$co_cordoba[aire_2019$co_cordoba=="s/d"] <- NA
aire_2019$no2_cordoba[aire_2019$no2_cordoba=="s/d"] <- NA
aire_2019$pm10_cordoba[aire_2019$pm10_cordoba=="s/d"] <- NA

aire_2019$co_la_boca[aire_2019$co_la_boca=="s/d"] <- NA
aire_2019$no2_la_boca[aire_2019$no2_la_boca=="s/d"] <- NA
aire_2019$pm10_la_boca[aire_2019$pm10_la_boca=="s/d"] <- NA
  
  
aire_2019$co_centenario <- as.numeric(aire_2019$co_centenario)
aire_2019$no2_centenario <- as.numeric(aire_2019$no2_centenario)
aire_2019$pm10_centenario <- as.numeric(aire_2019$pm10_centenario)

aire_2019$co_cordoba <- as.numeric(aire_2019$co_cordoba)
aire_2019$no2_cordoba <- as.numeric(aire_2019$no2_cordoba)
aire_2019$pm10_cordoba <- as.numeric(aire_2019$pm10_cordoba)

aire_2019$co_la_boca <- as.numeric(aire_2019$co_la_boca)
aire_2019$no2_la_boca <- as.numeric(aire_2019$no2_la_boca)
aire_2019$pm10_la_boca <- as.numeric(aire_2019$pm10_la_boca)

aire_2019$fecha <- as.Date(aire_2019$fecha,"%m/%d/%Y")

prom_dia_CO <- aire_2019 %>% na.omit() %>% group_by(fecha) %>% 
  summarise(prom_co_cent = mean(co_centenario),
            prom_co_boca= mean(co_la_boca), 
            prom_co_cordoba= mean(co_cordoba)) %>% ungroup() 




g4 <- ggplot(data = prom_dia_CO)+ 
  geom_line(aes(x=fecha,y=prom_co_cordoba), color="purple")+
  ggtitle(label = "d) Concentración de monóxido de carbono", 
          subtitle = NULL)+
  theme_bw()+ 
  labs(x="mes del año 2019",y= "ppm de CO")+ 
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))

ejemplos <- grid.arrange(g1,g2,g3,g4)

ggsave("output/ejemplos_series.png", ejemplos, dpi = 300, width = 14, height = 8, units = "in")



autoplot(diff(ts(goog[1:1000])), col="#835581")+
  labs(y = "cambio en precio", x= "día")+
  ggtitle(label = "d) Cambio diario en el precio de cierre de las acciones de Google", 
          subtitle = NULL) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18))





autoplot(diff(ts(goog200)), col="#835581")+
  labs(y = "cambio en precio", x= "día")+
  ggtitle("", subtitle = "d) Cambio diario en el precio de cierre de las acciones de Google")+ 
  theme_bw()


# Otros:

data(UKDriverDeaths)
autoplot(UKDriverDeaths)+
  labs(y = "", x= "tiempo (años)")+
  theme_bw()

data(EuStockMarkets)
autoplot(EuStockMarkets)+
  labs(y = "", x= "tiempo (años)")+
  theme_bw()

# Sales of new one-family houses in the USA (Jan 1987 – Nov 1995).
autoplot(hsales2, col="tomato")+
  labs(y = "Ventas de casas unifamiliares nuevas en USA", 
       x= "tiempo (años)")+
  theme_bw()


# Ver: Forecasting: Principles and Practice (Hyndman)

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")



autoplot(goog)


######################################################################
# Ruido Blanco (RB)

set.seed(123)
n = 100
rb <- ts(rnorm(100, 0, 1))

g_rb <- autoplot(rb, col = "#1C5F9E") +
  labs(y = "Valor de las observaciones", x = "Tiempo (años)", title = "a)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18))


a1 <- ggAcf(rb,lag.max = 20)+ labs(title = "b)")+ 
  labs(title = "b)", x = "Retardo")+
  theme_bw()+
      geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), 
                   linetype = "dashed", color = "black")+
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 18)) 







# p1 <- ggPacf(rb,lag.max = 20)+ labs(title = "b)", x="retardo")+ theme_bw()

rb_acf <- grid.arrange(g_rb,a1,nrow=1) # Nombre: acf_ar1

ggsave("output/rb_acf.png", rb_acf, dpi = 300, width = 10, height = 3.5, units = "in")


######################################################################
# Proceso AR (1) 
# tamanio grafico 520x340

n = 200
sd = 1
set.seed(71)
ar_1a <- arima.sim(n = n, model = list(ar = 0.7),sd = sd)
ar_1b <- arima.sim(n = n, model = list(ar = -0.7),sd = sd)

p1 <- autoplot(ar_1a, color="#1C5F9E") +
  labs(y = "Valor de las observaciones", x = "Tiempo (escala arbitraria)",
       title = expression(paste(italic("\u03D5")[1], " = 0.7"))) +
  theme_bw()

p2 <- autoplot(ar_1b, color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)",
       title = expression(paste(italic("\u03D5")[1]," = -0.7")))+
  theme_bw()

a1 <- ggAcf(ar_1a,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1]," = 0.7")),
      x="Retardo")+ theme_bw()
a2 <- ggAcf(ar_1b,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1]," = -0.7")),
  x="Retardo")+ theme_bw()

pa1 <- ggPacf(ar_1a,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1]," = 0.7")),
       x="Retardo")+ theme_bw()
pa2 <- ggPacf(ar_1b,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1]," = 0.7")),
       x="Retardo")+ theme_bw()

grid.arrange(p1,a1,nrow=1) # Nombre: acf_ar1_a
grid.arrange(p2,a2,nrow=1) # Nombre: acf_ar1_b


acf_ar1_nueva <- grid.arrange(p1,a1,p2,a2,nrow=2)
ggsave("output/acf_ar1_nueva.png", acf_ar1_nueva, dpi = 300, width = 10, height = 6, units = "in")



# A mano

mu = 0
sigma=1
t_final = 100
y = rep(NA,t_final)
y[1] = 0
phi = 0.7
set.seed(123)
for(i in 2:t_final){
  a = rnorm(1,mu,sigma)
  y[i] = phi*y[i-1]+ a
}

y_ts <- ts(y)
ggAcf(y_ts)+ labs(title = "", x="retardo")+ theme_bw()
ggPacf(y_ts)+ labs(title = "a) Proceso AR(1)")+ theme_bw()


#####################################################################
# Proceso AR (2) 

n = 200
sd = 1
set.seed(71)
ar_2a <- arima.sim(n = n, model = list(ar = c(0.5, 0.3)),sd = sd)
ar_2b <- arima.sim(n = n, model = list(ar = c(-0.5, 0.3)),sd = sd)

p3 <- autoplot(ar_2a, color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)",
       title = expression(paste(italic("\u03D5")[1], " = 0.5, ", italic("\u03D5")[2], " = 0.3" ))) +
  theme_bw()
p4 <- autoplot(ar_2b, color="#")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)",
       title = expression(paste(italic("\u03D5")[1], " = -0.5, ",italic("\u03D5")[2], " = 0.3" )))+
  theme_bw()


a3 <- ggAcf(ar_2a,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1], " = 0.5, ",italic("\u03D5")[2], " = 0.3")), 
  x="Retardo")+ theme_bw()
a4 <- ggAcf(ar_2b,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1], " = -0.5, ",italic("\u03D5")[2], " = 0.3")), 
  x="Retardo")+ theme_bw()

pa3 <- ggPacf(ar_2a,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1], " = 0.5, ",italic("\u03D5")[2], " = 0.3")), 
       x="Retardo")+ theme_bw()
pa4 <- ggPacf(ar_2b,lag.max = 20)+ 
  labs(title = expression(paste(italic("\u03D5")[1], " = -0.5, ",italic("\u03D5")[2], " = 0.3")), 
       x="Retardo")+ theme_bw()


acf_ar2_nueva <- grid.arrange(p3,a3,p4,a4,nrow=2) 
ggsave("output/acf_ar2_nueva.png", acf_ar2_nueva, dpi = 300, width = 10, height = 6, units = "in")


acf_pacf_ar1_ar2 <- grid.arrange(a1,pa1,a3,pa3,nrow=2) 
ggsave("output/acf_pacf_ar1_ar2.png", acf_pacf_ar1_ar2, dpi = 300, width = 10, height = 6, units = "in")




# A mano
mu = 0
sigma=1
t_final = 100
z = rep(NA,t_final)
z[1] = 0
z[2] = 0
phi_1 = 0.5
phi_2 = 0.3
set.seed(12)
for(i in 3:t_final){
  a = rnorm(1,mu,sigma)
  z[i] = phi_1*z[i-1]+ phi_2*z[i-2]+ a
}


z_ts <- ts(z)
autoplot(y_ts, color="darkblue")+
  labs(y = "valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()
ggAcf(z_ts)+ labs(title = "", x="retardo")+ theme_bw()


p1 <- ggPacf(y_ts)+ labs(x = "a) retardo - Proceso AR(1)", title = "")+ theme_bw()
p2 <- ggPacf(z_ts)+ labs(x = "b) retardo - Proceso AR(2)", title = "")+ theme_bw()

# Tamaño 760x310
grid.arrange(p1, p2, nrow=1)

##################################################################
# MA (1)
set.seed(24)
ma_1a <- arima.sim(n = n, model = list(ma = 0.7),sd = sd)
ma_1b <- arima.sim(n = n, model = list(ma = -0.7),sd = sd)

autoplot(ma_1a, color="darkblue")+ # fma_1
  labs(y = "valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()

a5 <- ggAcf(ma_1a,lag.max = 10)+ 
  labs(x="Retardo",title = expression(paste("\u03B8"[1], " = 0.7")))+ 
  theme_bw()

a6 <- ggAcf(ma_1b,lag.max = 10)+ labs(title = "b)", x="retardo")+ theme_bw()

pa5 <- ggPacf(ma_1a,lag.max = 20)+ 
  labs(x= "Retardo", 
       title = expression(paste("\u03B8"[1], " = 0.7")))+
         theme_bw()

acf_pacf_ma1 <- grid.arrange(a5, pa5, nrow=1)

ggsave("output/acf_pacf_ma1.png", acf_pacf_ma1, dpi = 300, width = 10, height = 3.5, units = "in")



# MA (2)
set.seed(170)
ma_2a <- arima.sim(n = n, model = list(ma = c(0.8, -0.4)),sd = sd)
ma_2b <- arima.sim(n = n, model = list(ma = c(-0.5, -0.2)),sd = sd)
autoplot(ma_2a, color="darkblue")+ # fma_2
  labs(y = "valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()

a7 <- ggAcf(ma_2a,lag.max = 10)+ 
  labs(title = expression(paste("\u03B8"[1], " = 0.8, ","\u03B8"[2], " = -0.4"),
                          x="retardo"))+ theme_bw()
pa6 <- ggPacf(ma_2a,lag.max = 20)+ 
  labs(title = expression(paste("\u03B8"[1], " = 0.8, ","\u03B8"[2], " = -0.4"),
                          x="retardo"))+ theme_bw()
#grid.arrange(a7, pa6, nrow=1)

grid.arrange(a5, pa5,a7, pa6, nrow=2)


a8 <- ggAcf(ma_2b,lag.max = 10)+ labs(title = "b)", x="retardo")+ theme_bw()
grid.arrange(a7, a8, nrow=1)


p5 <- ggPacf(ma_1a,lag.max = 10)+ labs(title = "a)", x="retardo")+ theme_bw()
p6 <- ggPacf(ma_2a,lag.max = 10)+ labs(title = "a)", x="retardo")+ theme_bw()
grid.arrange(p5, p6, nrow=1)


# MA (1)
mu = 0
sigma=1
t_final = 100
m1 = rep(NA,t_final)
#aes = rep(NA,t_final)
m1[1] = 0
aes[1] = 0
tita = 0.7
set.seed(12)
aAnt = 0
for(i in 2:t_final){
  a = rnorm(1,mu,sigma)
  m1[i] = tita*aAnt + a
  #aes[i] =  a
  aAnt =  a
}

m1_ts <- ts(m1)

###################################################################
# MA (2)

mu = 0
sigma=1
t_final = 100
m2 = rep(NA,t_final)
m2[1] = 0
tita_1 = 0.5
tita_2 = 0.2
set.seed(456)
aAnt_1 = 0
aAnt_2 = 0
for(i in 2:t_final){
  a = rnorm(1,mu,sigma)
  m2[i] = tita_1*aAnt_1 + tita_2*aAnt_2 + a
  aAnt_1 =  a
  aAnt_2 =  aAnt_1
}

m2_ts <- ts(m2)

###################################################################
# Graficos MA(1) y MA(2)

autoplot(m1_ts, color="darkblue")+
  labs(y = "valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()

autoplot(m2_ts, color="darkblue")+
  labs(y = "valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()

afc_m1 <- ggAcf(m1_ts)+ labs(title = "", x="retardo")+ theme_bw()
pafc_m1 <- ggPacf(m1_ts)+ labs(x = "retardo", title = "")+ theme_bw()
grid.arrange(afc_m1, pafc_m1, nrow=1)


afc_m2 <- ggAcf(m2_ts)+ labs(x = "retardo", title = "")+ theme_bw()
pafc_m2 <- ggPacf(m2_ts)+ labs(x = "retardo", title = "")+ theme_bw()
grid.arrange(afc_m2, pafc_m2, nrow=1)
# Obtengo 4 lineas signific en ACF... 

set.seed(123)
ma_2 <- arima.sim(n = n, model = list(ma = c(0.5, 0.2)),sd = sd)
afc_ma_2 <- ggAcf(ma_2)+ labs(x = "retardo", title = "")+ theme_bw()
pafc_ma_2 <- ggPacf(ma_2)+ labs(x = "retardo", title = "")+ theme_bw()
grid.arrange(afc_ma_2, pafc_ma_2, nrow=1)

#########################################################################

# ARMA (1,1)

set.seed(123)
n = 200
sd = 1
modelo <- list(ar = 0.8, ma=0.8)
arma11 <- arima.sim(n = n, model = modelo ,sd = sd)

afc_1 <- ggAcf(arma11, lag.max = 20)+ 
  labs( x="Retardo" ,title = expression(paste("\u03D5"[1], " = 0.8, ","\u03B8"[2], " = -0.4")))+ theme_bw()

pafc_1 <- ggPacf(arma11, lag.max = 20)+ 
  labs(x = "Retardo", title = "")+ theme_bw()

acf_pacf_arma11 <- grid.arrange(afc_1, pafc_1, nrow=1)

ggsave("output/acf_pacf_arma11.png", acf_pacf_arma11, dpi = 300, width = 10, height = 3.5, units = "in")


###########################################################
# Paseo aleatorio sin deriva:
set.seed(123)
Y <- sarima.sim(0,1,0,mean=0,n=1000)
pa <- autoplot(Y, color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)")+
  theme_bw()
autoplot(diff(Y), color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)")+
  theme_bw()

afc_pa <- ggAcf(Y)+ labs(x = "Retardo", title = "")+ theme_bw()
pafc_pa <- ggPacf(Y)+ labs(x = "Retardo", title = "")+ theme_bw()
grid.arrange(afc_pa, pafc_pa, nrow=1)


# Paseo aleatorio con deriva:
set.seed(123)
Z <- sarima.sim(0,1,0,mean=0.1,n=1000)
pa_d <- autoplot(Z, color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)")+
  theme_bw()
autoplot(diff(Z), color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "tiempo (escala arbitraria)")+
  theme_bw()

afc_pa_d <- ggAcf(Z)+ labs(x = "retardo", title = "")+ theme_bw()
pafc_pa_d <- ggPacf(Z)+ labs(x = "retardo", title = "")+ theme_bw()
grid.arrange(afc_pa_d, pafc_pa_d, nrow=1)

paseo <- grid.arrange(pa,pa_d, nrow=1)

ggsave("output/paseo.png", paseo, dpi = 300, width = 10, height = 3.5, units = "in")



# Arima (1,1,1)

set.seed(123)
n = 100
sd = 0.5
modelo <- list(ar = 0.7, ma=0.3)
arima111 <- arima.sim(n = n, model = list(ar = c(0.7, 0.2)),sd = sd)

a <- autoplot(arima111, color="#1C5F9E")+
  labs(y = "Valor de las observaciones", x= "Tiempo (escala arbitraria)", title = "a)")+
  theme_bw() 

b <- autoplot(diff(arima111), color="#1C5F9E")+
  labs(y = "Primera diferencia de las observaciones", x= "Tiempo (escala arbitraria)",  title = "b)")+
  theme_bw()

arima_airma_dif <- grid.arrange(a,b, nrow=1)

ggsave("output/arima_airma_dif.png", arima_airma_dif, dpi = 300, width = 10, height = 3.5, units = "in")


# AR(2) con raices complejas:

# Cargar el paquete 'stats' para generar ruido blanco
library(stats)

# Definir los coeficientes del proceso AR(2) con raíces complejas
phi1 <- 0.5 + 0.5i
phi2 <- -0.5 - 0.5i

# Definir el tamaño de la muestra
n <- 200

# Generar una secuencia de ruido blanco
white_noise <- rnorm(n)

# Inicializar un vector para almacenar los valores generados del proceso AR(2)
ar_process <- numeric(n)

# Calcular los valores del proceso AR(2) utilizando la ecuación
for (i in 3:n) {
  ar_process[i] <- phi1 * ar_process[i-1] + phi2 * ar_process[i-2] + white_noise[i]
}

# Imprimir los primeros 10 valores del proceso AR(2) generado
print(ar_process[1:10])


ggAcf(as.numeric(ar_process),lag.max = 200)+ labs(title = "a)", x="retardo")+ theme_bw()

