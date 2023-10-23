# Fuente de los Datos: https://snih.hidricosargentina.gob.ar/Filtros.aspx

library(tidyverse)
library(dplyr)
library(stats)
library(tseries)

# Bajé los datos hasta 2021, pero voy a usar hasta 2016,
# como en la tesis de Melanie.

tunel <- read.table('data/datos_tunel_hasta_2021.txt')
head(tunel)
tail(tunel)


# Verificar si hay datos repetidos en la columna de la fecha (V1)
columna <- "V1"  # Nombre de la columna
hay_repetidos <- any(duplicated(tunel[[columna]])) # FALSE

# Verificar si hay datos faltantes
which(is.na(tunel),arr.ind=T) # No hay datos faltantes

nrow(tunel) # Hay 563 filas, asi que debe faltar alguna fecha...

dates <- as.Date(tunel$V1, "%d/%m/%Y")
head(dates)
tail(dates)
require(lubridate)
library(lubridate)
day<-day(dates)
month<-month(dates)
year<-year(dates)
df<-as.data.frame(cbind(day, month, year,tunel$V3))
colnames(df)<-c("day", "month", "year", "caudal")
head(df)
tail(df)
nrow(df)


# Veo cual fecha falta, armando una grilla de fechas
# y haciendo merge con df

library(RcppBDT)
dates <-seq(as.Date("1975-01-01"), as.Date("2021-12-01"), by="1 month")
dfdates <- data.frame(
  year = as.numeric(format(dates, format = "%Y")),
  month = as.numeric(format(dates, format = "%m")),
  day = as.numeric(format(dates, format = "%d")))
names(df)
names(dfdates)
tail(dfdates)
df_full_raw<-merge(x=dfdates, y=df,by=c('year', "month","day"), all.x=TRUE)
df_full<-df_full_raw[order(df_full_raw$year, df_full_raw$month, df_full_raw$day),]
rownames(df_full)<-seq(length=nrow(df_full)) 
tail(df_full)

# deberían haber 564 filas, asi que veo qué falta

which(is.na(df_full),arr.ind=T) # Fila 537 hay un dato faltante de caudal (columna 4)
df_full$month[is.na(df_full$caudal)] # mes septiempre
df_full$year[is.na(df_full$caudal)] # anio 2019
sum(is.na(df_full$caudal)) # 1


# Completo el NA con el valor medio de caudal de septiembre

df_full %>% na.omit(df_full) %>% 
  filter(month == 9) %>%
  summarise(prom = mean(caudal)) # 13523.42

# Completo el df_full con el dato que falta:

df_full[537, 4] <- 13523.42
# View(df_full)

write.csv(df_full, file = "data/tunel_full.csv", row.names = FALSE)


### Listo para empezar a trabajar!!!!
