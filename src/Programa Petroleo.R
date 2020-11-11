#################################################################
#################################################################
#####   Programa Adecuación grupo Sergio, Javier y Camilo    ####
#####   Fecha de creación: 20200502                          ####
#####   Fecha de última edición: 20200502                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(tseries)
library(forecast)
library(ggplot2)
library(readxl)
library(ggfortify)
library(data.table)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate)

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/2 - TAREAS/CLASE 4/Sergio Yesid Aroca/inpath/"
outpath <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/2 - TAREAS/CLASE 4/Sergio Yesid Aroca/outpath/"

#################################################################
#####                   Carga de información                #####   
#################################################################

Base.petroleo <- read_excel(paste(inpath,"Serie Precio_Petroleo.xlsx", sep = ""),
                            sheet = 1)

#################################################################
#####                   Ejecución Programa                  #####   
#################################################################

head(Base.petroleo)

Base.petroleo.new <- Base.petroleo %>% 
  mutate(date = ymd(Fecha)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  group_by(year, month) %>%
  summarise(Pbarril.month = mean(Pbarril, na.rm = T))

Base.petroleo.new <- Base.petroleo.new[-363, ]

Precio.mes <- ts(Base.petroleo.new$Pbarril.month, start = 1990, end = c(2020,2), 
                 frequency= 12) 

autoplot(Precio.mes, main = 'Precio Barril promedio mensual')
autoplot(log(Precio.mes), main = 'Log Precio Barril promedio mensual')


### Estacionario en Varianza
BoxCox.ar(Precio.mes)

### Estacionario en Media
diff.Precio.mes <- diff(Precio.mes, differences = 1)
autoplot(diff.Precio.mes)
autoplot(diff(log(Precio.mes), differences = 1))

est.Precio.mes <- diff(log(Precio.mes), differences = 1)

adf.test(est.Precio.mes)

### Especificación del modelo
acf(est.Precio.mes)
pacf(est.Precio.mes)

auto.arima(est.Precio.mes)

modelo <- Arima(log(Precio.mes), order = c(1,1,0), method="ML")

### Diagnostico
plot(modelo, type='b', xlab='Year',ylab='log(precio.barril.mes)', main = 'ARI(1,1)')
qqnorm(residuals(modelo), main = 'Q-Q Plot AR(3)'); qqline(residuals(modelo))
acf(residuals(modelo), main = 'ACF Residuales ARI(1,1)')
tsdiag(modelo, gof=62, omit.initial=F)


### Pronóstico
autoplot(forecast(modelo, h = 12), main = 'Pronóstico 12 pasos adelante - ARI(1,1)')


### Sugerencia de implementar un modelo SARIMA
modelo.2 <- Arima(log(Precio.mes), order=c(1, 1, 0),
                  seasonal = list(order=c(1, 0, 0), period=12))

### Diagnostico SARIMA
plot(modelo.2, type='b', xlab='Year',ylab='log(precio.barril.mes)', main = 'SARIMA')
qqnorm(residuals(modelo.2), main = 'Q-Q Plot SARIMA'); qqline(residuals(modelo.2))
acf(residuals(modelo.2), main = 'ACF Residuales SARIMA')
tsdiag(modelo.2, gof=62, omit.initial=F)

### Pronóstico
autoplot(forecast(modelo.2, h = 12), main = 'Pronóstico 12 pasos adelante - SARIMA')

### Sugerencia Holt-Winters
hw <- HoltWinters(log(Precio.mes))
plot(hw)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)
#https://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/




