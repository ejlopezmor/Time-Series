#################################################################
#################################################################
#####   Programa Clase 3 - Series de Tiempo                  ####
#####   Fecha de creación: 20200325                          ####
#####   Fecha de última edición: 202003                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())
# https://rpubs.com/riazakhan94/arima_with_example
#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)
library(tseries)
library(forecast)
library(ggplot2)
library(readxl)
library(ggfortify)

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/1-SERIES DE TIEMPO/inpath/"
outpath <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/1-SERIES DE TIEMPO/outpath/"

#################################################################
#####                   Carga de información                #####   
#################################################################

TRM_COL <- read_excel(paste(inpath,"TRM COL.xlsx", sep = ""),
                      sheet = "BaseDatos")
TRM     <- ts(TRM_COL$TRM, start = c(1991, 315), frequency=365)

#################################################################
#####                 Ejecución de programa                 #####   
#################################################################

### Simulación IMA(2,2)
set.seed(1236)
ts.sim.ima <- arima.sim(list(order = c(0,2,2), ma = c(1, -0.6)), n = 300)
autoplot(ts.sim.ima, ylab=' ')

ts.sim.ima.d1 <- autoplot(diff(ts.sim.ima),ylab='Primera diferencia')
autoplot(ts.sim.ima.d1)

ts.sim.ima.d2 <- autoplot(diff(ts.sim.ima, difference=2),ylab='Segunda diferencia')
autoplot(ts.sim.ima.d2)

ts.sim.ima.d3 <- autoplot(diff(ts.sim.ima, difference=3),ylab='Tercera diferencia')
autoplot(ts.sim.ima.d3)


### Transformación en varianza
data("electricity")
autoplot(electricity)
autoplot(log(electricity), ylab = 'Log(Electricity)')
autoplot(diff(log(electricity)))
BoxCox.ar(electricity)


### Ejercicio Practico 1
data(oil.price)
autoplot(oil.price)
adf.test(oil.price)
BoxCox.ar(oil.price)
log.oil.price <- log(oil.price) 
autoplot(log.oil.price)

# Diferenciando
new.oil.price <- diff(log.oil.price)
autoplot(new.oil.price)
adf.test(new.oil.price)

autoplot(acf(new.oil.price, plot = FALSE))
autoplot(pacf(new.oil.price, plot = FALSE))

plot(pacf(new.oil.price))
# Podriamos ajustar un ARIMA(1,1,0)

### Ejericico Practico 2

autoplot(TRM)
diff.TRM <- diff(TRM, difference=1)
autoplot(diff.TRM)
adf.test(diff.TRM)
autoplot(acf(diff.TRM, plot = FALSE))
autoplot(pacf(diff.TRM, plot = FALSE))
# Podriamos ajustar un ARIMA(5,1,0)
auto.arima(diff.TRM)



ts.prueba <- arima.sim(list(order = c(2,0,0), ar = c(0.25, -0.75)), n = 300)
autoplot(ts.prueba)
autoplot(acf(ts.prueba))
autoplot(pacf(ts.prueba))

























data(color)
autoplot(color)
BoxCox.ar(color)

acf(color)
pacf(color)
adf.test(color)

m1.color <- Arima(color,order=c(1,0,0))
plot(m1.color,n.ahead=12,type='b',xlab='Time', ylab='Color Property')
abline(h=coef(m1.color)[names(coef(m1.color))=='intercept'])

qqnorm(residuals(m1.color)); qqline(residuals(m1.color))
tsdiag(m1.color,gof=15,omit.initial=F)
Box.test(resid(m1.color),type="Ljung",lag=5,fitdf=1)

#autoplot(forecast(m1.color, h = 50))

