#################################################################
#################################################################
#####   Programa Clase 7 - Series de Tiempo                  ####
#####   Fecha de creación: 20200605                          ####
#####   Fecha de última edición: 20200605                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())
# https://cran.r-project.org/web/packages/timeSeries/vignettes/timeSeriesPlot.pdf
#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(TSA)

library(mvtnorm)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

library(dse) 
library(vars) 
#library(MTS)

#################################################################
#####                   Sistema de carpetas                 #####   
#################################################################

inpath  <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/1-SERIES DE TIEMPO/inpath/"
outpath <- "D:/PERSONAL/6. EDUCACIÓN/1-LIBERTADORES/1-SERIES DE TIEMPO/outpath/"

#################################################################
#####                   Carga de información                #####   
#################################################################

GDP_EEUU   <- read_excel(paste(inpath,"GDP EEUU.xlsx", sep = ""),
                         col_types = c("numeric", "numeric", "numeric","numeric"))
GDP_UKCAUS <- read_excel(paste(inpath,"GDP UK CA US.xlsx", sep = ""))

#################################################################
#####                   Ejecución Programa                  #####   
#################################################################

### Simulación VAR(2)
library(dse) 
library(vars) 

Apoly <- array(c(1.0, -0.5, 0.3, 0, 0.2, 0.1, 0, -0.2, 0.7, 1, 0.5, -0.3),
               c(3, 2, 2)) 
B   <- diag(2) 
TRD <- c(5, 10)

### Generando el modelo VAR(2)
var2 <- ARMA(A = Apoly, B = B, TREND = TRD) 

### Simulación de 500 observaciones
varsim <- simulate(var2, sampleT = 500, 
                   noise = list(w = matrix(rnorm(1000), nrow = 500, ncol = 2)),
                   rng = list(seed = c (123456)))
vardat <- matrix(varsim$output, nrow = 500 , ncol = 2) 
colnames(vardat) <- c('y1', 'y2')
plot.ts(vardat, main = 'Simulación variables de VAR(2)', xlab = 'tiempo')
#MTSplot(vardat)

infocrit <- VARselect(vardat, lag.max = 3 , type = 'const')

### Estimación del modelo
varsimest <- VAR(vardat, p = 2, type = 'const',
                 season = NULL, exogen = NULL) 

varsimest <- VAR(vardat, type = 'const',
                 lag.max = 3, ic = 'SC')

### revisando raices
roots <- roots(varsimest)


### Testeando correlación 
args(serial.test) 

### Portmanteau ??? Test
var2c.serial <- serial.test(varsimest, lags.pt = 16,
                            type = 'PT.asymptotic')

var2c.serial
plot(var2c.serial, names = 'y1')
plot(var2c.serial, names = 'y2')

### Testeando heteroscedaticidad
args(arch.test) 
var2c.arch <- arch.test(varsimest, lags.multi = 5,
                        multivariate.only = TRUE)
var2c.arch

### Testeando normalidad
args(normality.test)
var2c.norm <- normality.test(varsimest,
                             multivariate.only = TRUE) 
var2c.norm 

### Calse y metodos para diagnosticos
class(var2c.serial)
class(var2c.arch)
class(var2c.norm)
methods(class = 'varcheck')

### Graficos de clase Varcheck
args(vars:::plot.varcheck)
plot(var2c.serial, names = 'y1')

reccusum <- stability(varsimest, type = 'OLS???CUSUM')
fluctuation <- stability(varsimest, type = 'fluctuation')

## Forecasting
args(vars:::predict.varest)
predictions <- predict(varsimest, n.ahead = 25, ci = 0.95)
class(predictions)

args(vars:::plot.varprd)
plot(predictions, names = 'y1')
args(fanchart)
fanchart(predictions , names = 'y2')

### Test de causalidad de Granger
var.causal <- causality(varsimest, cause = 'y2')

## Analisis de impulso
irf.y1 <- irf(varsimest, impulse = 'y1',
              response = 'y2', n.ahead = 10,
              ortho = FALSE, cumulative = FALSE,
              boot = FALSE, seed = 12345) 
args(vars:::plot.varirf)
plot(irf.y1)
irf.y2 <- irf(varsimest, impulse = 'y2',
              response = 'y1', n.ahead = 10,
              ortho = TRUE, cumulative = TRUE,
              boot = FALSE, seed = 12345)
plot(irf.y2)

## Pronostico descomosición de la varianza
fevd.var2 <- fevd(varsimest, n.ahead = 10) 
args(vars:::plot.varfevd) 
plot(fevd.var2, addbars = 2)



#############################################################
### Cointegración 
#############################################################

### Regresión espurea
library(lmtest)
set.seed(123456) 
e1 <- rnorm(500)
e2 <- rnorm(500) 
trd <- 1:500 
y1 <- 0.8*trd + cumsum(e1) 
y2 <- 0.6*trd + cumsum(e2) 
plot.ts(cbind(y1, y2), main = 'Simulación variables', xlab = 'tiempo')
sr.reg <- lm(y1 ~ y2)
sr.dw  <- dwtest(sr.reg)$statistic
sr.reg
sr.dw

### Cointegración 
set.seed(123456) 
e1 <- rnorm(100) 
e2 <- rnorm(100)
y1 <- cumsum(e1)
y2 <- 0.6*y1 + e2
lr.reg <- lm(y2 ~ y1)
plot(y1, y2)
plot.ts(cbind(y1, y2), main = 'Simulación series')
error  <- residuals(lr.reg)
error.lagged <- error[-c(99, 100)]
dy1 <- diff(y1) 
dy2 <- diff(y2) 
diff.dat <- data.frame(embed(cbind(dy1, dy2), 2))
colnames(diff.dat) <- c('dy1', 'dy2', 'dy1.1', 'dy2.1')
head(diff.dat)
plot.ts(diff.dat)
ecm.reg <- lm(dy2 ~ error.lagged + dy1.1 + dy2.1, data = diff.dat)
ecm.reg


#### VECM
library(urca)
set.seed(12345) 
e1 <- rnorm(250, 0, 0.5)
e2 <- rnorm(250, 0, 0.5)
e3 <- rnorm(250, 0, 0.5)
u1.ar1 <- arima.sim(model = list(ar = 0.75),
                    innov = e1, n = 250)
u2.ar1 <- arima.sim(model = list(ar = 0.3),
                    innov = e2, n = 250)
y3 <- cumsum(e3)
y1 <- 0.8*y3 + u1.ar1
y2 <- -0.3*y3 + u2.ar1
ts.plot(y1, y2, y3, main = 'Series simuladas',
        gpars = list(xlab = "tiempo", ylab = "valores", lty = c(1:3)))
y.mat <- data.frame(y1, y2, y3)
vecm  <- ca.jo(y.mat)
jo.results <- summary(vecm)
vecm.r2 <- cajorls(vecm, r = 2)
class(jo.results)
slotNames(jo.results)

library(vars)
vecm.level <- vec2var(vecm, r = 2)
arch.test(vecm.level)
normality.test(vecm.level)
serial.test(vecm.level) 
predict(vecm.level)
irf(vecm.level, boot = FALSE) 
fevd(vecm.level)
class(vecm.level)
methods(class = 'vec2var')


par(mfrow = c(1,1))
parks.ts <- cbind(everglades.ts, glacier.ts, redwood.ts)
plot(aggregate.ts(parks.ts, FUN=sum), xlab = "Time (years)", ylab = "Total Yearly Visitors (1000's)", plot.type = "single", col = c(1:3), lwd = 2)
title("Total Annual Recreational Visitors")
legend("topleft", c("Everglades", "Glacier", "Redwood"), lty = c(1,1,1), col = c(1:3), lwd = 2)

### Cointegración versión 2
# https://faculty.chicagobooth.edu/ruey.tsay/teaching/mts/sp2011/

library(fUnitRoots)
library(urca)
library(MTS)
library(ggplot2)
library(readr)

m_bnd <- read_table2(paste(inpath, "m-bnd.txt", sep = ""), col_names = FALSE,
                     col_types = cols(X4 = col_number(), X5 = col_number()))
head(m_bnd)
tail(m_bnd)

bnd <- as.matrix(m_bnd[,4:5], ncol = 2)
colnames(bnd) <- c("Aaa","Baa")
plot.ts(bnd)
ts.plot(bnd)

m1 <- VARorder(bnd)
datos.int  <- bnd
plot.crite <- data.frame(rbind(cbind(round(0:13,0), 'AIC', VARorder(datos.int)$aic),
                               cbind(round(0:13,0), 'BIC', VARorder(datos.int)$bic),
                               cbind(round(0:13,0), 'HQ', VARorder(datos.int)$hq)))
names(plot.crite) <- c('p', 'Criterio', 'Valores')
plot.crite$p       <- as.numeric(as.character(plot.crite$p)) 
plot.crite$Valores <- as.numeric(as.character(plot.crite$Valores))
ggplot(plot.crite, aes(x = p,y = Valores, group = Criterio)) + 
  geom_line(aes(color = Criterio))

pacf(bnd[,1])
pacf(bnd[,2])

adfTest(bnd[,1], lags=3, type="c")
adfTest(bnd[,2], lags=2, type="c")

m2 <- ca.jo(bnd, K=2, ecdet=c("none")) 
summary(m2)

m3 <- ca.jo(bnd, K=2, ecdet=c("none"), spec=c("transitory"))
summary(m3) 

m4 <- ca.jo(bnd, K=2, ecdet=c("none"), type=c("trace"), spec=c("transitory"))
summary(m4)

wt <- bnd[,1] - 0.886*bnd[,2] 
adfTest(wt, lags=3, type='c')

#### Ejemplo Canada
# https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf

library("vars")
data("Canada")
str(Canada)
Canada
summary(Canada)
plot(Canada, nc = 2, xlab = "")
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))

VARselect(Canada, lag.max = 8, type = "both")

Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct   <- VAR(Canada, p = 1, type = "both")

summary(p1ct, equation = "e")
plot(p1ct, names = "e")
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial
norm1 <- normality.test(p1ct)
norm1$jb.mul

arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul

plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)

summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3,
                 spec = "transitory"))
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2,
                 spec = "transitory"))

vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",
                 ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)




library("vars")
data(Canada)
VAR(Canada, p = 2, type = "none")
VAR(Canada, p = 2, type = "const")
VAR(Canada, p = 2, type = "trend")
VAR(Canada, p = 2, type = "both")



