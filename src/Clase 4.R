#################################################################
#################################################################
#####   Programa Clase 4 - Series de Tiempo                  ####
#####   Fecha de creación: 20200415                          ####
#####   Fecha de última edición: 20200415                    ####
#####   Autor: Edgar Javier López Moreno                     ####  
#####   E-mail: edgar.lopez@libertadores.edu.co              ####
#################################################################
#################################################################

rm(list = ls())
# https://stats.stackexchange.com/questions/229948/plotting-predicted-values-in-arima-time-series-in-r/353375
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

### Metodo Momentos
set.seed(1236)
ma.sim.1 <- arima.sim(list(order = c(0,0,1), ma = c(-0.9)), n = 120)
ma.sim.2 <- arima.sim(list(order = c(0,0,1), ma = c(0.9)), n = 120)
ma.sim.3 <- arima.sim(list(order = c(0,0,1), ma = c(-0.9)), n = 60)
ma.sim.4 <- arima.sim(list(order = c(0,0,1), ma = c(0.9)), n = 60)
ma.sim.5 <- arima.sim(list(order = c(0,0,1), ma = c(0.5)), n = 60)

Arima(ma.sim.1, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.2, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.3, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.4, order = c(0,0,1), method = 'CSS', include.mean = F)
Arima(ma.sim.5, order = c(0,0,1), method = 'CSS', include.mean = F)

ar.sim.1 <- arima.sim(list(order = c(1,0,0), ar = c(0.9)), n = 60)
ar.sim.2 <- arima.sim(list(order = c(1,0,0), ar = c(0.4)), n = 60)
ar.sim.3 <- arima.sim(list(order = c(2,0,0), ar = c(1.5, -0.75)), n = 120)

ar(ar.sim.1, order.max=1, AIC=F, method='yw')
ar(ar.sim.2, order.max=1, AIC=F, method='yw')
ar(ar.sim.3, order.max=2, AIC=F, method='yw')
Arima(ar.sim.3, order = c(2,0,0), include.mean = F)
arima(ar.sim.3, order = c(2,0,0), include.mean = F)

### Ejemplo real de abundancia de liebres canadienses de 1905 a 1935

data(hare)
autoplot(hare)
BoxCox.ar(hare)
acf(hare^0.5)
pacf(hare^0.5)
# Puede ser un AR(2) o AR(3)
ar(hare^0.5, order.max=2, AIC=F, method='yw')
ar(hare^0.5, order.max=3, AIC=F, method='yw')
# r1 = 0.736 y r2 = 0.304


### Esquema de simulación

data(ar1.s); data(ar1.2.s)
autoplot(ar1.s)
autoplot(ar1.2.s)
length(ar1.s); length(ar1.2.s)

ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.s,order.max=1,AIC=F,method='ols')
ar(ar1.s,order.max=1,AIC=F,method='mle')

ar(ar1.2.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='ols')
ar(ar1.2.s,order.max=1,AIC=F,method='mle')


data(ar2.s)
autoplot(ar2.s)
ar(ar2.s, order.max=2, AIC=F, method='yw')
ar(ar2.s, order.max=2, AIC=F, method='ols')
ar(ar2.s, order.max=2, AIC=F, method='mle')

data(arma11.s)
autoplot(arma11.s)
Arima(arma11.s, order=c(1,0,1),method='CSS', include.mean = F)
Arima(arma11.s, order=c(1,0,1),method='ML', include.mean = F)
Arima(arma11.s, order=c(1,0,1),method ='CSS-ML', include.mean = F) 



### Ejemplo con hare

# Especificación del modelo

data(hare)
autoplot(hare, main = 'Serie de tiempo de abundancia de liebres en Canada (1905-1935)')
BoxCox.ar(hare)
adf.test(hare^0.5)
acf(hare^0.5, main = 'ACF Liebres^1/2')
pacf(hare^0.5, main = 'PACF Liebres^1/2')


# Estimación y diagnostico

m1.hare <- Arima(hare^0.5, order=c(3,0,0), method='ML')
m2.hare <- Arima(hare^0.5, order=c(2,0,0), method='ML')
plot(m1.hare, type='b', xlab='Year',ylab='Sqrt(hare)', main = 'Raices AR(3)')
plot(m2.hare, type='b', xlab='Year',ylab='Sqrt(hare)', main = 'Raices AR(2)')

qqnorm(residuals(m1.hare), main = 'Q-Q Plot AR(3)'); qqline(residuals(m1.hare))
qqnorm(residuals(m2.hare), main = 'Q-Q Plot AR(2)'); qqline(residuals(m2.hare))
acf(residuals(m1.hare), main = 'ACF Residuales AR(3)')
acf(residuals(m2.hare), main = 'ACF Residuales AR(2)')

acf(residuals(m2.hare),plot=F)$acf
signif(acf(residuals(m2.hare),plot=F)$acf[1:6],2)
tsdiag(m2.hare, gof=15, omit.initial=F)
tsdiag(m1.hare, gof=15, omit.initial=F)


w <- Box.test(resid(m2.hare),type="Ljung",lag=5,fitdf=1)
Box.test(resid(m1.hare),type="Ljung",lag=5,fitdf=1)

# Pronostico

autoplot(forecast(m1.hare, h = 30), main = 'Pronóstico 30 pasos adelante - AR(3)')
autoplot(forecast(m2.hare, h = 30), main = 'Pronóstico 30 pasos adelante - AR(2)')

autoplot(forecast(m1.hare, h = 10), main = 'Pronóstico 10 pasos adelante - AR(3)')
autoplot(forecast(m2.hare, h = 10), main = 'Pronóstico 10 pasos adelante - AR(2)')


### Ejemplo Oil

# Especificación

data(oil.price)
autoplot(oil.price)
BoxCox.ar(oil.price)
autoplot(log(oil.price))
autoplot(diff(oil.price))
autoplot(diff(log(oil.price)))
adf.test(diff(log(oil.price)))
acf(diff(log(oil.price)))
pacf(diff(log(oil.price)))


# Estimación y diagnostico
Arima(log(oil.price),order=c(0,1,1),method='CSS')
Arima(log(oil.price),order=c(0,1,1),method='ML')
Arima(log(oil.price),order=c(0,1,2),method='ML')


m1.oil <- Arima(log(oil.price), order=c(0,1,1))#, include.drift = T)
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)
#autoplot(rstandard(m1.oil))

qqnorm(residuals(m1.oil)); qqline(residuals(m1.oil))
acf(residuals(m1.oil))
hist(window(rstandard(m1.oil), start=c(1986,1)),
     xlab='Standardized Residuals', 
     main = 'Histograma Residuales')
tsdiag(m1.oil, gof=15, omit.initial=F)
Box.test(resid(m1.oil),type="Ljung",lag=10,fitdf=1)

autoplot(forecast(m1.oil, h = 12))
autoplot(forecast(auto.arima(log(oil.price))))
auto.arima(oil.price)

autoplot(forecast(auto.arima(oil.price), h = 60))

hw <- HoltWinters(oil.price)
plot(hw)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)
#https://www.r-bloggers.com/holt-winters-forecast-using-ggplot2/



data(hare)
autoplot(hare)
serie <- (hare^0.5)
autoplot(serie)

d = 0
k = 5
estim <- NULL
for( p in 0:5 ){
  for( q in 0:5 ){
    if( !(p == 0 & q == 0) ){
      model <- Arima(serie, order=c(p,d,q), method="ML")
      BIC   <- AIC(model,k = log(length(sunspots)))
      BOX   <- Box.test(resid(model), type="Ljung",lag=k,fitdf=1)$p.value
      estim <- rbind(estim , cbind(p,d,q,AIC(model),BIC, BOX))
      
      print(paste('ARIMA(',p,',',d,',',q,')', sep = ""))
      
    }
  }
}
estim <- data.frame(estim)
names(estim) <- c('AR(p)', 'Diff', 'MA(q)', 'AIC', 'BIC', 'BOX')
estim %>% filter(BOX <= 0.1) %>% arrange(AIC)



#### Simulación

##Simulate VAR(2)???data1
library(dse)
library(vars)

##Setting the lag???polynomialA(L)
Apoly <- array(c(1.0, -0.5, 0.3, 0, 0.2, 0.1, 
                 0, -0.2, 0.7, 1, 0.5, -0.3),
               c(3,2,2))

##Setting Covariance to identity???matrix
B <-diag(2)

##Setting constant term to 5 and 10
TRD <- c(5,10)

##Generating the VAR(2) model 
var2 <- ARMA(A=Apoly,B=B,TREND=TRD)

##Simulating 500 observations
varsim <- simulate(var2, sampleT=500,
                   noise=list(w=matrix(rnorm(1000),
                                       nrow=500,ncol=2)),
                   rng=list(seed=c(123456)))

##Obtaining the generated series
vardat <- matrix(varsim$output,nrow=500,ncol=2)
colnames(vardat)<-c('y1','y2')

##Plotting the series
plot.ts(vardat,main='',xlab='')

##Determining an appropriate lag???order
infocrit<-VARselect(vardat,lag.max=3,
                    type='const')

##Estimating the model
varsimest<-VAR(vardat,p=2,type='const',
               season=NULL,exogen=NULL)

##Alternatively, selection accordingto AIC
varsimest<-VAR(vardat,type='const',
               lag.max=3,ic='SC')

##Checking the roots
roots<-roots(varsimest)





#### Estimación

da=read.table("q-gdp-ukcaus.txt",header=T)
gdp=log(da[,3:5])
dim(gdp)
z=gdp[2:126,]-gdp[1:125,] ## Growth rate
z=z*100 ## Percentage growth rates
dim(z)
Z=z[3:125,]
X=cbind(rep(1,123),z[2:124,],z[1:123,])
X=as.matrix(X)
XPX=t(X)%*%X
XPXinv=solve(XPX)
Z=as.matrix(Z)
> XPZ=t(X)%*%Z
> bhat=XPXinv%*%XPZ
> bhat

> COV=kronecker(Sig,XPXinv)
> se=sqrt(diag(COV))
> para=cbind(beta,se,beta/se)
> para

> Sig1=t(A)%*%A/(125-2) ## MLE of Sigma_a
> Sig1

> da=read.table("q-gdp-ukcaus.txt",header=T)
> gdp=log(da[,3:5])
> z=gdp[2:126,]-gdp[1:125,]
> z=z*100
> m1=VAR(z,2)

> da=read.table("q-gdp-ukcaus.txt",header=T)
> x=log(da[,3:5])
> dim(x)
[1] 126 3
> dx=x[2:126,]-x[1:125,]
> dx=dx*100
> C=0.1*diag(7) ### lambda = 0.1
> V0=diag(3) ### Vo = I_3
> mm=BVAR(dx,p=2,C,V0)


