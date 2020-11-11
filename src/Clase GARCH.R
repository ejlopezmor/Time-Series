#################################################################
#################################################################

rm(list = ls())

 # https://cran.r-project.org/web/packages/qrmtools/vignettes/ARMA_GARCH_VaR.html
 # https://www.r-bloggers.com/the-realized-garch-model/
 # https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf
 # https://rpubs.com/johnakwei/207852
 # https://stats.stackexchange.com/questions/254101/forecasting-time-series-using-arma-garch-in-r
 # http://www.unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
 # https://www.stat.pitt.edu/stoffer/tsa4/
 # https://www.stat.pitt.edu/stoffer/tsda/

#################################################################
#####                   Librerias utilizadas                #####   
#################################################################

library(astsa)
library(tseries)
library(rugarch)

#################################################################
#####                   Carga de información                #####   
#################################################################

##### Datos simulados

n <- 1100
a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e <- rnorm(n)  
y <- double(n)
y[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n) y[i] <- e[i]*sqrt(a[1] + a[2]*y[i-1]^2 + a[3]*y[i-2]^2)
y <- ts(y[101:1100])
plot(y)

#####

par(mfrow=c(1,2))
acf(y)
pacf(y)
y.arch <- garch(y, order = c(0,2))  # Fit ARCH(2) 
summary(y.arch)                     # Diagnostic tests
plot(y.arch) 

#####

model.sim = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   distribution.model = "norm")
fit.sim   = ugarchfit(model.sim, y)
fit.sim
plot(fit.sim)

fspec.sim = getspec(fit.sim) 
setfixed(fspec.sim) <- as.list(coef(fit.sim))

m <- ceiling(1000 / 10)
pred.sim <- ugarchforecast(fspec.sim, data = y, n.ahead = 50, n.roll = m-1, out.sample = m)
plot(pred.sim)


#### Datos NYSE 


data(nyse)

plot(nyse, ylab="Retornos de NYSE")
nyse.g = garch(nyse, order=c(1,1))
summary(nyse.g)

### Otra forma de estimar

model = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   distribution.model = "norm")
fit   = ugarchfit(model, nyse)
fit
plot(fit)

fspec = getspec(fit) 
setfixed(fspec) <- as.list(coef(fit))

m <- ceiling(100 / 10)
pred <- ugarchforecast(fspec, data = nyse, n.ahead = 50, n.roll = m-1, out.sample = m)
plot(pred)
#################################################################
#####                                                       #####   
#################################################################































library(astsa)
library(tseries)

data(nyse)
data("gnp")

plot(nyse, ylab="Retornos de NYSE")
nyse.g = garch(nyse, order=c(1,1))
#summary.garch(nyse.g)
summary(nyse.g)

mu.predict <- fitted(pred) # extract predicted X_t (= conditional mean mu_t; note: E[Z] = 0)
sig.predict <- sigma(pred) # extract predicted sigma_t
VaR.predict <- as.numeric(quantile(pred, probs = alpha)) # corresponding predicted VaR_alpha

plot(nyse)
## Predictions
t. <- length(nyse) + seq_len(m) # future time points
lines(t., mu.predict, col = "blue") # predicted process X_t (or mu_t)
lines(t., VaR.predict, col = "red") # predicted VaR_alpha
lines(t., VaR.CI[1,], col = "orange") # lower 95%-CI for VaR_alpha
lines(t., VaR.CI[2,], col = "orange") # upper 95%-CI for VaR_alpha


## Simulated (original) data (X_t), fitted conditional mean mu_t and VaR_alpha
plot(nyse, type = "l", xlim = xran, ylim = yran, xlab = "Time t", ylab = "",
     main = "Simulated ARMA-GARCH, fit, VaR, VaR predictions and CIs")


lines(as.numeric(mu.), col = adjustcolor("darkblue", alpha.f = 0.5)) # hat{\mu}_t
lines(VaR., col = "darkred") # estimated VaR_alpha
mtext(paste0("Expected exceed.: ",btest$expected.exceed,"   ",
             "Actual exceed.: ",btest$actual.exceed,"   ",
             "Test: ", btest$cc.Decision),
      side = 4, adj = 0, line = 0.5, cex = 0.9) # label
plot(nyse)
## Predictions
t. <- length(nyse) + seq_len(m) # future time points
lines(t., mu.predict, col = "blue") # predicted process X_t (or mu_t)
lines(t., VaR.predict, col = "red") # predicted VaR_alpha
lines(t., VaR.CI[1,], col = "orange") # lower 95%-CI for VaR_alpha
lines(t., VaR.CI[2,], col = "orange") # upper 95%-CI for VaR_alpha
legend("bottomright", bty = "n", lty = rep(1, 6), lwd = 1.6,
       col = c("black", adjustcolor("darkblue", alpha.f = 0.5), "blue",
               "darkred", "red", "orange"),
       legend = c(expression(X[t]), expression(hat(mu)[t]),
                  expression("Predicted"~mu[t]~"(or"~X[t]*")"),
                  substitute(widehat(VaR)[a], list(a = alpha)),
                  substitute("Predicted"~VaR[a], list(a = alpha)),
                  substitute("95%-CI for"~VaR[a], list(a = alpha))))




bootp = ugarchboot(fit, method = c("Partial", "Full")[1], n.ahead = 500, n.bootpred = 500)
show(bootp)
plot(bootp)




u = predict(nyse.g)

par(mfrow=c(1,2))
plot(nyse, type="l", xlab="Time", ylab="NYSE Returns")
lines(u[,1], col="blue", lty="dashed")
lines(u[,2], col="blue", lty="dashed")

plot(800:1000, nyse[800:1000], type="l", xlab="Time", ylab="NYSE Returns")
lines(u[,1], col="blue", lty="dashed")
lines(u[,2], col="blue", lty="dashed")




plot(gnp, ylab="PNB", main = "PNB trimestral de USA, de 1947(1) a 2002(3)")
acf(gnp, 50)
gnpgr = diff(log(gnp)) # growth rate
plot.ts(gnpgr)
par(mfrow=c(2,1))
acf(gnpgr, 24)
pacf(gnpgr, 24)
# ARIMA fits:
gnpgr.ar = arima(gnpgr, order = c(1, 0, 0))
gnpgr.ma = arima(gnpgr, order = c(0, 0, 2))
# to view the results:
gnpgr.ar # potential problem here (see below *)
gnpgr.ma
ARMAtoMA(ar=.35, ma=0, 10) # prints psi-weights

tsdiag(gnpgr.ma, gof.lag=20)
hist(gnpgr.ma$resid, br=12)
qqnorm(gnpgr.ma$resid)
shapiro.test(gnpgr.ma$resid)

# AIC
gnpgr.ma$aic
-1431.929 # MA(2)
gnpgr.ar$aic
-1431.221 # AR(1)
# AICc - see Section 2.2
log(gnpgr.ma$sigma2)+(222+2)/(222-2-2)
-8.297199 # MA(2)
log(gnpgr.ar$sigma2)+(222+1)/(222-1-2)
-8.294156 # AR(1)
# SIC or BIC - see Section 2.2
log(gnpgr.ma$sigma2)+(2*log(222)/222)
-9.276049 # MA(2)
log(gnpgr.ar$sigma2)+(1*log(222)/222)
-9.288084 # AR(1)

The AIC and AICc both prefer the MA(2) fit, whereas the SIC (or BIC)
prefers the simpler AR(1) model. It is often the case that the SIC will
select a model of smaller order than the AIC or AICc. It would not be
unreasonable in this case to retain the AR(1) because pure autoregressive
models are easier to work with.

par(mfrow=c(1,2))
acf((gnpgr.ar$resid)^2, 24)
pacf((gnpgr.ar$resid)^2, 24)

acf(residuals(gnpgr.ar)^2, 120)
pacf(residuals(gnpgr.ar)^2, 120)


gnpr <- diff(log(gnp)) # gnp returns
gnpr.mod <- garch(gnpr, c(1,0))
gnpr.mod <- garch(gnpr~ar(1),~garch(1,0))
summary(gnpr.mod)

gnpr.ar = ar.mle(gnpr, order.max=1) # recall phi1 =.347
y = gnpr.ar$resid[2:length(gnpr)] # first resid is NA
arch.y = garch(y,order=c(0,1))
#summary.garch(arch.y)
summary(arch.y)



# NOT RUN {
n <- 1100
a <- c(0.1, 0.5, 0.2)  # ARCH(2) coefficients
e <- rnorm(n)  
x <- double(n)
x[1:2] <- rnorm(2, sd = sqrt(a[1]/(1.0-a[2]-a[3]))) 
for(i in 3:n) x[i] <- e[i]*sqrt(a[1]+a[2]*x[i-1]^2+a[3]*x[i-2]^2)
x <- ts(x[101:1100])
x.arch <- garch(x, order = c(0,2))  # Fit ARCH(2) 
summary(x.arch)                     # Diagnostic tests
plot(x.arch)   


library(rugarch)
model = ugarchspec(variance.model=list(model="sGARCH"), distribution="norm")
fit   = ugarchfit(model, x)

model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = list(beta1=0.86) )


modelfit=ugarchfit(x, order = c(0,2))

w <- predict(x.arch)

data(EuStockMarkets)  
plot(EuStockMarkets[,"DAX"])
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
plot(dax.garch)          # conditional normality seems to be violated
# }

data(sp500ret)
spec = ugarchspec(variance.model=list(model="csGARCH"), distribution="std")
fit = ugarchfit(spec, sp500ret)
summary(fit)
fit
bootp = ugarchboot(fit, method = c("Partial", "Full")[1],
                   n.ahead = 500, n.bootpred = 500)
show(bootp)


#####################################


library(rugarch)
library(qrmtools)

## Model specification (for simulation)
nu <- 3 # d.o.f. of the standardized distribution of Z_t
fixed.p <- list(mu = 0, # our mu (intercept)
                ar1 = 0.5, # our phi_1 (AR(1) parameter of mu_t)
                ma1 = 0.3, # our theta_1 (MA(1) parameter of mu_t)
                omega = 4, # our alpha_0 (intercept)
                alpha1 = 0.4, # our alpha_1 (GARCH(1) parameter of sigma_t^2)
                beta1 = 0.2, # our beta_1 (GARCH(1) parameter of sigma_t^2)
                shape = nu) # d.o.f. nu for standardized t_nu innovations
armaOrder <- c(1,1) # ARMA order
garchOrder <- c(1,1) # GARCH order
varModel <- list(model = "sGARCH", garchOrder = garchOrder)
spec <- ugarchspec(varModel, mean.model = list(armaOrder = armaOrder),
                   fixed.pars = fixed.p, distribution.model = "std") # t standardized residuals

## Simulate (X_t)
n <- 1000 # sample size (= length of simulated paths)
x <- ugarchpath(spec, n.sim = n, m.sim = 1, rseed = 271) # n.sim length of simulated path; m.sim = number of paths
## Note the difference:
## - ugarchpath(): simulate from a specified model
## - ugarchsim():  simulate from a fitted object
plot(x)

## Extract the resulting series
X <- fitted(x) # simulated process X_t = mu_t + epsilon_t for epsilon_t = sigma_t * Z_t
sig <- sigma(x) # volatilities sigma_t (conditional standard deviations)
eps <- x@path$residSim # unstandardized residuals epsilon_t = sigma_t * Z_t
## Note: There are no extraction methods for the unstandardized residuals epsilon_t
##       for uGARCHpath objects (only for uGARCHfit objects; see below).

## Sanity checks (=> fitted() and sigma() grab out the right quantities)
stopifnot(all.equal(X,   x@path$seriesSim, check.attributes = FALSE),
          all.equal(sig, x@path$sigmaSim,  check.attributes = FALSE))

## Plots
plot(X,   type = "l", xlab = "t", ylab = expression(X[t]))

plot(sig, type = "h", xlab = "t", ylab = expression(sigma[t]))

plot(eps, type = "l", xlab = "t", ylab = expression(epsilon[t]))

## Fit an ARMA(1,1)-GARCH(1,1) model
spec <- ugarchspec(varModel, mean.model = list(armaOrder = armaOrder),
                   distribution.model = "std") # without fixed parameters here
fit <- ugarchfit(spec, data = X) # fit

## Extract the resulting series
mu. <- fitted(fit) # fitted hat{mu}_t (= hat{X}_t)
sig. <- sigma(fit) # fitted hat{sigma}_t

## Sanity checks (=> fitted() and sigma() grab out the right quantities)
stopifnot(all.equal(as.numeric(mu.),  fit@fit$fitted.values),
          all.equal(as.numeric(sig.), fit@fit$sigma))

## Plot data X_t and fitted hat{mu}_t
plot(X, type = "l", xlab = "t",
     ylab = expression("Data"~X[t]~"and fitted values"~hat(mu)[t]))
lines(as.numeric(mu.), col = adjustcolor("blue", alpha.f = 0.5))
legend("bottomright", bty = "n", lty = c(1,1),
       col = c("black", adjustcolor("blue", alpha.f = 0.5)),
       legend = c(expression(X[t]), expression(hat(mu)[t])))

## Plot the unstandardized residuals epsilon_t
resi <- as.numeric(residuals(fit))
stopifnot(all.equal(fit@fit$residuals, resi))
plot(resi, type = "l", xlab = "t", ylab = expression(epsilon[t])) # check residuals epsilon_t

## Q-Q plot of the standardized residuals Z_t against their specified t
## (t_nu with variance 1)
Z <- as.numeric(residuals(fit, standardize = TRUE))
stopifnot(all.equal(Z, fit@fit$z, check.attributes = FALSE),
          all.equal(Z, as.numeric(resi/sig.)))
qq_plot(Z, FUN = function(p) sqrt((nu-2)/nu) * qt(p, df = nu),
        main = substitute("Q-Q plot of ("*Z[t]*") against a standardized"~italic(t)[nu.],
                          list(nu. = round(nu, 2))))

## VaR confidence level we consider here
alpha <- 0.99

## Extract fitted VaR_alpha
VaR. <- as.numeric(quantile(fit, probs = alpha))

## Build manually and compare the two
nu. <- fit@fit$coef[["shape"]] # extract (fitted) d.o.f. nu
VaR.. <- as.numeric(mu. + sig. * sqrt((nu.-2)/nu.) * qt(alpha, df = nu.)) # VaR_alpha computed manually
stopifnot(all.equal(VaR.., VaR.))
## => quantile(<rugarch object>, probs = alpha) provides VaR_alpha = hat{mu}_t + hat{sigma}_t * q_Z(alpha)

## Note: VaRTest() is written for the lower tail (not sign-adjusted losses)
##       (hence the complicated call here, requiring to refit the process to -X)
btest <- VaRTest(1-alpha, actual = -X,
                 VaR = quantile(ugarchfit(spec, data = -X), probs = 1-alpha))
btest$expected.exceed # number of expected exceedances = (1-alpha) * n

btest$actual.exceed # actual exceedances

## Unconditional test
btest$uc.H0 # corresponding null hypothesis

btest$uc.Decision # test decision

## Conditional test
btest$cc.H0 # corresponding null hypothesis

btest$cc.Decision # test decision


fspec <- getspec(fit) # specification of the fitted process
setfixed(fspec) <- as.list(coef(fit)) # set the parameters to the fitted ones
m <- ceiling(n / 10) # number of steps to forecast (roll/iterate m-1 times forward with frequency 1)
pred <- ugarchforecast(fspec, data = X, n.ahead = 1, n.roll = m-1, out.sample = m) # predict from the fitted process

## Extract the resulting series
mu.predict <- fitted(pred) # extract predicted X_t (= conditional mean mu_t; note: E[Z] = 0)
sig.predict <- sigma(pred) # extract predicted sigma_t
VaR.predict <- as.numeric(quantile(pred, probs = alpha)) # corresponding predicted VaR_alpha

## Checks
## Sanity checks
stopifnot(all.equal(mu.predict, pred@forecast$seriesFor, check.attributes = FALSE),
          all.equal(sig.predict, pred@forecast$sigmaFor, check.attributes = FALSE)) # sanity check
## Build predicted VaR_alpha manually and compare the two
VaR.predict. <- as.numeric(mu.predict + sig.predict * sqrt((nu.-2)/nu.) *
                             qt(alpha, df = nu.)) # VaR_alpha computed manually
stopifnot(all.equal(VaR.predict., VaR.predict))

## Simulate B paths
B <- 1000
set.seed(271)
X.sim.obj <- ugarchpath(fspec, n.sim = m, m.sim = B) # simulate future paths

## Compute simulated VaR_alpha and corresponding (simulated) confidence intervals
## Note: Each series is now an (m, B) matrix (each column is one path of length m)
X.sim <- fitted(X.sim.obj) # extract simulated X_t
sig.sim <- sigma(X.sim.obj) # extract sigma_t
eps.sim <- X.sim.obj@path$residSim # extract epsilon_t
VaR.sim <- (X.sim - eps.sim) + sig.sim * sqrt((nu.-2)/nu.) * qt(alpha, df = nu.) # (m, B) matrix
VaR.CI <- apply(VaR.sim, 1, function(x) quantile(x, probs = c(0.025, 0.975)))

## Setup
yran <- range(X, # simulated path
              mu., VaR., # fitted conditional mean and VaR_alpha
              mu.predict, VaR.predict, VaR.CI) # predicted mean, VaR and CIs
myran <- max(abs(yran))
yran <- c(-myran, myran) # y-range for the plot
xran <- c(1, length(X) + m) # x-range for the plot

mu.predict <- fitted(pred) # extract predicted X_t (= conditional mean mu_t; note: E[Z] = 0)
sig.predict <- sigma(pred) # extract predicted sigma_t
VaR.predict <- as.numeric(quantile(pred, probs = alpha)) # corresponding predicted VaR_alpha


## Simulated (original) data (X_t), fitted conditional mean mu_t and VaR_alpha
plot(X, type = "l", xlim = xran, ylim = yran, xlab = "Time t", ylab = "",
     main = "Simulated ARMA-GARCH, fit, VaR, VaR predictions and CIs")
lines(as.numeric(mu.), col = adjustcolor("darkblue", alpha.f = 0.5)) # hat{\mu}_t
lines(VaR., col = "darkred") # estimated VaR_alpha
mtext(paste0("Expected exceed.: ",btest$expected.exceed,"   ",
             "Actual exceed.: ",btest$actual.exceed,"   ",
             "Test: ", btest$cc.Decision),
      side = 4, adj = 0, line = 0.5, cex = 0.9) # label

## Predictions
t. <- length(X) + seq_len(m) # future time points
lines(t., mu.predict, col = "blue") # predicted process X_t (or mu_t)
lines(t., VaR.predict, col = "red") # predicted VaR_alpha
lines(t., VaR.CI[1,], col = "orange") # lower 95%-CI for VaR_alpha
lines(t., VaR.CI[2,], col = "orange") # upper 95%-CI for VaR_alpha
legend("bottomright", bty = "n", lty = rep(1, 6), lwd = 1.6,
       col = c("black", adjustcolor("darkblue", alpha.f = 0.5), "blue",
               "darkred", "red", "orange"),
       legend = c(expression(X[t]), expression(hat(mu)[t]),
                  expression("Predicted"~mu[t]~"(or"~X[t]*")"),
                  substitute(widehat(VaR)[a], list(a = alpha)),
                  substitute("Predicted"~VaR[a], list(a = alpha)),
                  substitute("95%-CI for"~VaR[a], list(a = alpha))))



