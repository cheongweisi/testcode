# volatility modelling to predict DBS stock price
library(quantmod); library(tidyverse); library(fGarch); library(forecast); library(tseries)

getSymbols("D05.SI", src="yahoo", from=as.Date("2017-03-28"), to=as.Date("2019-01-22"))
head(D05.SI)
tail(D05.SI)
nrow(D05.SI)

library(timetk)
DBS_p <- as.data.frame(D05.SI)
str(DBS_p)

DBS.c4 <- DBS_p[,4]
DBS.c <- ts(DBS.c4, start=2017, freq=270)

head(DBS.c)
tail(DBS.c)

plot(DBS.c, type="l")

# DBS log-returns
DBS.r <- diff(log(DBS.c),1)*100
plot(DBS.r, type="l")

# DBS absolute log returns
DBS.abr <- abs(DBS.r)
DBS.sqr <- DBS.r^2
plot(DBS.abr, type="l")
plot(DBS.sqr, type="l")


# ACF & PACF of log-return of dbs stock
acf(as.vector(DBS.r))
pacf(as.vector(DBS.r))

# ACF & PACF of absolute log-return & squared log-return of DBS stock
par(mfrow=c(1,2))
acf(as.vector(DBS.abr))
pacf(as.vector(DBS.abr))

acf(as.vector(DBS.sqr))
pacf(as.vector(DBS.sqr))

## using ARCH(1) to model DBS returns
garch10.ma1 <- garchFit(formula=~arma(0,0)+garch(1,0), data=DBS.r, trace=FALSE)
summary(garch10.ma1)

# view the ACF plot of the standardised residuals and standardised residuals squared
garch10.ma1.sr <- garch10.ma1@residuals/garch10.ma1@sigma.t
acf(garch10.ma1.sr)
acf(garch10.ma1.sr^2)

# predicting the returns one step ahead
pred.rtn <- predict(garch10.ma1, n.ahead=1)/100
pred.rtn

pred.rtn.1sd.ll <- pred.rtn[1,1]-2*pred.rtn[1,3]
pred.rtn.1sd.ul <- pred.rtn[1,1]+2*pred.rtn[1,3]

# predicting the price one step ahead
pred.price <- exp(pred.rtn[1,1]+log(DBS.c[457]))
pred.price.1sd.ll <- exp(pred.rtn.1sd.ll+log(DBS.c[457]))
pred.price.1sd.ul <- exp(pred.rtn.1sd.ul+log(DBS.c[457]))

DBS.c[457]

pred.price
pred.price.1sd.ul
pred.price.1sd.ll


# 22-01-2019 O:24.90 H:24.94 L:24.55 C:24.55






