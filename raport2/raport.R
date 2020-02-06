rm(list=ls())
graphics.off()

library(forecast)
library(tseries)

data <- read.csv('/Users/martynaswiesciak/Desktop/Studia/sem5/KASZCZ/kaszcz/raport2/bangladesz.csv')
serie.all <- ts(data$rain, start = c(1901, 1), end = c(2015, 12), frequency = 12)
start(serie.all)

# PODZIA? DANYCH NA PRÓBKĘ TESTOWĄ I DO PREDYKCJI
serie.all <- window(serie.all, start=c(1960, 1))
serie <- window(serie.all,end=c(1984,12)) # dane testowe
plot(serie, main='Opady w Bangladeszu', xlab='lata', ylab='opady [mm]') 
abline(v=1960:1985, col=50, lty=3)

dane.test <- window(serie.all,start=c(1985,1), end=c(1990,12)) # dane do predykcji

# acf i pacf
acf(serie,100, main = "Funkcja autokorelacji")
pacf(serie,100, main = "Funkcja częściowej autokorelacji")

#badanie statystyk
m=apply(matrix(serie,nr=12),2,mean)
v=apply(matrix(serie,nr=12),2,var)
plot(m,v,xlab="Średnia roczna", ylab="Wariancja roczna")
abline(lm(v~m),col=6,lty=3,lwd=2)
summary(lm(v~m)) # statystyki liczbowe

# boxplot dla każdego roku
boxplot(serie~floor(time(serie)), xlab = "Lata", ylab="Opady [mm]")

# Wykresy srednich rocznych i wariancji
plot(1960:1984, m, main="Średnia rocznie", xlab='lata', ylab= '')
plot(1960:1984, v, main="Wariancja rocznie", xlab='lata', ylab='')

# TRANSFORMACJA DANYCH - nakładamy pierwiastek kwadratowy
sqrtserie <- sqrt(serie)
acf(sqrtserie,100, main = "Funkcja autokorelacji")
pacf(sqrtserie,100, main = "Funkcja częściowej autokorelacji")

plot(sqrtserie, main='', xlab='lata', ylab='') 
abline(v=1960:1985, col=6, lty=3)


sqrtm=apply(matrix(sqrtserie,nr=12),2,mean)
sqrtv=apply(matrix(sqrtserie,nr=12),2,var)
plot(sqrtm,sqrtv,xlab="Średnia roczna", ylab="Wariancja roczna")
abline(lm(sqrtv~sqrtm),col=6,lty=3,lwd=2)
summary(lm(sqrtv~sqrtm)) # statystyki liczbowe

boxplot(serie~floor(time(sqrtserie)), xlab = "Lata", ylab="Opady [mm]")

plot(1960:1984, sqrtm, main="Średnia rocznie", xlab='lata', ylab= '')
plot(1960:1984, sqrtv, main="Wariancja rocznie", xlab='lata', ylab='')

# STACJONARNOŚĆ 
adf.test(serie)
kpss.test(serie)

adf.test(sqrtserie)
kpss.test(sqrtserie)

# DEKOMPOZYCJA
plot(decompose(serie))
plot(decompose(sqrtserie))

par(mfrow=c(2,1))
monthplot(serie,main="Dane miesięcznie", ylab="")
monthplot(sqrtserie,main="Dane spierwiastkowane miesięcznie", ylab="")

# SEZONOWOŚĆ 
sqrtserie.d12<-diff(sqrtserie,12)
tsdisplay(sqrtserie.d12, main=expression("Szereg X'"[t]*" = X"[t]-"X"[t-12]))
sqrtserie.d12.d12<-diff(sqrtserie.d12,12)
tsdisplay(sqrtserie.d12.d12, main=expression("Szereg X''"[t]*" = X'"[t]-"X'"[t-12]))
sqrtserie.d12.d12.d12<-diff(sqrtserie.d12.d12,12)
tsdisplay(sqrtserie.d12.d12.d12, main=expression("Szereg X'''"[t]*" = X''"[t]-"X''"[t-12]))

var(sqrtserie)
var(sqrtserie.d12)
var(sqrtserie.d12.d12)
var(sqrtserie.d12.d12.d12)

# MA(1) --> P = 1, p = 1
# AR(0) --> q = 0, Q = 2
acf(sqrtserie.d12, ylim=c(-1,1), col=c(2,rep(1,11)), lwd=1, lag.max=84, main=expression("Autokorelacja szeregu X'"[t]*" = X"[t]-"X"[t-12]))
pacf(sqrtserie.d12, ylim=c(-1,1), col=c(rep(1,11),2), lwd=1, lag.max=84, main=expression("Częściowa autokorelacja szeregu X'"[t]*" = X"[t]-"X"[t-12]))

# PORÓWNANIE MODELI
mod=arima(sqrtserie,order=c(0,0,1),seasonal=list(order=c(0,1,2),period=12))
plot(sqrtserie, xlab="", ylab="", main = "Dane rzeczywiste vs dopasowany model nr 1")
lines(fitted(mod), col="red")
legend("topright",legend=c("Dane rzeczywiste", "Dopasowany szereg"),
       col=c("black", "red"),lty=c(1,1), text.font=1, cex=0.6) 

summary(mod)
BIC(mod)

arima.auto.bic<-auto.arima(sqrtserie, ic="bic", allowdrift = FALSE)
plot(sqrtserie, xlab="", ylab="", main = "Dane rzeczywiste vs dopasowany model nr 2")
lines(fitted(arima.auto.bic), col="magenta")
legend("topright",legend=c("Dane rzeczywiste", "Dopasowany szereg"),
       col=c("black", "magenta"),lty=c(1,1), text.font=1, cex=0.6) 
summary(arima.auto.bic)

arima.auto.aic<-auto.arima(sqrtserie, ic="aic", allowdrift = FALSE)
summary(arima.auto.aic)
plot(sqrtserie, xlab="", ylab="", main = "Dane rzeczywiste vs dopasowany model nr 3")
lines(fitted(arima.auto.bic), col="blue")
legend("topright",legend=c("Dane rzeczywiste", "Dopasowany szereg"),
       col=c("black", "blue"),lty=c(1,1), text.font=1, cex=0.6) 


# WALIDACJA MODELI
mod.reszty<-mod$residuals
plot(mod.reszty)
acf(mod.reszty,50)
pacf(mod.reszty,50)
acf(mod.reszty^2,50)
tsdiag(mod,gof.lag=24)
Box.test(mod.reszty,type="Ljung-Box",lag=2,fitdf=1)

arima.auto.bic.reszty<-arima.auto.bic$residuals
plot(arima.auto.bic.reszty)
acf(arima.auto.bic.reszty,50)
pacf(arima.auto.bic.reszty,50)
acf(arima.auto.bic.reszty^2,50)
tsdiag(arima.auto.bic,gof.lag=24)
Box.test(arima.auto.bic.reszty,type="Ljung-Box",lag=2,fitdf=1)

arima.auto.aic.reszty<-arima.auto.aic$residuals
plot(arima.auto.aic.reszty)
acf(arima.auto.aic.reszty,50)
pacf(arima.auto.aic.reszty,50)
acf(arima.auto.aic.reszty^2,50)
tsdiag(arima.auto.aic.reszty,gof.lag=24)
Box.test(arima.auto.aic.reszty,type="Ljung-Box",lag=2,fitdf=1)

# RESIDUA - ANALIZA
qqnorm(mod.reszty)
qqline(mod.reszty,col=2,lwd=2)

hist(mod.reszty,breaks=20,freq=F, main='Histogram', xlab='', ylab='Gęstość')
curve(dnorm(x,mean=mean(mod.reszty),sd=sd(mod.reszty)),col=2,add=T)


qqnorm(arima.auto.bic.reszty)
qqline(arima.auto.bic.reszty,col=2,lwd=2)

hist(arima.auto.bic.reszty,breaks=20,freq=F, main='Histogram', xlab='', ylab='Gęstość')
curve(dnorm(x,mean=mean(arima.auto.bic.reszty),sd=sd(arima.auto.bic.reszty)),col=2,add=T)


qqnorm(arima.auto.aic.reszty)
qqline(arima.auto.aic.reszty,col=2,lwd=2)

hist(arima.auto.aic.reszty,breaks=20,freq=F, main='Histogram', xlab='', ylab='Gęstość')
curve(dnorm(x,mean=mean(arima.auto.aic.reszty),sd=sd(arima.auto.aic.reszty)),col=2,add=T)

# PREDYKCJA
par(mfrow=c(1,1))

prognoza.mod<-forecast(mod,h=6*12)
plot(prognoza.mod,main="Prognoza modelu nr 1")
lines(sqrt(dane.test), col="red")
lines(sqrtserie,col='red')
accuracy(mod)
#accuracy(mod,sqrt(dane.test))
summary(prognoza.mod)

prognoza.arima.auto.bic<-forecast(arima.auto.bic,h=6*12)
plot(prognoza.arima.auto.bic,main="Prognoza modelu nr 2")
lines(sqrt(dane.test), col="red")
lines(sqrtserie,col='red')
accuracy(prognoza.arima.auto.bic)
accuracy(prognoza.arima.auto.bic,sqrt(dane.test))


prognoza.arima.auto.aic<-forecast(arima.auto.aic,h=6*12)
plot(prognoza.arima.auto.aic,main="Prognoza modelu nr 3")
lines(sqrt(dane.test), col="red")
lines(sqrtserie,col='red')
accuracy(prognoza.arima.auto.aic)
accuracy(prognoza.arima.auto.aic,sqrt(dane.test))

#?rednia szeroko?? przedzia?u ufno?ci
mean(prognoza.mod$upper[1:72,2]-prognoza.mod$lower[1:72,2])
mean(prognoza.arima.auto.bic$upper[1:72,2]-prognoza.arima.auto.bic$lower[1:72,2])
mean(prognoza.arima.auto.aic$upper[1:72,2]-prognoza.arima.auto.aic$lower[1:72,2])



ultim=c(1984,12)
pred=predict(mod,n.ahead=6*12)
pr<-ts(c(tail(sqrtserie,1),pred$pred),start=ultim,freq=12)
se<-ts(c(0,pred$se),start=ultim,freq=12)

# 
# 
#Intervals
# tl1<-ts((pr-1.96*se)^2,start=ultim,freq=12)
# tu1<-ts((pr+1.96*se)^2,start=ultim,freq=12)
# pr1<-ts((pr)^2,start=ultim,freq=12)
# 
# ts.plot(serie,tl1,tu1,pr1,lty=c(1,2,2,1),col=c(1,4,4,2),type="l",main="Model 1 ")
# abline(v=1949+0:13,lty=3,col=4)
# lines(serie.all)
# 
# ultim=c(1960,1)
# se<-ts(c(0,mod$se),start=ultim,freq=12)
# tl1<-ts((fitted(mod)-1.96*se)^2,start=ultim,freq=12)
# tu1<-ts((fitted(mod)+1.96*se)^2,start=ultim,freq=12)
# ts1<-ts((fitted(mod))^2,start=ultim,freq=12)
# 
# ts.plot(serie,lty=1, col=1, type="l")
# lines(tl1, lty=2, col="4")
# 
# #ts.plot(serie,tl1,tu1,ts1,lty=c(1,2,2,1),col=c(1,4,4,2),type="l",main="Model 1 ")
# abline(v=1949+0:13,lty=3,col=4)
# lines(serie.all)







