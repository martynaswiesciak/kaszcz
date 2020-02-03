rm(list=ls())
graphics.off()

library(forecast)
library(tseries)

data <- read.csv('/Users/martynaswiesciak/Desktop/Studia/sem5/KASZCZ/kaszcz/raport2/bangladesz.csv')
serie.all <- ts(data$rain, start = c(1901, 1), end = c(2015, 12), frequency = 12)
start(serie.all)

# PODZIAŁ DANYCH NA PRÓBKĘ TESTOWĄ I DO PREDYKCJI
serie.all <- window(serie.all, start=c(1960, 1))
serie <- window(serie.all,end=c(1984,12)) # dane testowe
plot(serie, main='Opady w Bangladeszu', xlab='lata', ylab='opady [mm]') 
abline(v=1960:1985, col=50, lty=3)

dane.test <- window(serie.all,start=c(1985,1)) # dane do predykcji

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

# MA(1) --> P=1, p=1
# AR(0) --> q = 0
acf(sqrtserie.d12, ylim=c(-1,1), col=c(2,rep(1,11)), lwd=1, lag.max=84, main=expression("Autokowariancja szeregu X'"[t]*" = X"[t]-"X"[t-12]))
pacf(sqrtserie.d12, ylim=c(-1,1), col=c(rep(1,11),2), lwd=1, lag.max=84, main=expression("Częściowa autokowariancja szeregu X'"[t]*" = X"[t]-"X"[t-12]))

# PORÓWNANIE MODELI
mod=arima(sqrtserie,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
summary(mod)

arima.auto.bic<-auto.arima(sqrtserie, ic="bic")
summary(arima.auto.bic)

arima.auto<-auto.arima(sqrtserie, ic="aic")
summary(arima.auto)

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

arima.auto.reszty<-arima.auto$residuals
plot(arima.auto.reszty)
acf(arima.auto.reszty,50)
pacf(arima.auto.reszty,50)
acf(arima.auto.reszty^2,50)
tsdiag(arima.auto,gof.lag=24)
Box.test(arima.auto.reszty,type="Ljung-Box",lag=2,fitdf=1)


