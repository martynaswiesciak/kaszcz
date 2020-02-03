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
summary(lm(v~m))

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
summary(lm(sqrtv~sqrtm))

boxplot(serie~floor(time(sqrtserie)), xlab = "Lata", ylab="Opady [mm]")

plot(1960:1984, sqrtm, main="Średnia rocznie", xlab='lata', ylab= '')
plot(1960:1984, sqrtv, main="Wariancja rocznie", xlab='lata', ylab='')
