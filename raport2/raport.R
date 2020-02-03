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

# AUTOKORELACJA
acf(serie,100, main = "Funkcja autokorelacji")
pacf(serie,100, main = "Funkcja częściowej autokorelacji")

m=apply(matrix(serie,nr=12),2,mean)
v=apply(matrix(serie,nr=12),2,var)
plot(m,v,xlab="Średnia roczna", ylab="Wariancja roczna")
abline(lm(v~m),col=6,lty=3,lwd=2)
summary(lm(v~m))

boxplot(serie~floor(time(serie)), xlab = "Lata", ylab="Opady [mm]")

