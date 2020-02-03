#####################################################################
library(forecast)
serie.all<-AirPassengers

start(serie.all)
end(serie.all)
serie<-window(serie.all,end=c(1958,12))
dane.test<-window(serie.all,start=c(1959,1))



plot(serie,main="Iloœæ pasa¿erów",type="o")
abline(v=1949:1960,col=4,lty=3)
acf(serie,100)
acf(serie,18)
pacf(serie,18)

m=apply(matrix(serie,nr=12),2,mean)
v=apply(matrix(serie,nr=12),2,var)
par(mfrow=c(2,1))
plot(m,v,xlab="Mean anualy",ylab="Variance anualy",main="serie")
abline(lm(v~m),col=2,lty=3,lwd=2)

##Boxplot for each year
boxplot(serie~floor(time(serie)))

#Rysunki œrednich rocznych i wariancji
plot(1949:1958,m,main="œrednia rocznie")
plot(1949:1958,v,main="wariancja rocznie")


par(mfrow=c(1,1))
# Logarithmic transformation in order to deal with non constant varaince
lnserie=log(serie)
plot(lnserie,main="logarithmized data")

boxplot(lnserie~floor(time(lnserie)))


m.log=apply(matrix(lnserie,nr=12),2,mean)
v.log=apply(matrix(lnserie,nr=12),2,var)

#Rysunki œrednich rocznych i wariancji
plot(1949:1958,m.log,main="œrednia rocznie")
plot(1949:1958,v.log,main="wariancja rocznie")

par(mfrow=c(1,1))
plot(m.log,v.log)

##Decomposition
plot(decompose(lnserie))
plot(decompose(serie))



par(mfrow=c(2,1))
monthplot(serie,main="dane")
monthplot(lnserie,main="dane log")

tsdisplay(serie)
tsdisplay(lnserie)

library(tseries)
adf.test(serie)
kpss.test(serie)

adf.test(lnserie)

lnserie.d12<-diff(lnserie,12)
tsdisplay(lnserie.d12)
lnserie.d12.d1<-diff(lnserie.d12)
tsdisplay(lnserie.d12.d1)
lnserie.d12.d1.d1<-diff(lnserie.d12.d1)
tsdisplay(lnserie.d12.d1.d1)

serie.d12<-diff(serie,12)
tsdisplay(serie.d12)
serie.d12.d1<-diff(serie.d12)
tsdisplay(serie.d12.d1)
serie.d12.d1.d1<-diff(serie.d12.d1)

var(lnserie)
var(lnserie.d12)
var(lnserie.d12.d1)
var(lnserie.d12.d1.d1)

var(serie)
var(serie.d12)
var(serie.d12.d1)

par(mfrow=c(1,2))
acf(lnserie.d12.d1,ylim=c(-1,1),col=c(2,rep(1,11)),lwd=1,lag.max=84)
pacf(lnserie.d12.d1,ylim=c(-1,1),col=c(rep(1,11),2),lwd=1,lag.max=84)
par(mfrow=c(1,1))


(mod=arima(lnserie,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12)))
summary(mod)

(mod1=arima(lnserie,order=c(0,1,3),seasonal=list(order=c(0,1,1),period=12)))
mod2<-arima(lnserie,order=c(5,1,0),seasonal=list(order=c(0,1,2),period=12))

arima.auto.bic<-auto.arima(lnserie,ic="bic")
arima.auto<-auto.arima(lnserie,ic="aic")

summary(arima.auto)
summary(arima.auto.bic)

summary(mod1)


mod.reszty<-mod$residuals
plot(mod.reszty,type='o')
acf(mod.reszty,50)
pacf(mod.reszty,50)
acf(mod.reszty^2,50)
tsdiag(mod,gof.lag=24)
Box.test(mod.reszty,type="Ljung-Box",lag=2,fitdf=1)


#################Validacion function#################################
validacion=function(model,dades){
  s=frequency(get(model$series))
  resid=model$residuals
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  
  plot(resid,main="Residuals")
  abline(h=0)
  abline(h=c(-3*sd(resid),3*sd(resid)),lty=3,col=4)
  
  scatter.smooth(sqrt(abs(resid)),main="Square Root of Absolute residuals",lpars=list(col=2))
  
  
  qqnorm(resid)
  qqline(resid,col=2,lwd=2)
  
  
  hist(resid,breaks=20,freq=F)
  curve(dnorm(x,mean=mean(resid),sd=sd(resid)),col=2,add=T)
  
  
  
  par(mfrow=c(1,2))
  acf(resid,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)
  par(mfrow=c(1,1))
  
  
  par(mfrow=c(1,2))
  acf(resid^2,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid^2,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)
  par(mfrow=c(1,1))
  
  
  
  
  tsdiag(model,gof.lag=7*s)
  par(mfrow=c(1,1))
  
  
}


validacion(mod,lnserie)

par(mfrow=c(1,1))
prognozy.mod3<-forecast(mod,h=24)
plot(prognozy.mod3,main="model 3")
lines(log(dane.test), col="red")
# lines(lnserie,col='red')
accuracy(prognozy.mod3)
accuracy(prognozy.mod3,log(dane.test))

names(prognozy.mod3)


ultim=c(1958,12)
pred=predict(mod,n.ahead=24)
pr<-ts(c(tail(lnserie,1),pred$pred),start=ultim,freq=12)
se<-ts(c(0,pred$se),start=ultim,freq=12)



#Intervals
tl1<-ts(exp(pr-1.96*se),start=ultim,freq=12)
tu1<-ts(exp(pr+1.96*se),start=ultim,freq=12)
pr1<-ts(exp(pr),start=ultim,freq=12)

ts.plot(serie,tl1,tu1,pr1,lty=c(1,2,2,1),col=c(1,4,4,2),type="o",main="Model 1 ")
abline(v=1949+0:13,lty=3,col=4)
lines(serie.all)