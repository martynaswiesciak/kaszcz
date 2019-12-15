rm(list=ls())
graphics.off()

library(moments)
library(tseries)

data <- read.table("D:/STUDIA/kaszcz/raport/kaszcz/data.csv",header=TRUE,sep=",")

# zamiana na cm i kg
data$Height <- data$Height * 2.54
data$Weight <- data$Weight * 0.45

par(mfrow=c(1,2)) 
plot(data$Weight, xlab = "Indeks", ylab="Waga [kg]")
plot(data$Height, xlab = "Indeks", ylab="Wzrost [cm]")

# wybór tylko obserwacji dla kobiet
data_female <- data[data$Gender=="Female",]

height <- data_female$Height
weight <- data_female$Weight

# wykresy pude³kowe
boxplot(weight, col="yellow", ylab="Waga [kg]")
boxplot(height, col="orange", ylab="Wzrost [cm]")
par(mfrow=c(1,1)) 


# statystyki dla próbek
stats_weight <- c(mean(weight), var(weight), sd(weight), sd(weight)/mean(weight), 
                min(weight), max(weight), quantile(weight, 0.25),
                quantile(weight, 0.5), quantile(weight, 0.75),
                skewness(weight), kurtosis(weight))

stats_height <- c(mean(height), var(height), sd(height), sd(height)/mean(height), 
                  min(height), max(height), quantile(height, 0.25),
                  quantile(height, 0.5), quantile(height, 0.75),
                  skewness(height), kurtosis(height))

####### REGRESJA #######
data_sorted <- data_female[order(data_female$Weight),] # dane posortowane po wadze
# wybór co drugiej obserwacji do modelu i testu
data_test <- data_sorted[seq(1,5000,2),]
data_model <- data_sorted[seq(2,5001,2),]

x <- data_model$Weight
y <- data_model$Height

weight_test <- data_test$Weight
height_test <- data_test$Height

# statystyki dla próbki "modelowej"
stats_x <- c(mean(x), var(x), sd(x), sd(x)/mean(x), min(x), max(x),skewness(x), kurtosis(x))
stats_y <- c(mean(y), var(y), sd(y), sd(y)/mean(y), min(y), max(y), skewness(y), kurtosis(y))

# zastosowanie funkcji dopasowuj¹cej model, odczytanie wartoœci estymatorów
fit <- lm(y~x)
summary(fit)

b0 <- fit$coefficients[1]
b1 <- fit$coefficients[2]

plot(x, y, xlab = "Waga [kg]", ylab = "Wzrost [cm]")
lines(x, predict(fit), col="red")

mse <- mean((predict(fit)-y)^2)
rmse <- sqrt(mse)
r_sq_model <- summary(fit)$r.squared

# predykcja
alpha <- 0.05
height_pred <- b1*weight_test+b0 # wartoœci wynikaj¹ce z modelu dla próbki testowej
n_model <- length(x)
n_test <- 5000 - n_model
S <- sqrt(sum((height_pred-height_test)^2)/(n_test - 2)) # estymator wariancji
x_avg <- mean(x)
q <- qt(1-alpha/2, n_model-2)
m <- sum((x - x_avg)^2)

lwr<-0
upr<-0

# przedzia³y ufnoœci predykcji
for (i in 1:n_test) {
  lwr[i] <- height_pred[i] - q*S*sqrt(1+1/n_model+(weight_test[i] - x_avg)^2/m)
  upr[i] <- height_pred[i] + q*S*sqrt(1+1/n_model+(weight_test[i] - x_avg)^2/m)
}

plot(x, y,cex=1, pch=1, col="orange", xlab = "Waga [kg]", ylab = "Wzrost [cm]", xlim=c(min(weight), max(weight)), ylim=c(min(height), max(height)))
lines(weight_test, height_test, type="p",cex=1, pch=1, col="black")
lines(weight_test, height_pred, col="blue")
lines(weight_test, lwr, col="green")
lines(weight_test, upr, col="green")
legend("bottomright",legend=c("Dane modelowe", "Dane testowe", "Predykcja na danych testowych", "Przedzia³y ufnoœci predykcji"),
       col=c("orange", "black", "blue", "green"),lty=c(0,0,1,1),pch = c(1,1,NA,NA), text.font=6, cex=0.7) 

# test czy faktycznie 95% obserwacji wpada w przedzia³ ufnoœci
c <- 0
for (i in 1:n_test) {
  if (height_test[i]<=upr[i] && height_test[i]>=lwr[i]){
    c <- c+1
  }
}
c/n_test # = 0.9472, odpowiedni wynik

# analiza residuum
e <- residuals(fit)
plot(e, xlab="Indeks", ylab="", main="Residua w dopasowanym modelu regresji", cex.main=0.9, cex.lab=0.9)

e_avg <- mean(e)
e_sd <- sd(e)

# porównanie z gêstoœci¹, dystrybuant¹ i kwantylami rozk³adu normalnego
plot(density(e), xlab="", ylab="", main="", ylim=c(0, 0.132))
lines(seq(-15, 15, 0.001), dnorm(seq(-15, 15, 0.001), e_avg, e_sd), col="purple")
legend("topleft", legend = c("Estymator gêstoœci", "Gêstoœæ rozk³. normalnego"), col=c("black", "purple"), lty=1, cex=0.7)

plot(ecdf(e), xlab="", ylab="", main="")
lines(seq(-15, 15, 0.001), pnorm(seq(-15, 15, 0.001), e_avg, e_sd), col="purple")
legend("topleft", legend = c("Dystrybuanta empiryczna", "Dystrybuanta rozk³. normalnego"), col=c("black", "purple"), lty=1, cex=0.7)

qqplot(e, rnorm(10^6, mean=e_avg, sd=e_sd), xlab="Residua", ylab = "Rozk³ad normalny")

# test Ko³mogorowa-Smirnova z metod¹ Monte Carlo
ks_p <- 0

for (i in 1:10) { # by³o wykonane dla 500 ale wolno idzie
  normal <- rnorm(10^6, mean=e_avg, sd=e_sd)
  ks <- ks.test(e, normal)
  ks_p[i] <- ks$p.value
}
mean(ks_p) # œrednia p-wartoœæ dla KS-testu

# test Shapiro-Wilka
shapiro.test(e)

# test Jarque-Bera
jarque.bera.test(e)




