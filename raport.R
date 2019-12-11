rm(list=ls())
library(moments)
library(tseries)

data <- read.table("D:/STUDIA/kaszcz/raport/kaszcz/data.csv",header=TRUE,sep=",")

data$Height <- data$Height * 2.54
data$Weight <- data$Weight * 0.45

data_female <- data[data$Gender=="Female",]
#data_male<-data[data$Gender=="Male",]

height <- data_female$Height
weight <- data_female$Weight

par(mfrow=c(1,2)) 

plot(weight)
plot(height)

boxplot(weight, col="yellow", ylab="Waga [kg]")
boxplot(height, col="orange", ylab="Wzrost [cm]")

par(mfrow=c(1,1)) 

stats_weight <- c(mean(weight), var(weight), sd(weight), sd(weight)/mean(weight), 
                min(weight), max(weight), quantile(weight, 0.25),
                quantile(weight, 0.5), quantile(weight, 0.75),
                skewness(weight), kurtosis(weight))

stats_height <- c(mean(height), var(height), sd(height), sd(height)/mean(height), 
                  min(height), max(height), quantile(height, 0.25),
                  quantile(height, 0.5), quantile(height, 0.75),
                  skewness(height), kurtosis(height))

#regresja

data_sorted <- data_female[order(data_female$Weight),]
data_test <- data_sorted[seq(1,5000,2),]
data_model <- data_sorted[seq(2,5001,2),]

x <- data_model$Weight
y <- data_model$Height

weight_test <- data_test$Weight
height_test <- data_test$Height

stats_x <- c(mean(x), var(x), sd(x), sd(x)/mean(x), min(x), max(x),skewness(x), kurtosis(x))

stats_y <- c(mean(y), var(y), sd(y), sd(y)/mean(y), min(y), max(y), skewness(y), kurtosis(y))


fit <- lm(y~x)
plot(x, y)
lines(x, predict(fit), col="red")

b0 <- fit$coefficients[1]
b1 <- fit$coefficients[2]

summary(fit)

mse <- mean((predict(fit)-height_model)^2)
rmse <- sqrt(mse)
r_sq_model <- summary(fit)$r.squared


# predykcja
alpha <- 0.05
height_pred <- b1*weight_test+b0
n <- length(x)
S <- sqrt(sum(height_pred-height_test)^2/(n-2))
x_avg <- mean(x)
q <- qt(1-alpha/2, n-2)
m <- sum((x - x_avg)^2)
lwr<-0
upr<-0

for (i in 1:n) {
  lwr[i] <- height_pred[i] - q*S*sqrt(1+1/n+(weight_test[i] - x_avg)^2/m)
  upr[i] <- height_pred[i] + q*S*sqrt(1+1/n+(weight_test[i] - x_avg)^2/m)
}

plot(x, y,cex=1, pch=1, col="orange")
lines(weight_test, height_test, type="p",cex=1, pch=1, col="black")
lines(x, predict(fit), col="red")
lines(weight_test, lwr, col="green")
lines(weight_test, upr, col="green")

c <- 0
for (i in 1:n) {
  if (height_test[i]<=upr[i] && height_test[i]>=lwr[i]){
    c <- c+1
  }
}
c/n


#analiza residuum
e <- residuals(fit)
normal <- rnorm(10^6, mean=mean(e), sd=sd(e))
ks.test(e, normalny)

plot(density(e))
lines(density(normal), col="purple")

qqplot(e,normal)

shapiro.test(e)

