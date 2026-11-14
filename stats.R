#Exercise 2
set.seed(124)
engine <- rnorm(100, 2.0, 0.5)
horsepower <- engine*50 + rnorm(100, 0, 5)
weight <- horsepower*10 + rnorm(100, 0, 50)
mpg <- 50 - 5*engine - 0.01*weight + rnorm(100, 0, 2)
data2 <- data.frame(mpg, engine, horsepower, weight)

model_full <- lm(mpg ~ engine + horsepower + weight, data = data2)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

model.ref <- lm(mpg ~ engine + weight)
vif(model.ref)

par(mfrow=c(2,2))
plot(model.ref)
summary(model.ref)

#Exercise 3
set.seed(125)
study <- rnorm(100, 5, 2)
attendance <- study*2 + rnorm(100, 0, 1)
sleep <- rnorm(100, 7, 1)
grade <- 50 + 5*study + 3*attendance + 2*sleep + rnorm(100, 0, 5)
data3 <- data.frame(grade, study, attendance, sleep)

model_full <- lm(grade ~ sleep + attendance + study, data = data3)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

model.ref <- lm(grade ~ sleep + attendance)
vif(model.ref)
par(mfrow=c(2,2))
plot(model.ref)
summary(model.ref)

#Exercise 4
set.seed(126)
tv <- rnorm(100, 200, 50)
radio <- tv*0.5 + rnorm(100, 0, 10)
social <- tv*0.3 + rnorm(100, 0, 15)
sales <- 10 + 0.05*tv + 0.04*radio + 0.03*social + rnorm(100, 0, 2)
data4 <- data.frame(sales, tv, radio, social)

model_full <- lm(sales ~ social + radio + tv, data = data4)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

model.ref <- lm(sales ~ tv + social)
vif(model.ref)
par(mfrow=c(2,2))
plot(model.ref)
summary(model.ref)

#Exercise 5
set.seed(127)
income <- rnorm(100, 50000, 10000)
interest <- income/10000 + rnorm(100,0,0.5)
demand <- 200 + 0.01*income - 5*interest + rnorm(100,0,10)
data5 <- data.frame(demand, income, interest)

model_full <- lm(demand ~ income + interest, data = data5)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)

#Exercise 6
set.seed(128)
temp <- rnorm(100, 20, 5)
humidity <- temp*2 + rnorm(100,0,3)
energy <- 100 - 2*temp + 0.5*humidity + rnorm(100,0,5)
data6 <- data.frame(energy, temp, humidity)

model_full <- lm(energy ~ temp + humidity, data = data6)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)

#Exercise 7
set.seed(129)
experience <- rnorm(100, 10, 5)
education <- experience*0.8 + rnorm(100,0,1)
salary <- 30000 + 2000*experience + 1500*education + rnorm(100,0,5000)
data7 <- data.frame(salary, experience, education)

model_full <- lm(salary ~ education + experience, data = data7)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)

#Exercise 8
set.seed(130)
labor <- rnorm(100, 50, 10)
capital <- labor*1.5 + rnorm(100,0,5)
output <- 100 + 2*labor + 3*capital + rnorm(100,0,10)
data8 <- data.frame(output, labor, capital)

model_full <- lm(output ~ capital + labour, data = data8)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)

#Exercise 9
set.seed(131)
exercise <- rnorm(100, 3, 1)
diet <- exercise*1.2 + rnorm(100,0,0.5)
health <- 50 + 5*exercise + 4*diet + rnorm(100,0,3)
data9 <- data.frame(health, exercise, diet)

model_full <- lm(health ~ diet + exercise, data = data9)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)

#Exercise 10
set.seed(132)
debt <- rnorm(100, 10000, 3000)
income <- debt*2 + rnorm(100,0,2000)
risk <- 0.5*debt - 0.3*income + rnorm(100,0,1000)
data10 <- data.frame(risk, debt, income)

model_full <- lm(risk ~ income + debt , data = data10)
summary(model_full)
library(car)
vif(model_full)
library(MASS)
model_step<- stepAIC(model_full)
model_step

par(mfrow=c(2,2))
plot(model.ref)


