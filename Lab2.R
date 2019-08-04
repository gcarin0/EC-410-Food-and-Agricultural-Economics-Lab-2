#Giancarlo Carino
#EC 410
#Lab 2, Supply Estimation

#Install packages
install.packages(pkgs=c("psych", "stargazer", "lmtest", "car", "dplyr"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(dplyr)

#Import
setwd("/Users/gc/Desktop/GitHub/EC-410-Food-and-Agricultural-Economics/Lab 2/data")
Lab2data <- read.csv("Lab2data.csv")
attach(Lab2data)

#Convert Nominal to Real prices
Lab2data$realpchicken <- (Lab2data$pchicken / Lab2data$cpi) * 177.1
Lab2data$realpcorn <- (Lab2data$pcorn / Lab2data$cpi) * 177.1

#Create per capita supply variable
Lab2data$percap_qchicken <- Lab2data$qchicken / Lab2data$population

#Order/Sort by year and month
Lab2data <- Lab2data[with(Lab2data, order(year)),]

#Create time trend variable
Lab2data$t <- time(Lab2data$percap_qchicken)
#Create a quadratic time trend "t2"
Lab2data$t2 <- Lab2data$t^2

#Convert percap_qchicken into a time series object
Lab2data$percap_qchicken <- ts(Lab2data$percap_qchicken, start=1950, end=2001, frequency=1)

#Plot quantity supplied of chicken over time
plot.ts (Lab2data$percap_qchicken, ylab="Supply of Chicken")

#Descriptive statistics

summary(Lab2data)     #summary statistics in native R package
describe(Lab2data)    #summary statistics in Psych package
stargazer(
Lab2data[c("qchicken", "realpchicken", "realpcorn")], type="text", title="Descriptive Statistics", digits=2, out="table.htm", covariate.labels=c("Quantity of Chicken Supplied", "Real Price of Chicken", "Real Price of Corn"), summary.stat=c("n", "mean", "sd", "min", "max")
)

#Estimate supply of chicken
#Regression: DV: percap_qchicken; IV: real_pchicken, real_pcorn
#Regression without time trend
reg1 <- lm(percap_qchicken ~ realpchicken + realpcorn, data = Lab2data)
summary(reg1)
#Regression with a time trend
reg2 <- lm(percap_qchicken ~ realpchicken + realpcorn + t, data = Lab2data)
summary(reg2)
#Regression with a time trend and a quadratic time trend
reg3 <- lm(percap_qchicken ~ realpchicken + realpcorn + t + t2, data = Lab2data)
summary(reg3)

#Create a dummy variable for years before 1984: y84=0 before 1984 / y84=1 starting after 1984
Lab2data$y84 <- 0
Lab2data$y84 <- ifelse(year >= 1984, 1, 0)
#Create lagged prices using dplyr
Lab2data$lag_realpchicken <- lag(Lab2data$realpchicken, n=1L)
Lab2data$lag_realpcorn <- lag(Lab2data$realpcorn, n=1L)

reg4 <- lm(percap_qchicken ~ realpchicken + realpcorn  + t + y84 + lag_realpchicken + lag_realpcorn, data = Lab2data)
summary(reg4)

#Check assumptions of linear model: reg3

#Assumption of Homoscedasticity (Violation: Heteroscedasticity)
Lab2data$residuals <- resid(reg3)
plot(Lab2data$realpchicken, Lab2data$residuals, ylab = "Residuals", xlab = "Real Price of Broilers", main = "Residuals vs the real price of Broilers")
plot(Lab2data$realpcorn, Lab2data$residuals, ylab = "Residuals", xlab = "Real Price of Corn", main = "Residuals vs the real price of Corn")
plot(Lab2data$t, Lab2data$residuals, ylab = "Residuals", xlab = "Linear time trend", main = "Residuals vs Linear time trend")
plot(Lab2data$t2, Lab2data$residuals, ylab = "Residuals", xlab = "Quadratic time trend", main = "Residuals vs Quadratic time trend")

#Test for heteroscedasticity: H0: Homoscedasticity; Ha: Heteroscedasticity
#Bresuch-Pagan Test
lmtest::bptest(reg3)

#Assumption of no autocorrelation (Violation: Autocorrelation)
#Create lag of residuals
Lab2data$lag_resid <- lag(Lab2data$residuals, n=1L)
#Plot a scatterplot of residuals vs lags
plot(Lab2data$residuals,Lab2data$lag_residuals, ylab="Residuals", xlab="Lag Residuals", main="Residuals vs their lag")
#Plot residuals vs time - try to notice a pattern
plot(Lab2data$residuals,Lab2data$ts, ylab="Residuals", xlab="Time", 
     main="Residuals vs time")

#Test for Autocorrelation. H0: No autocorrelation; Ha: Autocorrelation
#Durbin-Watson test
dwtest(reg3) 

#Assumption: No Multicollinearity (violation: Multicollinearity)
#Test for Multicollinearity using "vif" from "car" package
#H0: No Multicollinearity (if VIF<10), Ha: Multicollinearity (if VIF>10) 
#VIF - Variance Inflation Factor
car::vif(reg3)