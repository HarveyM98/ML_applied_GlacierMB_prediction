
# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(GGally)
library(broom)
library(readr)
library(forecast)
library(tseries)
library(car)

# DATA --------------------------------------------------------------------

df <- read_csv("ERA5/Datos Finales/Datos_merged_ERA5.csv")
y <- df$MB
y
X <- df[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","2mtemp","sltemp4",
                          "sltemp1","skntemp","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X


# MULTIPLE LINEAR REGRESSION ----------------------------------------------


# Preliminaries -----------------------------------------------------------
df <- df[,c("ppt","srfcltnthtflx","2mdptemp",
            "srfcpress","2mtemp","sltemp4",
            "sltemp1","skntemp","sltemp3","sltemp2","MB")]
df
ggpairs(df)
plot(df)

# The Model ---------------------------------------------------------------
model <- lm(y ~ X)
summary(model)
plot(model)
durbinWatsonTest(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae

# Variables selection confidence intervals -----------------------------------------------------
#Drop 1 (ppt)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","2mtemp","sltemp4",
           "sltemp1","skntemp","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae

#Drop 2 (sltemp3)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","2mtemp","sltemp4",
           "sltemp1","skntemp","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae

#Drop 3 (skntemp)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","2mtemp","sltemp4",
           "sltemp1","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae

#Drop 4 (sltemp1)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","2mtemp","sltemp4","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 5 (sltemp2)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","2mtemp","sltemp4")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 6 (2mtemp)
X <- df[,c("srfcltnthtflx","2mdptemp",
           "srfcpress","sltemp4")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 7 (2mdptemp)
X <- df[,c("srfcltnthtflx",
           "srfcpress","sltemp4")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 8 (sltemp4)
X <- df[,c("srfcltnthtflx",
           "srfcpress")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 9 ("srfcpress")
X <- df[,c("srfcltnthtflx")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)
#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 9 ()
#X <- df[,c("srfcpress", "ppt")]
#X <- data.matrix(X, rownames.force = NA)
#X
#model <- lm(y ~ X)
#summary(model)
#confint(model)



# Variable selection p-value ----------------------------------------------------
#Drop 1 ("sltemp4")
X <- df[,c("ppt","2mdptemp",
           "srfcpress","2mtemp","srfcltnthtflx",
           "sltemp1","skntemp","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 2 ("srfcpress")
X <- df[,c("ppt","2mdptemp","2mtemp","srfcltnthtflx",
           "sltemp1","skntemp","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 3 ("2mtemp")
X <- df[,c("ppt","2mdptemp","srfcltnthtflx",
           "sltemp1","skntemp","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 4 ("skntemp")
X <- df[,c("ppt","2mdptemp","srfcltnthtflx",
           "sltemp1","sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 5 ("sltemp1")
X <- df[,c("ppt","2mdptemp","srfcltnthtflx",
           "sltemp3","sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 6 ("sltemp3")
X <- df[,c("ppt","2mdptemp","srfcltnthtflx",
           "sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 7 ("2mdptemp")
X <- df[,c("ppt","srfcltnthtflx",
           "sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 8 ("srfcltnthtflx")
X <- df[,c("ppt",
           "sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
#Drop 9 ("ppt")
X <- df[,c("sltemp2")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae

#Drop 10 ()
X <- df[,c("ppt","srfcpress")]
X <- data.matrix(X, rownames.force = NA)
X
model <- lm(y ~ X)
summary(model)
confint(model)
predicted <- predict(model)

#ERROR METRICS
# Calculate MSE
mse <- mean((predicted - y)^2)
mse
# Calculate RMSE
rmse <- sqrt(mse)
rmse
# Calculate MAE
mae <- mean(abs(predicted - y))
mae
# ARIMA MODEL -------------------------------------------------------------
MB_ts = ts(df$MB, start = 1995, end=2017, frequency = 1)
MB_ts
plot(MB_ts)
adf.test(MB_ts)

forecast:: tsdisplay(MB_ts)

acf(MB_ts)
pacf(MB_ts)

#fcast <- forecast = predict(model,h=1,newxreg=X)
#autoplot(forecast(fit))

fit = auto.arima(y,xreg=scale(X))
print(summary(fit))
checkresiduals(fit)
confint(fit)


# Variables Selection -----------------------------------------------------

#Drop 1
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","2mtemp","sltemp4",
                          "sltemp1","skntemp","sltemp2")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 2
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","2mtemp","sltemp4",
                          "sltemp1","sltemp2")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 3
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","2mtemp","sltemp4",
                          "sltemp1")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 4
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","2mtemp","sltemp4")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 5
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx","2mdptemp",
                          "srfcpress","sltemp4")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 6
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx",
                          "srfcpress","sltemp4")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 7
X <- Datos_merged_ERA5[,c("ppt","srfcltnthtflx",
                          "srfcpress")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#Drop 8
X <- Datos_merged_ERA5[,c("ppt",
                          "srfcpress")]

X <- data.matrix(X, rownames.force = NA)
fit = auto.arima(y,xreg=scale(X))
checkresiduals(fit)
print(summary(fit))
confint(fit)


# ARIMAs with two variables ----------------------------------------------------------------

X <- Datos_merged_ERA5[,c("ppt",
                          "srfcpress")]

X <- data.matrix(X, rownames.force = NA)

#ARIMA(1,0,0)
fit = Arima(y,xreg=scale(X), order=c(1,0,0))
checkresiduals(fit)
print(summary(fit))
confint(fit)

#ARIMA(0,0,1)
fit = Arima(y,xreg=scale(X), order=c(0,0,1))
checkresiduals(fit)
print(summary(fit))
confint(fit)
#ARIMA(1,0,1)
fit = Arima(y,xreg=scale(X), order=c(1,0,1))
checkresiduals(fit)
print(summary(fit))
confint(fit)


# FORECAST ----------------------------------------------------------------

#plot(fit$fitted)
#lines(y,col='red',type = 'l')

#fit1 = auto.arima(y_train,xreg=scale(X_train))

#pp=forecast(fit1,xreg=data.matrix(scale(X_test), rownames.force = NA),h=10)


#plot(pp$fitted)
#lines(y,col='red',type = 'l')



train <- cbind(y_train, X_train)
test <- cbind(y_test, X_test)
fit1 <- auto.arima(train[,1], xreg = scale(train[,-1]))
forecast1 <- forecast(fit1, xreg = scale(test[,-1]), h = nrow(test))

plot(forecast1)

train <- cbind(y_train, X_train)
test <- cbind(y_test, X_test)
fit1 <- Arima(train[,1], xreg = scale(train[,-1]), order=c(1,0,1))
forecast1 <- forecast(fit1, xreg = scale(test[,-1]), h = nrow(test))

plot(forecast1)

train <- cbind(y_train, X_train)
test <- cbind(y_test, X_test)
fit1 <- Arima(train[,1], xreg = scale(train[,-1]), order=c(0,0,1))
forecast1 <- forecast(fit1, xreg = scale(test[,-1]), h = nrow(test))

plot(forecast1)
