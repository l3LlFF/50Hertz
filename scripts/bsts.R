library(data.table)
library(prophet)
library(tidyr)
library(tidyverse)
library(rstan)
library(bsts)
library(rstan)
library(hydroGOF)
library(MLmetrics)
library(imputeTS)
library(anomalize)
library(writexl)
library(feasts)
library(slider)
library(tsibble)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(readr)
library(tibble)

metrics <- function(y_valid, y_pred) {
  return (data.table(
    MSE=mse(y_pred, y_valid),
    RMSE=rmse(y_pred, y_valid),
    MAE=mae(y_pred, y_valid),
    MAPE=mean(abs((y_valid-y_pred)/y_valid)),
    CP=cp(y_pred, y_valid),
    KGE=KGE(y_pred, y_valid)
  ))
}

data <- fread('data_bsts.csv')

data_train <- data %>% 
  slice(1:(n() - 3 * 24))

data_test <- data %>% 
  tail(3 * 24)

dt_train <- data_train %>% pull(dt)
y_train <- data_train %>% pull(y)

ss <- list()
ss <- AddLocalLinearTrend(ss, y_train)
ss <- AddAutoAr(ss, y_train, lag=7)
model <- bsts(y ~ t + school + school_flag +
                workplace + workplace_flag + 
                stay_home + stay_home_flag + borders +
                P0 + U + Ff + VV +
                Td + is_light + height + clouds_left +
                clouds_right, 
              ss,
              data = data_train,
              timestamps = dt_train, 
              niter = 1500, seed = 42, ping = 50)

model_pred <- predict(model, horizon = 3,
                      newdata = data_test)

AcfDist(residuals(model))

plot(model_pred, plot.original = 90)
with(data_test, points(dt, y, pch = 19, col = "yellow"))

metrics(data_test$y, model_pred$median)

plot(model, y = "coefficients")

