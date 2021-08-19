library(data.table)
library(prophet)
library(tidyr)
library(tidyverse)
library(rstan)
library(prophet)
library(rstan)
library(hydroGOF)
library(MLmetrics)
library(imputeTS)

# Prophet forecasting

custom_plot <- function(x, fcst, df_test, days = 30, uncertainty = TRUE, plot_cap = TRUE,
                        xlabel = 'ds', ylabel = 'y', ...) {
  df <- df_for_plotting(x, fcst)
  
  if (is.null(df_test)) {
    right_bound <- tail(fcst$ds, 1)
    left_bound <- head(fcst$ds, 1)
  } else {
    right_bound <- tail(df_test$ds, 1)
    left_bound <- right_bound - as.difftime(days, unit="days")
  }
  
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = ds, y = y)) +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::lims(x=c(left_bound, 
                      right_bound))
  if (exists('cap', where = df) && plot_cap) {
    gg <- gg + ggplot2::geom_line(
      ggplot2::aes(y = cap), linetype = 'dashed', na.rm = TRUE)
  }
  if (x$logistic.floor && exists('floor', where = df) && plot_cap) {
    gg <- gg + ggplot2::geom_line(
      ggplot2::aes(y = floor), linetype = 'dashed', na.rm = TRUE)
  }
  if (uncertainty && x$uncertainty.samples && exists('yhat_lower', where = df)) {
    gg <- gg +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper),
                           alpha = 0.5,
                           fill = "#6AB693",
                           na.rm = TRUE)
  }
  if (!is.null(df_test)) {
    gg <- gg + geom_line(data=data_test, 
                         aes(x=ds, y=y), 
                         na.rm=TRUE, 
                         color="#0B289E")
  }
  gg <- gg +
    theme_minimal() +
    ggplot2::geom_point(na.rm=TRUE,
                        size=0.6) +
    ggplot2::geom_line(ggplot2::aes(y = yhat), 
                       color = "#04693A",
                       na.rm = TRUE) +
    ggplot2::theme(aspect.ratio = 3 / 5) 
  return(gg)
}
df_for_plotting <- function(m, fcst) {
  fcst$y <- NULL
  df <- m$history %>%
    dplyr::select(ds, y) %>%
    dplyr::full_join(fcst, by = "ds") %>%
    dplyr::arrange(ds)
  return(df)
}
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

data <- fread('stacking_data_2/data_Hamburg.csv')
data <- data[minute(data$to) == 0]

is_morning <- function(ds) {
  hour <- as.numeric(format(ds, '%H'))
  return(hour > 3 & hour < 12)
}

data <- data %>% 
  mutate(VV = na_locf(strtoi(VV))
         )

data[, `:=`(c=NULL, DD=NULL, V1=NULL, WW=NULL)]

data_train <- data %>% 
  select(ds = to,
         y = MW,
         t = T,
         is_light = is_light,
         school=school,
         school_flag=school_flag,
         workplace=workplace,
         workplace_flag=workplace_flag,
         stay_home=stay_home,
         stay_home_flag=stay_home_flag,
         borders=borders,
         Ff=Ff,
         P0=P0,
         P=P,
         U=U,
         Td=Td,
         VV=VV,
         clouds_left=clouds_left,
         clouds_right=clouds_right,
         height=height) %>% 
  slice(1:(n() - 3 * 24)) %>% 
  as.data.frame()

data_test <- data %>% 
  select(ds = to,
         y = MW,
         t = T,
         is_light = is_light,
         school=school,
         school_flag=school_flag,
         workplace=workplace,
         workplace_flag=workplace_flag,
         stay_home=stay_home,
         stay_home_flag=stay_home_flag,
         borders=borders,
         Ff=Ff,
         P0=P0,
         P=P,
         U=U,
         Td=Td,
         VV=VV,
         clouds_left=clouds_left,
         clouds_right=clouds_right,
         height=height) %>% 
  tail(3 * 24) %>% 
  as.data.frame()

future_df <- data %>% 
  select(ds = to,
         t = T,
         is_light = is_light,
         school=school,
         school_flag=school_flag,
         workplace=workplace,
         workplace_flag=workplace_flag,
         stay_home=stay_home,
         stay_home_flag=stay_home_flag,
         borders=borders,
         Ff=Ff,
         P0=P0,
         P=P,
         U=U,
         Td=Td,
         VV=VV,
         clouds_left=clouds_left,
         clouds_right=clouds_right,
         height=height
         ) %>% 
  as.data.frame() 

# Model

model <- prophet(growth = "linear",
                 changepoints = NULL,
                 n.changepoints = 20,
                 changepoint.range = 0.9, 
                 changepoint.prior.scale = 5,
                 holidays.prior.scale = 1.5,
                 seasonality.prior.scale = 5,
                 seasonality.mode = "additive",
                 uncertainty.samples = 0) 

model <- add_country_holidays(m = model, country_name="DE")

model <- add_regressor(model, 't', prior.scale=15, mode="multiplicative")
model <- add_regressor(model, 'school')
model <- add_regressor(model, 'school_flag')
model <- add_regressor(model, 'workplace')
model <- add_regressor(model, 'workplace_flag')
model <- add_regressor(model, 'stay_home')
model <- add_regressor(model, 'stay_home_flag')
model <- add_regressor(model, 'borders')
model <- add_regressor(model, 'Ff', prior.scale = 5)
model <- add_regressor(model, 'P0', prior.scale = 5)
model <- add_regressor(model, 'U', prior.scale = 5)
model <- add_regressor(model, 'Td', prior.scale = 5)
#model <- add_regressor(model, 'clouds_left', prior.scale = 0.5)
model <- add_regressor(model, 'clouds_right', prior.scale = 5)
#model <- add_regressor(model, 'height', prior.scale = 0.5)


#plot_forecast_component(model, forecast, name='clouds_right')

model <- add_seasonality(model, 
                         name = 'is_light',
                         period = 1,
                         fourier.order = 9,
                         condition.name = 'is_light')

model <- fit.prophet(model, data_train)


forecast <- predict(model, tail(future_df, 3 * 24))
forecast <- predict(model, future_df)
forecast_3_weeks <- tail(forecast, 3*24)
dates <- forecast_3_weeks$ds
y_pred <- forecast_3_weeks$yhat
y_valid <- data_test$y
metrics(y_valid, y_pred)



start <- 30200
end <- 30400
dates <- forecast[start:end, ]$ds
y_pred <- forecast[start:end, ]$yhat
y_valid <- data[start:end, ]$MW
ggof(y_pred, y_valid, dates=dates)
metrics(y_valid, y_pred)
dates[1]


ggof(y_pred, y_valid, dates=dates, xlab='ds', ylab='MW')

custom_plot(model, forecast, df_test=data_test, days=2500) 

prophet_plot_components(model, forecast)

#CV

model_CV <- cross_validation(model, 
                 initial = 5 * 365 * 24,
                 horizon =  90 * 24,
                 period = 90 * 24,
                 units= "hours"
)

model_CV <- cross_validation(model, 
                 initial = 2 * 365 * 24,
                 horizon =  365 * 24,
                 period = 365 * 24 / 2,
                 units= "hours"
                 )

model_metrics <- performance_metrics(model_CV, metrics=c("mse", "rmse", "mae"),
                    rolling_window = 0)
head(model_metrics)

plot_cross_validation_metric(model_CV, metric = "mse",
                             rolling_window = 0)
head(model_metrics)

future_df <- make_future_dataframe(model, periods = 3 * 24, 
                                   freq=60*60, include_history = TRUE)
forecast <- predict(model, future_df)

plot(model, forecast) + add_changepoints_to_plot(model)

ggplot(data_test) + geom_line(aes(ds, y), color='red') + 
  geom_line(aes(ds, t), color='green')



