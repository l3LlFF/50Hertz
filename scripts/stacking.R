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
library(dplyr)

# Traing models on different weather data

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

files <- c(
  "Laage",
  "Holzdorf",
  "Hamburg",
  "Erfurt",
  "Drezden",
  "Berlin",
  "Braunschweig"
)


# Population 

berlin_pop <- 3500000
holzdorf_pop <- 2500000
hamburg_pop <- 1800000
laage_pop <- 1600000
braunschweig_pop <- 2200000
drezden_pop <- 4000000
erfurt_pop <- 2100000


for (x in  files) {
  file <- paste('stacking_data_2/data_',x,'.csv', sep='')
  data <- fread(file)
  data <- data[minute(data$to) == 0]
  
  data <- data %>% 
    mutate(VV = na_locf(strtoi(VV))
    )
  
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
    select(ds = to, y = MW) %>% 
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
  
  model <- prophet(growth = "linear",
                   changepoints = NULL,
                   n.changepoints = 50,
                   changepoint.range = 0.7, 
                   changepoint.prior.scale = 5,
                   holidays.prior.scale = 1.5,
                   seasonality.prior.scale = 5,
                   seasonality.mode = "additive",
                   uncertainty.samples = 0)
  
  
  model <- add_country_holidays(m = model, country_name="DE")
  
  model <- add_regressor(model, 't', prior.scale=5, mode="multiplicative")
  model <- add_regressor(model, 'school')
  model <- add_regressor(model, 'school_flag')
  model <- add_regressor(model, 'workplace')
  model <- add_regressor(model, 'workplace_flag')
  model <- add_regressor(model, 'stay_home')
  model <- add_regressor(model, 'stay_home_flag')
  model <- add_regressor(model, 'borders')
  model <- add_regressor(model, 'Ff', prior.scale = 2)
  model <- add_regressor(model, 'P0', prior.scale = 5)
  model <- add_regressor(model, 'U', prior.scale = 0.5)
  model <- add_regressor(model, 'Td', prior.scale = 5)
  model <- add_regressor(model, 'clouds_right', prior.scale = 5)
  model <- add_regressor(model, 'height', prior.scale = 0.5)
  model <- add_seasonality(model, 
                           name = 'is_light',
                           period = 1,
                           fourier.order = 12,
                           condition.name = 'is_light')
  print('model started training')
  model <- fit.prophet(model, data_train)
  saveRDS(model, file=paste('models/',x,'.RDS', sep=''))
  print('model saved')
}

total_pop <- berlin_pop + holzdorf_pop + hamburg_pop + 
  laage_pop + braunschweig_pop + drezden_pop + erfurt_pop



preds <- list()
forecast_all <- numeric(72)
forecast_all_weighed <- numeric(72)

for (x in  files) {
  file <- paste('stacking_data_2/data_',x,'.csv', sep='')
  data <- fread(file)
  data <- data[minute(data$to) == 0]
  
  data <- data %>% 
    mutate(VV = na_locf(strtoi(VV))
    )
  
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
    select(ds = to, y = MW) %>% 
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
  model <- readRDS(file = paste('models/',x,'.RDS', sep=''))
  forecast <- predict(model, tail(future_df, 3 * 24))
  forecast_3_weeks <- tail(forecast, 3*24)
  dates <- forecast_3_weeks$ds
  y_pred <- forecast_3_weeks$yhat
  y_valid <- data_test$y
  print(x)
  print(metrics(y_valid, y_pred))
  ggof(y_pred, y_valid, dates=dates, 
       main=x, xlab='ds', ylab='MW')
  forecast_all <- forecast_all + y_pred 
  preds <- append(preds, list(y_pred))
}
forecast_all <- forecast_all / 7
metrics(y_valid, forecast_all)
ggof(forecast_all, y_valid, dates=dates, 
     main="average", xlab='ds', ylab='MW')
  

weighted_forecast <- berlin_pop * preds[[1]] / total_pop + 
  holzdorf_pop * preds[[2]] / total_pop + 
  hamburg_pop * preds[[3]] / total_pop + 
  laage_pop * preds[[4]] / total_pop + 
  braunschweig_pop * preds[[5]] / total_pop + 
  drezden_pop * preds[[6]] / total_pop + 
  erfurt_pop * preds[[7]] / total_pop

metrics(y_valid, weighted_forecast)
ggof(weighted_forecast, y_valid, dates=dates, 
     main="weighted", xlab='ds', ylab='MW')


metrics(head(y_valid, 69), tail(forecast_all, 69))
ggof(tail(forecast_all, 69), head(y_valid, 69), dates=head(dates, 69), 
     main="average", xlab='ds', ylab='MW')

model <- readRDS(file = paste('models/','Hamburg','.RDS', sep=''))
forecast <- predict(model, future_df)
custom_plot(model, forecast, df_test=data_test, days=500) 



head(data, 1)

data %>% 
  select(MW
    
  )
data %>% 
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





