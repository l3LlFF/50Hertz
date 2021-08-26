library(h2o)
library(tibble)
library(readr)
library(data.table)
library(tidyr)
library(tidyverse)
library(imputeTS)
library(lubridate, warn.conflicts = FALSE)
library(hydroGOF)
library(MLmetrics)
library(mltools)


# H2O gbm forecasting

h2o.init()



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
data[, `:=`(V1 = NULL, P =NULL, DD =NULL, WW = NULL, c = NULL, 
            clouds_left = NULL, clouds_right = NULL)]

data <- data[minute(data$to) == 0]

data <- data %>% mutate(
  #day = factor(lubridate::day(to)),
  month = factor(lubridate::month(to)),
  hour = factor(lubridate::hour(to)),
  weekday = factor(as.character(as.POSIXlt(to)$wday)),
  yday = yday(to)
)

#n_shift <- 24 * 7
#data <- setDT(data)[, paste0('T', 1:n_shift) := shift(data$T, 1:n_shift)][]
#data <- data[n_shift:nrow(data)]

data <- data %>% mutate(
  T_1_day = lag(T, 24),
  T_1_week = lag(T, 24 * 1 * 7),
  T_2_weeks = lag(T, 24 * 2 * 7),
  T_3_week = lag(T, 24 * 3 * 7),
  T_4_weeks = lag(T, 24 * 4 * 7),
  T_1_hours = lag(T, 1),
  T_2_hours = lag(T, 2),
  T_3_hours = lag(T, 3),
  T_4_hours = lag(T, 4),
  T_5_hours = lag(T, 5),
  T_6_hours = lag(T, 6),
  T_7_hours = lag(T, 7),
  T_8_hours = lag(T, 8),
  
  Ff_1_day = lag(Ff, 24),
  Ff_1_hour = lag(Ff, 1),
  Ff_2_hours = lag(Ff, 2),
  Ff_3_hours = lag(Ff, 3),
  Ff_4_hours = lag(Ff, 4),
  Ff_5_hours = lag(Ff, 5),
  
  U_1_day = lag(U, 24),
  U_1_hour = lag(U, 1),
  U_2_hours = lag(U, 2),
  U_3_hours = lag(U, 3),
  U_4_hours = lag(U, 4),
  U_5_hours = lag(U, 5),
  
  P0_1_day = lag(P0, 24),
  P0_1_hour = lag(P0, 1),
  P0_2_hours = lag(P0, 2),
  P0_3_hours = lag(P0, 3),
  P0_4_hours = lag(P0, 4),
  P0_5_hours = lag(P0, 5)
)
n_shift = 24 * 4 * 7
data <- data[n_shift:nrow(data)]

dates <- data[(nrow(data) - 24*3 + 1):nrow(data), ]$to

data[, to:=NULL]
nrow(data)
train_border <- nrow(data) - 24 * 30
train_data <- as.h2o(data[1:(train_border), ])
val_data <- as.h2o(data[(train_border + 1):(nrow(data) - 24*3), ])
test_data <- as.h2o(data[(nrow(data) - 24*3 + 1):nrow(data), ])

# Set the predictors and response; set the factors:
predictors <- c(names(data[, -12]))


response <- "MW"


# Grid search

gbm_params <- list(
                    ntrees = seq(1000, 2000, 100),
                    learn_rate = seq(0.001, 0.05, 0.001),
                    max_depth = seq(4, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria <- list(strategy = "RandomDiscrete", max_models = 36, seed = 1)

# Build and train the model:
pros_gbm <- h2o.grid("gbm",
    grid_id = "gbm_grid",
    training_frame = train_data,
    validation_frame = test_data,
    y = response,
    x = predictors,
    #ntrees = 900,
    seed = 1915,
    nfolds = 5,
    keep_cross_validation_predictions = TRUE,
    hyper_params = gbm_params,
    search_criteria = search_criteria,
    stopping_metric = 'MSE'
                    )

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                             decreasing = TRUE)


best_model <- h2o.getModel(gbm_gridperf@model_ids[[1]])
best_model@model[["model_summary"]]



best <- h2o.performance(model = best_model,
                        newdata = test_data)
# Eval performance:
perf <- h2o.performance(pros_gbm)

# Generate predictions on a validation set (if necessary):
pred <- h2o.predict(best_model, newdata = as.h2o(test_data))

df <- data.frame(
  dt = dates,
  y = as.data.table(test_data$MW),
  y_pred = as.data.table(pred$predict)
  )

ggplot(data=df, aes(x=dt)) + 
  geom_line(aes(y=MW), color = "darkred") +
  geom_line(aes(y=predict), color="steelblue", linetype="twodash") +
  theme_minimal()

metrics(df$MW, df$predict)



# Build and train the model:
pros_gbm <- h2o.gbm(    
        training_frame = train_data,
        validation_frame = val_data,
        y = response,
        x = predictors,
        seed = 1915,
        nfolds = 7,
        keep_cross_validation_predictions = TRUE,
        ntrees = 1800,
        learn_rate = 0.01,
        max_depth = 7,
        sample_rate = 0.9,
        col_sample_rate = 0.9,
        min_rows = 15,
        categorical_encoding = 'OneHotExplicit')

# Eval performance:
perf <- h2o.performance(pros_gbm)

# Generate predictions on a validation set (if necessary):
pred <- h2o.predict(pros_gbm, newdata = as.h2o(test_data))

df <- data.frame(
  dt = dates,
  y = as.data.table(test_data$MW),
  y_pred = as.data.table(pred$predict)
)

ggplot(data=df, aes(x=dt)) + 
  geom_line(aes(y=MW), color = "darkred") +
  geom_line(aes(y=predict), color="steelblue", linetype="twodash") +
  theme_minimal() 

metrics(df$MW, df$predict)

h2o.varimp_plot(pros_gbm, 40)
tail(h2o.varimp(pros_gbm), 10)


