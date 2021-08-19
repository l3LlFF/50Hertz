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
data[, `:=`(V1 = NULL, P =NULL, DD =NULL, WW = NULL, c = NULL)]
#data[, `:=`("school" = NULL, "workplace" =NULL, "stay_home" =NULL, 
#            "borders" = NULL, "school_flag" = NULL, workplace_flag = NULL,
#            "stay_home_flag" = NULL, VV = NULL)]

data <- data[minute(data$to) == 0]

data <- data %>% mutate(
  year = lubridate::year(to),
  day = lubridate::day(to),
  month = lubridate::month(to),
  hour = lubridate::hour(to),
  minute = lubridate::minute(to),
  weekday = as.POSIXlt(to)$wday
)

dates <- data[57059:57130, ]$to
data[, to:=NULL]
train_data <- as.h2o(data[1:57058, ])
test_data <- as.h2o(data[57059:57130, ])


# Set the predictors and response; set the factors:
predictors <- c(
  "school", "school_flag", "workplace", "workplace_flag",
  "stay_home", "stay_home_flag", "borders", "T", 
  "P0", "U", "Ff", "Td", "is_light",
  "year", "day", "month", "hour", "minute",
  "height", "clouds_right", "weekday")
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
        validation_frame = test_data,
        y = response,
        x = predictors,
        seed = 1915,
        nfolds = 8,
        keep_cross_validation_predictions = TRUE,
        ntrees = 1500,
        learn_rate = 0.01,
        max_depth = 8,
        sample_rate = 0.8,
        col_sample_rate = 0.8,
        min_rows = 5)

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
