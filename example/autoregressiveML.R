#https://github.com/Akai01/caretForecast

library(caretForecast)
data(retail_wide, package = "caretForecast")
i <- 8
dtlist <- split_ts(retail_wide[,i], test_size = 12)

training_data <- dtlist$train

testing_data <- dtlist$test

fit <- ARml(training_data, max_lag = 12, caret_method = "glmboost", 
            verbose = FALSE)
#> initial_window = NULL. Setting initial_window = 301
#> Loading required package: ggplot2
#> Loading required package: lattice

forecast(fit, h = length(testing_data), level = c(80,95))-> fc

accuracy(fc, testing_data)
#>                        ME     RMSE      MAE       MPE     MAPE     MASE
#> Training set 1.899361e-14 16.55233 11.98019 -1.132477 6.137096 0.777379
#> Test set     7.472114e+00 21.68302 18.33423  2.780723 5.876433 1.189685
#>                   ACF1 Theil's U
#> Training set 0.6181425        NA
#> Test set     0.3849258 0.8078558


autoplot(fc) + 
  autolayer(testing_data, series = "testing_data")

??ARml
