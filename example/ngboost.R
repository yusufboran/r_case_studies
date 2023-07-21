

library(ngboostForecast)
#> Loading required package: reticulate
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

train = window(seatbelts, end = c(1983,12))

test = window(seatbelts, start = c(1984,1))

# without external variables with Ridge regression

model <- NGBforecast$new(Dist = Dist("LogNormal"),
                         Base = sklearner(module = "linear_model",
                                          class = "Ridge"),
                         Score = Scores("LogScore"),
                         natural_gradient = TRUE,
                         n_estimators = 200,
                         learning_rate =  0.1,
                         minibatch_frac = 1,
                         col_sample = 1,
                         verbose = TRUE,
                         verbose_eval = 5,
                         tol = 1e-5)

model$fit(y = train[,2], 
          seasonal = TRUE, 
          max_lag = 12, 
          early_stopping_rounds = 10L)

fc <- model$forecast(h = 12, level = c(99,95,90, 80, 70, 60), 
                     data_frame = FALSE)

autoplot(fc) + autolayer(test[,2])
