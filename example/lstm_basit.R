# https://www.datatechnotes.com/2019/01/regression-example-with-lstm-networks.html

library(keras)

N = 400
set.seed(123)
n = seq(1:N)
a = n/10+4*sin(n/10)+sample(-1:6,N,replace=T)+rnorm(N)

step = 2
a = c(a, replicate(step, tail(a, 1)))
x = NULL
y = NULL
for(i in 1:N)
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}


X = array(x, dim=c(N, step,1))
model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>%  
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()

model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(X)

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)

x_axes = seq(1:length(y_pred))
plot(x_axes, y, type="l", col="red", lwd=2)
lines(x_axes, y_pred, col="blue",lwd=2)

legend("topleft", legend=c("y-original", "y-predicted"),
       col=c("red", "blue"), lty=1,cex=0.8)

cor(y,y_pred)
