y= dataset$pib_rs
x = y[1:64]
pred1 = rep(0,28)
pred2=pred
previ= rep(0,8)
# ts.plot(x)
# acf(x,lag=12)
# pacf(x,lag=12)
m1 = arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=4))
m2 = auto.arima(x, stationary = TRUE, seasonal = TRUE)
r1 = residuals(m1)
# acf(r1)
# pacf(r1)
for(i in 1:24){
  previ=predict(m1,n.ahead = 4)
  pred1[i]=previ$pred[4]
  m1 = arima(y[1:(64+i)],order=c(1,0,0),seasonal=list(order=c(1,0,0),period=4))
}
for(i in 1:24){
  previ=predict(m2,n.ahead = 4)
  pred2[i]=previ$pred[4]
  m2 = auto.arima(y[1:(64+i)], stationary = TRUE, seasonal = TRUE)
}

z = ts(y[65:92])
ts.plot(z,ts(pred1),col=c("black","blue"))
erro1=sum((z-pred1)^2)
erro2=sum((z-pred2)^2)

## 4 steps ahead
library(forecast)
y = dataset$pib_rs
t_seq = 65:92
pred = numeric(length(t_seq))

for(i in seq_along(t_seq)){
  t = t_seq[i]
  origin = t - 4
  fit = Arima(y[1:origin], order = c(1,0,0), seasonal = c(1,0,0))  # forecast::Arima
  f = forecast(fit, h = 4)
  pred[i] = as.numeric(f$mean[4])
}
ts.plot(z,ts(pred),col=c("black","blue"))
erro1=sum((z-pred)^2)

pred2 = numeric(length(t_seq))

for(i in seq_along(t_seq)){
  t = t_seq[i]
  origin = t - 4
  fit = auto.arima(y[1:origin], stationary=TRUE, seasonal=TRUE)
  f = forecast(fit, h = 4)
  pred2[i] = as.numeric(f$mean[4])
}
ts.plot(z,ts(pred2),col=c("black","blue"))
erro2=sum((z-pred2)^2)
