dataset <- readRDS("dataset.rds")
library(forecast)
y= dataset$pib_rs
x = y[1:64]
h = 4
pred1 = rep(0,(28-h+1))
pred2=pred1
previ= rep(0,8)

m1 = arima(x,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=4))
m2 = auto.arima(x, stationary = TRUE, seasonal = TRUE)
r1 = residuals(m1)

for(i in 1:(28-h+1)){
  previ=predict(m1,n.ahead = h)
  pred1[i]=previ$pred[h]
  m1 = arima(y[1:(64+i)],order=c(1,0,0),seasonal=list(order=c(1,0,0),period=4))
}
for(i in 1:(28-h+1)){
  previ=predict(m2,n.ahead = h)
  pred2[i]=previ$pred[h]
  m2 = auto.arima(y[1:(64+i)], stationary = TRUE, seasonal = TRUE)
}

z = ts(y[(64+h):92])
ts.plot(z,ts(pred1),ts(pred2),col=c("black","blue","red"))
erro1=sum((z-pred1)^2)
erro2=sum((z-pred2)^2)

