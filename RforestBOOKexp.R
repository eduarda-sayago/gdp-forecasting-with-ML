# ================================================
# -------------RANDOM FOREST MODEL----------------
# ================================================

library(randomForest)
library(MASS)
set.seed(101)

dim(Boston)
train = sample(1:nrow(Boston),300)
?Boston

rf.boston = randomForest(medv~., data = Boston, subset= train)
rf.boston

# the MSR and % variance explained are based on OOB (out of bag) estimates, 
# a device in RF to get honest error estimates.The model reports mtry=4, num of vars
# randomly chosen at each split. since p=13, we could try all possib vals of mtry.
# well do so, record the results and make a plot.

# tunning par = mtry.
oob.err = double(13)
test.err=double(13)
for(mtry in 1:13){
  fit= randomForest(medv~., data = Boston, subset= train, mtry=mtry, importance = TRUE, ntree=400)
  oob.err[mtry]= fit$mse[400]
  pred = predict(fit, Boston[-train,])
  test.err[mtry]=with(Boston[-train,], mean((medv-pred)^2))
  cat(mtry," ")
}
summary(pred)
importance(pred, type=1)
varImpPlot(rf.boston)

summary(fit)
importance(fit)
varImpPlot(fit)

matplot(1:mtry, 
        cbind(test.err, oob.err), 
        pch=19, 
        col=c("red","blue"), 
        type="b", 
        ylab = "Mean Squared Error")
legend("topright", legend=c("OOB", "Test"), pch=19, col=c("red", "blue"))
