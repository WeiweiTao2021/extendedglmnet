## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("extendedglmnet")

## -----------------------------------------------------------------------------
library(extendedglmnet)

## -----------------------------------------------------------------------------
size = 50
x = matrix(runif(55*size),ncol=55)
y = 10*x[,1] + 3*x[,2] + 20*(x[,3]-0.5)**2 + 10*x[,4] + 5*x[,35] + runif(1,0,1)

## -----------------------------------------------------------------------------
index = sample(1:size, 0.7*size)
trainx = x[index,]
testx = x[-index,]
trainy = y[index]
testy = y[-index]

## -----------------------------------------------------------------------------
fit.lr<- extendedglmnet(trainx, trainy, family = "gaussian", type = "regression")

## -----------------------------------------------------------------------------
print.extendglmnet(fit.lr)

## -----------------------------------------------------------------------------
ypred <- predict.extendglmnet(fit.lr, newx = testx, type = "response")

## -----------------------------------------------------------------------------
evaluation(testy, ypred, "MSE")
evaluation(testy, ypred, "RMSE")
evaluation(testy, ypred, "MAE")

## -----------------------------------------------------------------------------
fit.ridge<- extendedglmnet(trainx, trainy, family = "gaussian", type = "ridge")
print.extendglmnet(fit.ridge)

## -----------------------------------------------------------------------------
ypred <- predict.extendglmnet(fit.ridge, newx = testx, type = "response")

## -----------------------------------------------------------------------------
evaluation(testy, ypred, "MSE")
evaluation(testy, ypred, "RMSE")
evaluation(testy, ypred, "MAE")

## -----------------------------------------------------------------------------
fit.lasso<- extendedglmnet(trainx, trainy, family = "gaussian", type = "lasso")

ypred <- predict.extendglmnet(fit.lasso, newx = testx, type = "response")
evaluation(testy, ypred, "MSE")
evaluation(testy, ypred, "RMSE")
evaluation(testy, ypred, "MAE")

## -----------------------------------------------------------------------------
grid=10^seq(6,-2,length=10)
fit.lasso<- extendedglmnet(trainx, trainy, family = "gaussian", type = "lasso", lambda = grid)

ypred <- predict.extendglmnet(fit.lasso, newx = testx, type = "response")
evaluation(testy, ypred, "MSE")

## -----------------------------------------------------------------------------
fit.rl<- extendedglmnet(trainx, trainy, family = "gaussian", type = "random lasso", q1 = 30, q2 = 30)
print.extendglmnet(fit.rl)

## -----------------------------------------------------------------------------
ypred <- predict.extendglmnet(fit.rl, newx = testx, type = "response")
evaluation(testy, ypred, "MSE")
evaluation(testy, ypred, "RMSE")
evaluation(testy, ypred, "MAE")

## -----------------------------------------------------------------------------
size = 200
x = matrix(rnorm(210*size),ncol=210)
z = 1 + 2*x[,1] + 3*x[,40] - 4*x[,50] + 3*x[,210]
pr = 1/(1+exp(-z))
y = rbinom(size,1,pr)

index = sample(1:size, 0.7*size)
trainx = x[index,]
testx = x[-index,]
trainy = y[index]
testy = y[-index]

## -----------------------------------------------------------------------------
fit.logistic<- extendedglmnet(trainx, trainy, family = "binomial", type = "regression")
print.extendglmnet(fit.logistic)
ypred <- predict.extendglmnet(fit.logistic, newx = testx, family = "binomial", type = "class")
evaluation(testy, ypred, "accuracy")

## -----------------------------------------------------------------------------
ypred <- predict.extendglmnet(fit.logistic, newx = testx, family = "binomial", type = "link")
t(ypred)

## -----------------------------------------------------------------------------
ypred <- predict.extendglmnet(fit.logistic, newx = testx, family = "binomial", type = "response")
t(ypred)

## -----------------------------------------------------------------------------
fit.ridge<- extendedglmnet(trainx, trainy, family = "binomial", type = "ridge")
ypred <- predict.extendglmnet(fit.ridge, newx = testx, family = "binomial", type = "class")
evaluation(testy, ypred, "accuracy")

## -----------------------------------------------------------------------------
fit.lasso<- extendedglmnet(trainx, trainy, family = "binomial", type = "lasso")
ypred <- predict.extendglmnet(fit.lasso, newx = testx, family = "binomial", type = "class")
evaluation(testy, ypred, "accuracy")

## -----------------------------------------------------------------------------
fit.rl<- extendedglmnet(trainx, trainy, family = "binomial", type = "random lasso")
ypred <- predict.extendglmnet(fit.rl, newx = testx, family = "binomial", type = "class")
evaluation(testy, ypred, "accuracy")

