#Chapter 8 Applied Problem 10:
#a)
install.packages("ISLR")
library(ISLR)
attach(Hitters)
Hitters<-na.omit(Hitters)
Hitters$Salary<-log(Hitters$Salary)
summary(Hitters)


#b)
subset<-1:200
hitters.train <- Hitters[subset, ]
hitters.test <- Hitters[-subset, ]


#c)
install.packages("gbm")
library(gbm)
set.seed(1)

powerss<-seq(-2,0,by=0.1)
lambdas<-10^powerss

train.error<-rep(NA,length(lambdas))
for (i in 1:length(lambdas)){
  hitters.gbm<-gbm(Salary~.,hitters.train,distribution = "gaussian",n.trees=1000,shrinkage=lambdas[i])
  hitters.train.pred<-predict(hitters.gbm,hitters.train,n.trees=1000)
  train.error[i]<-mean((hitters.train.pred-hitters.train$Salary)^2)
}

#Plotting training MSE against Lambdas
plot(lambdas,train.error,type="b",xlab="Shrinkage Value(lambda)",ylab="Training MSE")

#d)
#test mse
set.seed(1)


test.error<-rep(NA,length(lambdas))
for (i in 1:length(lambdas)){
  hitters.gbm<-gbm(Salary~.,hitters.train,distribution = "gaussian",n.trees=1000,shrinkage=lambdas[i])
  hitters.test.pred<-predict(hitters.gbm,hitters.test,n.trees=1000)
  test.error[i]<-mean((hitters.test.pred-hitters.test$Salary)^2)
}

#Plotting testing MSE against Lambdas
plot(lambdas,test.error,type="b",xlab="Shrinkage Value(lambda)",ylab="Test MSE")

hitters.gbm.testerror<-min(test.error)
hitters.gbm.testerror



#e)
install.packages("glmnet")
library(glmnet)

#Fitting least square regression model
lm<-lm(Salary~.,hitters.train)
hitters.predict.lm<-predict(lm,hitters.test)
hitters.lm.test.mse<-mean((hitters.predict.lm-hitters.test$Salary)^2)
hitters.lm.test.mse


#Ridge regression model

#here we have selected a s=0.01  value of lambda to fit the model

x<-model.matrix(Salary~.,hitters.train)
x.test<-model.matrix(Salary ~ . , hitters.test)
y<-hitters.train$Salary
hitters.ridge<-glmnet(x,y,alpha=0)
hitters.ridge.predict<-predict(hitters.ridge,s=0.01,x.test)
hitters.ridge.test.mse<-mean((hitters.ridge.predict-hitters.test$Salary)^2)
hitters.ridge.test.mse


#Lasso regression model

#here we have selected a s=0.01  value of lambda to fit the model

x<-model.matrix(Salary~.,hitters.train)
x.test<-model.matrix(Salary ~ . , hitters.test)
y<-hitters.train$Salary
hitters.lasso<-glmnet(x,y,alpha=1)
hitters.lasso.predict<-predict(hitters.lasso,s=0.01,x.test)
hitters.lasso.test.mse<-mean((hitters.lasso.predict-hitters.test$Salary)^2)
hitters.lasso.test.mse


#f)
boost.hitters<-gbm(Salary~.,data=hitters.train,distribution = "gaussian",n.trees = 1000,shrinkage=lambdas[which.min(test.error)])

summary(boost.hitters)


#g)
set.seed(1)
hitters.bagging<-randomForest(Salary~.,hitters.train,mtry=19,importance=TRUE)
hitters.bagg.predict<-predict(hitters.bagging,hitters.test)
hitters.bagg.test.mse<-mean((hitters.bagg.predict-hitters.test$Salary)^2)
hitters.bagg.test.mse
