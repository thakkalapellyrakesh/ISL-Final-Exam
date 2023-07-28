# Chapter 9 Applied Problem 5:
install.packages("ISLR")
library(ISLR)

#a)
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

#b)
plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

#c)
logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)




#d)
data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))




#e)
install.packages("glm")
logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitnl.fit)



#f)
probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

#g)
install.packages("e1071")
library(e1071)
data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))


#h)
data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

#i) Comments
#We may draw the conclusion that discovering non-linear decision boundaries can be done extremely well using both logistic regression and SVM with non-linear kernels. 
#When it comes to identifying non-linear decision boundaries, SVM with a linear kernel and logistic regression without any interaction terms do very poorly. 
#An argument in favor of SVM, however, is that, in contrast to using SVM, we simply need to tune gamma. 
#This is because when using logistic regression, we must manually tune to identify the appropriate interaction terms.