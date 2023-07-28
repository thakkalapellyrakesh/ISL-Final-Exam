#Chapter 8 Conceptual Problem 5:

prob <- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)
#Majority Vote:
sum(prob >= 0.5) # number of 'Red' predictions

sum(prob < 0.5) # number of 'Green' predictions

ifelse(sum(prob >= 0.5) > sum(prob < 0.5), "Red", "Green")

#Average Approach:
mean(prob) # average P(Red)

ifelse(mean(prob) >= 0.5, "Red", "Green")

#With the majority vote approach, we classify X as Red as it is the most commonly occurring class among the 10 predictions (6 for Red vs 4 for Green). 
#With the average probability approach, we classify X as Green as the average of the 10 probabilities is 0.45.

#Chapter 8 Applied Problem 8:
#a)
install.packages("ISLR")
library(ISLR)
attach(Carseats)

train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train <- Carseats[train, ]
Carseats.test <- Carseats[-train, ]



#b)
install.packages("tree")
library(tree)
tree.carseats <- tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)


plot(tree.carseats)
text(tree.carseats, pretty = 0)

yhat <- predict(tree.carseats, newdata = Carseats.test)
mean((yhat - Carseats.test$Sales)^2)



#c)
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)

prune.carseats <- prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

yhat <- predict(prune.carseats, newdata = Carseats.test)
mean((yhat - Carseats.test$Sales)^2)


#d)
install.packages("randomForest")
library(randomForest)


bag.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
yhat.bag <- predict(bag.carseats, newdata = Carseats.test)
mean((yhat.bag - Carseats.test$Sales)^2)


importance(bag.carseats)


#e)
rf.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
yhat.rf <- predict(rf.carseats, newdata = Carseats.test)
mean((yhat.rf - Carseats.test$Sales)^2)

importance(rf.carseats)


