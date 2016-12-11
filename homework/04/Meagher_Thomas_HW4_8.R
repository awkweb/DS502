# Thomas Meagher HW4 Q8
# Section 8.4, Page 333-334, question 8

# a
library(ISLR)
attach(Carseats)
set.seed(1)

dim_carseats = dim(Carseats)[1]
train = sample(dim_carseats, dim_carseats / 2)
Carseats_train = Carseats[train, ]
Carseats_test = Carseats[-train, ]

# b
library(tree)
tree_carseats = tree(Sales~., data=Carseats_train)
summary(tree_carseats)

plot(tree_carseats)

text(tree_carseats, pretty=0)
pred_carseats = predict(tree_carseats, Carseats_test)
mean((Carseats_test$Sales - pred_carseats)^2)

# c
cv_carseats = cv.tree(tree_carseats, FUN=prune.tree)

par(mfrow=c(1, 2))
plot(cv_carseats$size, cv_carseats$dev, type="b")
plot(cv_carseats$k, cv_carseats$dev, type="b")

pruned_carseats = prune.tree(tree_carseats, best=9)
pred_pruned = predict(pruned_carseats, Carseats_test)
mean((Carseats_test$Sales - pred_pruned)^2)

# d
library(randomForest)
bag_carseats = randomForest(Sales~., data=Carseats_train, mtry=10, ntree=500, importance=T)
bag_pred = predict(bag_carseats, Carseats_test)
mean((Carseats_test$Sales - bag_pred)^2)
importance(bag_carseats)

# e
rf_carseats = randomForest(Sales~., data=Carseats_train, mtry=10, ntree=500, importance=T)
rf_pred = predict(rf_carseats, Carseats_test)
mean((Carseats_test$Sales - rf_pred)^2)
importance(rf_carseats)
