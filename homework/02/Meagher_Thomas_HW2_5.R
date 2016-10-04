# Thomas Meagher HW2 Q5
# Section 4.7, page 171-172, question 11

# a
library(ISLR)
data(Auto)
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
attach(Auto)

# b
pairs(Auto)
par(mfrow=c(2, 2))
plot(displacement, mpg01)
plot(horsepower, mpg01)
plot(weight, mpg01)
plot(acceleration, mpg01)

# c
set.seed(1)
rands <- rnorm(nrow(Auto))
test <- rands > quantile(rands, 0.75)
train <- !test
Auto.train <- Auto[train,]
Auto.test <- Auto[test,]

# d
library(MASS)
lda.fit = lda(mpg01~displacement+horsepower+weight+acceleration, data=Auto.train)
lda.fit
lda.predict = predict(lda.fit, Auto.test)
lda.class = lda.predict$class
table(lda.class, Auto.test$mpg01)
test_accuracy <- mean(lda.class == Auto.test$mpg01)
1 - test_accuracy

# e
qda.fit = qda(mpg01~displacement+horsepower+weight+acceleration, data=Auto.train)
qda.fit
qda.predict = predict(qda.fit, Auto.test)
qda.class = qda.predict$class
table(qda.class, Auto.test$mpg01)
test_accuracy <- mean(qda.class == Auto.test$mpg01)
1 - test_accuracy

# f
glm.fit = glm(mpg01~displacement+horsepower+weight+acceleration, family=binomial, data=Auto.train)
summary(glm.fit)
glm.probs = predict(glm.fit, Auto.test, type="response")
glm.pred = rep(0, nrow(Auto.test))
glm.pred[glm.probs > 0.50] = 1
table(glm.pred, Auto.test$mpg01)
test_accuracy <- mean(glm.pred == Auto.test$mpg01)
1 - test_accuracy

# g
library(class)
set.seed(1)
train.knn = Auto.train[,c("displacement","horsepower","weight","acceleration")]
test.knn =  Auto.test[,c("displacement","horsepower","weight","acceleration")]
knn.pred=knn(train.knn, test.knn, Auto.train$mpg01, k=1)
table(knn.pred, Auto.test$mpg01)
test_accuracy <- mean(knn.pred==Auto.test$mpg01)
1 - test_accuracy

knn.pred=knn(train.knn, test.knn, Auto.train$mpg01, k=2)
table(knn.pred, Auto.test$mpg01)
test_accuracy <- mean(knn.pred==Auto.test$mpg01)
1 - test_accuracy

knn.pred=knn(train.knn, test.knn, Auto.train$mpg01, k=3)
table(knn.pred, Auto.test$mpg01)
test_accuracy <- mean(knn.pred==Auto.test$mpg01)
1 - test_accuracy

knn.pred=knn(train.knn, test.knn, Auto.train$mpg01, k=4)
table(knn.pred, Auto.test$mpg01)
test_accuracy <- mean(knn.pred==Auto.test$mpg01)
1 - test_accuracy
