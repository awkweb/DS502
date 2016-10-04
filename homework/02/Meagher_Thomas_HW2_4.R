# Thomas Meagher HW2 Q4
# Section 4.7, page171, question 10

library(ISLR)
data(Weekly)

# a
attach(Weekly)
dim(Weekly)
summary(Weekly)
weekly_quantitative <- subset(Weekly, select = -c(Direction))
pairs(weekly_quantitative)
plot(Year, Volume)
cor(weekly_quantitative)

# b
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)

# c
glm.probs = predict(glm.fit, Weekly, type="response")
glm.pred = rep("Down", nrow(Weekly))
glm.pred[glm.probs > 0.50] = "Up"
table(glm.pred, Direction)
mean(glm.pred == Direction)

# d
train = Year <= 2008
Weekly2 = Weekly[!train,]
glm.fit = glm(Direction~Lag2, family=binomial, data=Weekly, subset=train)
summary(glm.fit)
glm.probs = predict(glm.fit, Weekly2, type="response")
glm.pred = rep("Down", nrow(Weekly2))
glm.pred[glm.probs > 0.50] = "Up"
table(glm.pred, Weekly2$Direction)
mean(glm.pred == Weekly2$Direction)

# e
library(MASS)
lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
lda.fit
lda.predict = predict(lda.fit, Weekly2)
lda.class = lda.predict$class
table(lda.class, Weekly2$Direction)
mean(lda.class == Weekly2$Direction)

# f
qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.fit
qda.predict = predict(qda.fit, Weekly2)
qda.class = qda.predict$class
table(qda.class, Weekly2$Direction)
mean(qda.class == Weekly2$Direction)

# g
library(class)
train.X = Weekly[train, "Lag2", drop=F]
test.X = Weekly[!train, "Lag2", drop=F]
train.Direction = Weekly[train, "Direction", drop=T]
test.Direction = Weekly[!train, "Direction", drop=T]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, test.Direction)
mean(knn.pred == test.Direction)

# i
# Logistic regression with predictors: Lag2, Volume, and Lag2*Volume
glm.fit = glm(Direction~Lag2*Volume, family=binomial, data=Weekly, subset=train)
summary(glm.fit)
glm.probs = predict(glm.fit, Weekly2, type="response")
glm.pred = rep("Down", nrow(Weekly2))
glm.pred[glm.probs > 0.50] = "Up"
table(glm.pred, Weekly2$Direction)
mean(glm.pred == Weekly2$Direction)

# KNN with predictor: Lag2, and k=4
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=4)
table(knn.pred, test.Direction)
mean(knn.pred == test.Direction)

# KNN with predictor: Lag2, Lag2^2, and k=4
train.X = cbind(Lag2, I(Lag2 ^ 2))[train,]
test.X = cbind(Lag2, I(Lag2 ^ 2))[!train,]
train.Direction = Direction [train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=4)
table(knn.pred, test.Direction)
mean(knn.pred == test.Direction)



















