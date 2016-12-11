# Thomas Meagher HW4 Q2
# Section 6.8, page 264, question 11 

# a
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)

# Best Subset Selection
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k, length = nrow(Boston)))
cv_errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv_errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

# Lasso
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv_lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv_lasso)

rmse = sqrt(cv_lasso$cvm[cv_lasso$lambda == cv_lasso$lambda.1se])
rmse

# Ridge Regression
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv_ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv_ridge)

rmse = sqrt(cv_ridge$cvm[cv_ridge$lambda == cv_ridge$lambda.1se])
rmse

# PCR
library(pls)
pcr_fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr_fit)
