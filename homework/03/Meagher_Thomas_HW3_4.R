# Thomas Meagher HW3 Q4
# Section 6.8, page 263, question 9

# a
library(ISLR)
set.seed(11)
train_size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train_size)
test = -train
college_train = College[train,]
college_test = College[test,]

# b
lm_fit = lm(Apps ~ ., data = college_train)
lm_pred = predict(lm_fit, college_test)
mean((college_test[, "Apps"] - lm_pred)^2)

# c
library(glmnet)
matrix_train = model.matrix(Apps ~ ., data = college_train)
matrix_test = model.matrix(Apps ~ ., data = college_test)
grid = 10 ^ (seq(4, -2, length=100))
model_ridge <- cv.glmnet(matrix_train, college_train[, "Apps"], alpha = 0, lambda = grid, thresh = 1e-12)
lambda_best <- model_ridge$lambda.min
lambda_best

ridge_pred = predict(model_ridge, newx = matrix_test, s = lambda_best)
mean((college_test[, "Apps"] - ridge_pred)^2)

# d
model_lasso = cv.glmnet(matrix_train, college_train[, "Apps"], alpha = 1, lambda = grid, thresh = 1e-12)
lambda_best = model_lasso$lambda.min
lambda_best

lasso_pred = predict(model_lasso, newx = matrix_test, s = lambda_best)
mean((college_test[, "Apps"] - lasso_pred)^2)

sum(coef(model_lasso)[,1] == 0)
names(coef(model_lasso)[, 1][coef(model_lasso)[, 1] == 0])

# e
library(pls)
pcr_fit = pcr(Apps ~ ., data = college_train, scale = T, validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")

pcr_pred = predict(pcr_fit, college_test, ncomp = 10)
mean((college_test[, "Apps"] - data.frame(pcr_prediction))^2)

# f
pls_fit = plsr(Apps ~ ., data = college_train, scale = T, validation = "CV")
validationplot(pls_fit, val.type = "MSEP")

pls_pred = predict(pls_fit, college_test, ncomp = 10)
mean((college_test[, "Apps"] - data.frame(pls_pred))^2)

# g
test_avg = mean(college_test[, "Apps"])
mean_college_test = mean((college_test[, "Apps"] - test_avg)^2)
                         
lm_r2 = 1 - mean((college_test[, "Apps"] - lm_pred)^2) / mean_college_test
ridge_r2 = 1 - mean((college_test[, "Apps"] - ridge_regression)^2) / mean_college_test
lasso_r2 = 1 - mean((college_test[, "Apps"] - lasso_pred)^2) / mean_college_test
pcr_r2 = 1 - mean((college_test[, "Apps"] - data.frame(pcr_pred))^2) / mean_college_test
pls_r2 = 1 - mean((college_test[, "Apps"] - data.frame(pls_pred))^2) / mean_college_test

results <- data.frame(method = c("OLS", "Ridge", "Lasso", "PCR", "PLS"), r2 = c(lm_r2, ridge_r2, lasso_r2, pcr_r2, pls_r2))
ggplot(results, aes(method, r2)) +
  geom_bar(stat = "identity") +
  labs(title = "Test R-squared")












