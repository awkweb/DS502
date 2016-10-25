# Thomas Meagher HW3 Q3
# Section 6.8, page 262-263, question 8

# a
set.seed(1)
X <- rnorm(100)
epsilon <- rnorm(100)

# b
beta_0 <- 1.5
beta_1 <- 20
beta_2 <- -1.5
beta_3 <- 0.1
Y <- beta_0 + beta_1 * X + beta_2 * X^2 + beta_3 * X^3 + epsilon

# c
library(leaps)
data_frame = data.frame(y = Y, x = X)
regfit_full = regsubsets(y ~ poly(x, 10, raw = T), data = data_frame, nvmax = 10)
regfit_summary = summary(regfit_full)

attach(regfit_summary)
which.min(cp)
which.min(bic)
which.min(adjr2)

library(ggplot2)
ggplot() +
  geom_line(aes(x = seq(1, length(cp), 1), y = cp)) + 
  geom_point(aes(x = 3, y = cp[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Cp")

ggplot() +
  geom_line(aes(x = seq(1, length(bic), 1), y = bic)) + 
  geom_point(aes(x = 3, y = bic[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("BIC")

ggplot() +
  geom_line(aes(x = seq(1, length(adjr2), 1), y = adjr2)) + 
  geom_point(aes(x = 3, y = adjr2[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Ajdusted R^2")

coefficients(regfit_full, id = 3)

# d
forward_full = regsubsets(y ~ poly(x, 10, raw = T), data = data_frame, nvmax = 10, method = "forward")
backward_full = regsubsets(y ~ poly(x, 10, raw = T), data = data_frame, nvmax = 10, method = "backward")
forward_summary = summary(forward_full)
backward_summary = summary(backward_full)

which.min(forward_summary$cp)
which.min(forward_summary$bic)
which.min(forward_summary$adjr2)

which.min(backward_summary$cp)
which.min(backward_summary$bic)
which.min(backward_summary$adjr2)

ggplot() +
  geom_line(aes(x = seq(1, length(forward_summary$cp), 1), y = forward_summary$cp)) + 
  geom_point(aes(x = 3, y = forward_summary$cp[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Cp")

ggplot() +
  geom_line(aes(x = seq(1, length(forward_summary$bic), 1), y = forward_summary$bic)) + 
  geom_point(aes(x = 3, y = forward_summary$bic[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("BIC")

ggplot() +
  geom_line(aes(x = seq(1, length(forward_summary$adjr2), 1), y = forward_summary$adjr2)) + 
  geom_point(aes(x = 3, y = forward_summary$adjr2[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Ajdusted R^2")

ggplot() +
  geom_line(aes(x = seq(1, length(backward_summary$cp), 1), y = backward_summary$cp)) + 
  geom_point(aes(x = 3, y = backward_summary$cp[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Cp")

ggplot() +
  geom_line(aes(x = seq(1, length(backward_summary$bic), 1), y = backward_summary$bic)) + 
  geom_point(aes(x = 3, y = backward_summary$bic[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("BIC")

ggplot() +
  geom_line(aes(x = seq(1, length(backward_summary$adjr2), 1), y = backward_summary$adjr2)) + 
  geom_point(aes(x = 3, y = backward_summary$adjr2[3]), colour = "red", size = 2) +
  xlab("Subset size") +
  ylab("Ajdusted R^2")

coefficients(forward_full, id = 3)
coefficients(backward_full, id = 3)
coefficients(forward_full, id = 1)
coefficients(backward_full, id = 1)

# e
library(glmnet)
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data_frame)[, -1]
lasso = cv.glmnet(xmat, Y, alpha = 1)
lambda_best = lasso$lambda.min
lambda_best

plot(lasso)

model_best = glmnet(xmat, Y, alpha = 1)
predict(model_best, s = lambda_best, type = "coefficients")

# f
beta_7 <- 7
Y <- beta_0 + beta_7 * X^7 + epsilon
data_frame <- data.frame(y = Y, x = X)
regfit_full <- regsubsets(y ~ poly(x, 10, raw = T), data = data_frame, nvmax = 10)
regfit_summary <- summary(regfit_full)

which.min(regfit_summary$cp)
which.min(regfit_summary$bic)
which.min(regfit_summary$adjr2)

coefficients(regfit_full, id = 2)
coefficients(regfit_full, id = 1)
coefficients(regfit_full, id = 10)

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data_frame)[, -1]
lasso = cv.glmnet(xmat, Y, alpha = 1)
lambda_best = lasso$lambda.min
lambda_best

plot(lasso)

model_best = glmnet(xmat, Y, alpha = 1)
predict(model_best, s = lambda_best, type = "coefficients")
