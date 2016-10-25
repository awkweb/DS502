# Thomas Meagher HW3 Q2
# Section 6.8, page 261, question 6 

# a
y <- 4
lambda <- 4
betas <- seq(-10, 10, 0.1)
func <- ((y - betas)^2) + (lambda * (betas^2))
est_beta <- y / (1 + lambda)
est_func <- ((y - est_beta)^2) + (lambda * (est_beta^2))

library(ggplot2)
ggplot() +
  geom_point(aes(x = betas, y = func)) + 
  geom_point(aes(x = est_beta, y = est_func), colour = "red", size = 2) +
  xlab("Beta") +
  ylab("Ridge optimization") +
  labs(title = "6.12 Solved By 6.14")

# b
y = 4
lambda = 4
betas = seq(-3, 3, 0.01)
func = ((y - betas)^2) + (lambda * abs(betas))
est_beta = y - lambda/2
est_func = ((y - est_beta)^2) + (lambda * abs(est_beta))

ggplot() +
  geom_point(aes(x = betas, y = func)) + 
  geom_point(aes(x = est_beta, y = est_func), colour = "red", size = 2) +
  xlab("Beta") +
  ylab("Lasso optimization") +
  labs(title = "6.13 Solved By 6.15")
