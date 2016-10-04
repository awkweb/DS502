# Thomas Meagher HW2 Q9
# Section 5.4, page 199, question 6

# a
library(ISLR)
data(Default)
attach(Default)
set.seed(1)
glm.fit = glm(default~income+balance, family="binomial", data=Default)
summary(glm.fit)$coef[,1] # Same as coef(glm.fit)
summary(glm.fit)$coef

# b
boot.fn = function (data, index) {
  coef(glm(default~income+balance, family="binomial", data=data, subset=index))
}

# c
library(boot)
boot(Default, boot.fn, 1000)
