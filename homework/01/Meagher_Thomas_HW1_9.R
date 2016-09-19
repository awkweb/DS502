# Thomas Meagher HW1 Q9
# Section 3.7, page 121-122, question 8

auto_data <- read.csv(file="data/Auto.csv", header=TRUE, sep=",", na.strings="?")
auto_data=na.omit(auto_data)

# a
lm.fit=lm(mpg~horsepower, data=auto_data)
summary(lm.fit)

# i
# Yes, there is a strong relationship between the predictor and response because
# the residual standard error (RSE),4.906, is less than 10%. Also, the coefficient of determination
# is 0.6059 -- fairly close to one, meaning a decent amount of the variation in mpg is
# explained by horsepower.

# ii
# Based on the class RSE guidelines, the RSE (4.906) is less than 10% so it's a very strong relationship.
# If the RSE is small, it means that the regression line fit the data well enough so the difference between
# the observed and predicted values (a.k.a. residual) were minimized, resulting in less error for the model.

# iii
# The relationship between the predictor and response is negative because the coefficient of the predictor's
# estimate is negative (-0.1578). This means that for every unit of horsepower that is added, mpg decreases by
# an average of -0.1578 miles per gallon.

# iv
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")

# b
plot(horsepower, mpg)
abline(lm.fit, col="red")

# c
par(mfrow=c(2, 2))
plot(lm.fit)
