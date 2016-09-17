# Thomas Meagher HW1 Q10
# Section 3.7, page 122, question 9

auto_data <- read.csv(file="data/Auto.csv", header=TRUE, sep=",", na.strings="?")
auto_data=na.omit(auto_data)

# a) scatterplot matrix
pairs(auto_data)

# b) matrix of correlations
auto_data_quantitative <- subset(auto_data, select = -c(name))
cor(auto_data_quantitative)

# c) multiple linear regression
lm.fit=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data=auto_data_quantitative)
summary(lm.fit)

# i
# Yes, there is a relationship between the predictors and response. If you isolate one predictor
# and hold the others constant, then for an unit increase in that variable, mpg will likely change.

# ii
# Origin, year, weight, and displacement all apear to have a statistically significant relationship to the
# response. This is because each has a p-value less than 0.05, meaning they are significant in the model.
# While origin, year, and weight's p-values are very small, displacement's (0.008) is quite a bit larger than
# the others.

# iii
# The coefficient for the year variable suggests (if holding all the other predictors constant) an increase in
# one year will lead to an average increase of 0.75 miles per gallon. We can say this with 95% confidence.

# d) diagnostic plots
par(mfrow=c(2, 2))
plot(lm.fit)

# e) interaction effects
lm.fit2=lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year*origin, data=auto_data_quantitative)
summary(lm.fit2)
# year * origin is statistically significant

lm.fit3=lm(mpg~cylinders+displacement+horsepower*acceleration+weight+year+origin, data=auto_data_quantitative)
summary(lm.fit3)
# horsepower * acceleration is statistically significant, but not significant in the original model

lm.fit4=lm(mpg~cylinders*displacement+horsepower+acceleration+weight+year+origin, data=auto_data_quantitative)
summary(lm.fit4)
# cylinders * displacement is statistically significant, but not significant in the original model

lm.fit5=lm(mpg~cylinders*displacement+horsepower*acceleration+weight+year+origin, data=auto_data_quantitative)
summary(lm.fit5)
# Using the interaction terms: cylinders * displacement and horsepower * acceleration, all of the predictors become
# significant in the model (although to varying degrees). Horsepower (p-value: 0.04825) is still significant because
# it is less than 0.05, but is not as significant as horsepower * acceleration (2.40e-05).

# f) predictor transformations
lm.fit6=lm(mpg~cylinders+displacement+horsepower+poly(weight,5)+acceleration+year+origin, data=auto_data_quantitative)
summary(lm.fit6)
# Interestingly, transforming weight up to a fourth order has a significant effect for each polynomial, except the third
# and fifth transformations.

lm.fit7=lm(mpg~cylinders+displacement+sqrt(horsepower)+weight+acceleration+year+origin, data=auto_data_quantitative)
summary(lm.fit7)
# Horsepower (not significant in the original model) is significant with a square root transformation

lm.fit8=lm(mpg~cylinders+displacement+log(horsepower)+weight+acceleration+year+origin, data=auto_data_quantitative)
summary(lm.fit8)
# A logarithmic transformation of horsepower makes log(horsepower) and acceleration significant, when neither were in the
# original model

# Overall, applying different transformations of the predictors causes them to fit the data better (****CHECK). Feature engineering?








