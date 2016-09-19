# Thomas Meagher HW1 Q5
# Section 2.4, page 56, question 9

auto_data <- read.csv(file="data/Auto.csv", header=TRUE, sep=",", na.strings="?")
auto_data=na.omit(auto_data)
attach(auto_data)

# a
# quantitative
# mpg, displacement, horsepower, weight, acceleration, year
quantitative <- subset(auto_data, select = c(mpg, displacement, horsepower, weight, acceleration, year))

# qualitative
# cylinders, origin, name
cylinders=as.factor(cylinders)
origin=as.factor(origin)
name=as.factor(name)

# b
sapply(quantitative, range)

# c
sapply(quantitative, mean)
sapply(quantitative, sd)

# d
quantitative <- quantitative[-c(10,85),]
sapply(quantitative, range)
sapply(quantitative, mean)
sapply(quantitative, sd)

# e
pairs(quantitative)
par(mfrow=c(2,3))
plot(mpg, acceleration)
plot(horsepower, weight)
plot(mpg, weight)
plot(cylinders, mpg)
plot(cylinders, weight)
plot(year, mpg)
hist(mpg)

# f
pairs(quantitative)
