# Thomas Meagher HW2 Q8
# Section 5.4, page 198, question 5

# a
library(ISLR)
data(Default)
attach(Default)
set.seed(1)
glm.fit = glm(default~income+balance, family="binomial", data=Default)

# b
set.seed(1)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

# c
set.seed(5)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

set.seed(25)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

set.seed(50)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

# d
set.seed(5)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance+student, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

set.seed(25)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance+student, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)

set.seed(50)
train = sample(nrow(Default), nrow(Default)/2)
Default.train = Default[train,]
Default.test = Default[-train,]
glm.fit = glm(default~income+balance+student, family="binomial", data=Default.train)
glm.probs = predict(glm.fit, Default.test, type="response")
glm.pred = ifelse(glm.probs>.5, "Yes", "No")
mean(glm.pred != Default.test$default)
