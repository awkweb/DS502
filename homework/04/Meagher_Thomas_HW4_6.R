# Thomas Meagher HW4 Q6
# Section 7.9, Page 299, question 7

library(ISLR)
set.seed(1)
attach(Wage)

summary(maritl)
summary(jobclass)

plot(maritl, wage)
plot(jobclass, wage)

lm_maritl = lm(wage~maritl, data=Wage)
lm_jobclass = lm(wage~jobclass, data=Wage)
lm_maritl_jobclass = lm(wage~maritl+jobclass, data=Wage)
library(gam)
lm_gam = gam(wage~maritl+jobclass+s(age,4)+s(year,5), data=Wage)

anova(lm_maritl, lm_jobclass, lm_maritl_jobclass, lm_gam)

