# Thomas Meagher HW4 Q3
# Section 7.9, Page 298, question 3

x = seq(-2,2)
y = 1 + x + -2 * (x-1)^2 * (x>1)
lm(y~x)
plot(x, y)
