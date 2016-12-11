# Thomas Meagher HW4 Q4
# Section 7.9, Page 298, question 4

x = seq(-2,2)
y = c(
  1 + 0 + 0, # x = -2
  1 + 0 + 0, # x = -1
  1 + 1 + 0, # x = 0
  1 + (1-0) + 0, # x = 1
  1 + (1-1) + 0 # x =2
)
lm(y~x)
plot(x,y)