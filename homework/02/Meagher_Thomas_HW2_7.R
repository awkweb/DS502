# Thomas Meagher HW2 Q7
# Section 5.4, page 197, question 2

prob_in_boot.fn = function (n) {
  return (1 - ((1 - 1/n)^n))
}

# d
prob_in_boot.fn(5)

# e
prob_in_boot.fn(100)

# f
prob_in_boot.fn(10000)

# g
x = seq(1, 100000)
y = sapply(x, function (n) { prob_in_boot.fn(n) })
plot(x, y, xlab="n", ylab="probability", log="x")

# h
store = rep(NA, 10000)
for (i in 1:10000) {
  store[i] = sum(sample(1:100, rep=TRUE) == 4) > 0 
}
mean(store)
