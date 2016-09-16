# Thomas Meagher HW1 Q5

AutoData <- read.csv(file="data/Auto.csv", header=TRUE, sep=",", na.strings="?")
AutoData=na.omit(AutoData)
names(AutoData)

# b
range(AutoData[1])

