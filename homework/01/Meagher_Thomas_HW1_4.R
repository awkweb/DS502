# Thomas Meagher HW1 Q4
# Section 2.4, page 54-55, question 8

# a) read College data set
college_data <- read.csv(file="data/College.csv", header=TRUE, sep=",")

# b
rownames(college_data)=college_data[,1]
fix(college_data)

college_data=college_data[,-1]
fix(college_data)

# c
# i
summary(college_data)

# ii
pairs(college_data[,1:10])

# iii
attach(college_data)
Private=as.factor(Private)
plot(Private, Outstate, xlab="Private", ylab="Tuition", main="Tuition at Non-Private/Private")

# iv
Elite=rep("No",nrow(college_data))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college_data=data.frame(college_data, Elite)
summary(college_data)
plot(Elite, Outstate, xlab="Elite", ylab="Tuition", main="Tuition at Non-Elite/Elite")

# v
hist(Accept/Apps, breaks=15)
hist(F.Undergrad, breaks=5)
hist(Grad.Rate, breaks=30)

# vi

