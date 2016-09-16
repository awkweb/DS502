# Thomas Meagher HW1 Q4

# a) read College data set
CollegeData <- read.csv(file="data/College.csv", header=TRUE, sep=",")

# b
rownames(CollegeData)=CollegeData[,1]
fix(CollegeData)

CollegeData=CollegeData[,-1]
fix(CollegeData)

# c
# i
summary(CollegeData)

# ii
pairs(CollegeData[,1:10])

# iii
attach(CollegeData)
Private=as.factor(Private)
plot(Private, Outstate, xlab="Private", ylab="Tuition")

# iv
Elite=rep("No",nrow(CollegeData))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
CollegeData=data.frame(CollegeData,Elite)
summary(CollegeData)
plot(Elite,Outstate, xlab="Elite", ylab="Tuition")

# v
hist(Accept/Apps,breaks=15)
hist(F.Undergrad,breaks=5)
hist(Grad.Rate,breaks=30)

# vi





