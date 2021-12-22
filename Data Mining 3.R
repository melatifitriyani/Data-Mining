#Import data
library(readxl)
sample1 <- read_excel("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/sample1.xlsx") 

#Membuat train data dan test data
traindata <- as.data.frame(sample1[1:14,])
testdata <- as.data.frame(sample1[16:17,])
traindata
testdata

#Menghitung banyaknya Enrolls=yes dan Enrolls=no
n.prior <- table(traindata$Enrolls)
n.prior

#Menghitung prior probability P(Ci) untuk Enrolls, dengan Ci???C dan C={yes,no}
p.prior <- n.prior/sum(n.prior)
p.prior

#Menghitung conditional probabilities P(X|C) untuk setiap kelas
n.age <- table(traindata[,c("Enrolls","age")])
p.age <- n.age/rowSums(n.age)
p.age

n.income <- table(traindata[,c("Enrolls","income")])
p.income <- n.income/rowSums(n.income)
p.income

n.JobStfc <- table(traindata[,c("Enrolls","JobSatisfaction")])
p.JobStfc <- n.JobStfc/rowSums(n.JobStfc)
p.JobStfc

n.desire <- table(traindata[,c("Enrolls","Desire")])
p.desire <- n.desire/rowSums(n.desire)
p.desire

#Menghitung P(X|Ci)*P(Ci) untuk menentukan kelas data test
p.age["yes",testdata[,c("age")]]
p.income["yes",testdata[,c("income")]]
p.JobStfc["yes",testdata[,c("JobSatisfaction")]]
p.desire["yes",testdata[,c("Desire")]]
p.prior["yes"]

p.age["no",testdata[,c("age")]]
p.income["no",testdata[,c("income")]]
p.JobStfc["no",testdata[,c("JobSatisfaction")]]
p.desire["no",testdata[,c("Desire")]]
p.prior["no"]

prob_yes <- p.age["yes",testdata[,c("age")]]*
  p.income["yes",testdata[,c("income")]]*
  p.JobStfc["yes",testdata[,c("JobSatisfaction")]]*
  p.desire["yes",testdata[,c("Desire")]]*
  p.prior["yes"]
prob_yes
prob_no <- p.age["no",testdata[,c("age")]]*
  p.income["no",testdata[,c("income")]]*
  p.JobStfc["no",testdata[,c("JobSatisfaction")]]*
  p.desire["no",testdata[,c("Desire")]]*
  p.prior["no"]
prob_no