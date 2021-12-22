library(readxl)
datap11 = read_excel("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/Materi/Data P11.xlsx")
head(datap11)

#K-Means
hasil2 = kmeans(datap11,2)
hasil2
all = cbind(datap11,hasil2$cluster)
all
