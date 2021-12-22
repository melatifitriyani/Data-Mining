library(readxl)
datap12 = read_excel("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/Materi/Data P11.xlsx")
datap12

library(cluster)
#AGNES
#metric = "euclidean" atau "manhattan"
#method = "average ([unweighted pair-]group [arithMetic] average method, aka 'UPGMA')
#         "single" (single linkage)
#         "complete" (complete linkage)
#         "ward" (ward's method)
#         weighted" (weighted average linkage, aka 'WPGMA')
hcA = agnes(datap12[,-1], metric="euclidean", method="single")
hcA
#Plot Dendogram
pltree(hcA, cex = 0.6, hang = -1, main = "Dendogram of AGNES")
hcA.cluster = cutree(hcA, k=2)
hcA.cluster
rect.hclust(hcA, k=2, border = 2:10)

#DIANA
#metric = "euclidean" atau "manhattan"
hcD = diana(datap12[,-1], metric = "euclidean")
hcD
#Plot Dendogram
pltree(hcD, cex = 0.6, hang = -1, main = "Dendogram of DIANA")
hcD.cluster = cutree(hcD, k=2)
hcD.cluster
rect.hclust(hcD, k=2, border = 2:10)
