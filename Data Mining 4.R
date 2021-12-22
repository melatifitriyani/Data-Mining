#Importing The Dataset
dataset = read.csv('E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/Social_Network_Ads.csv')
dataset = dataset[,3:5]
head(dataset)

#Encoding The Target Feature as Factor
dataset$Purchased = factor(dataset$Purchased, levels=c(0,1))

#Feature Scalling (x-mean)/sd
dataset[-3] = scale(dataset[-3])
head(dataset)
nrow(dataset)

#Splitting The Dataset Into The Training Set and Test Set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
head(train_set)
head(test_set)
nrow(train_set)
nrow(test_set)

#Fitting SVM to The Training Set
#Pilihan Fungsi Kernel
library(e1071)
modelSVM = svm(formula = Purchased ~ ., data = train_set, type = 'C-classification', kernel = 'linear')
modelSVM
#Prediksi Data Training
pred.train = predict(modelSVM, newdata = train_set[-3])
head(pred.train)
#Prediksi Data Testing
pred.test = predict(modelSVM, newdata = test_set[-3])
head(pred.test)
#Meghitung Akurasi SVM Berdasarkan Data Training
ts1 = table(train_set[,3], pred.train)
akurasi.train = sum(diag(ts1))/sum(ts1)*100
akurasi.train
#Menghitung Akurasi SVM Berdasarkan Data Testing
ts2 = table(test_set[,3], pred.test)
akurasi.test = sum(diag(ts2))/sum(ts2)*100
akurasi.test
