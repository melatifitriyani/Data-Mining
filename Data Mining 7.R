##Data Preparations
#Read Datasets
train = read.csv("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/Materi/BikeRental/bike_rental_train.csv")
test = read.csv("E:/UNIVERSITAS BRAWIJAYA/SEMESTER 6/DATA MINING/Materi/BikeRental/bike_rental_test.csv")

#Show
head(train)
nrow(train)
head(test)
nrow(test)


##Support Vector Machines - Regression
library(e1071)

#Train
#type: C-classification, nu-classification, one-classification, eps-regression, nu-regression
#kernel: linear, polynomial, radial basis, signoid
model.SVR = svm(bike_rent_count~., train, type='eps-regression', kernel='linear')
summary(model.SVR)
coef(model.SVR) #Hanya Untuk Kernel Linier

#RMSE Test
pred.SVR <- predict(model.SVR, newdata=test)
err.SVR <- test$bike_rent_count - pred.SVR
rmse.SVR_test <- sqrt(mean(err.SVR^2))
rmse.SVR_test

#RMSE Train
res.SVR <- train$bike_rent_count - predict(model.SVR, newdata=train)
stdres.SVR <- scale(res.SVR)
rmse.SVR_train <- sqrt(mean(res.SVR^2))
rmse.SVR_train

##Regresi Linier
model.reg <- lm(bike_rent_count~., data=train)
summary(model.reg)
coef(model.reg)

#RMSE Test
pred.reg <- predict(model.reg, newdata=test)
err.reg <- test$bike_rent_count - pred.reg
rmse.reg <- sqrt(mean(err.reg^2))
rmse.reg

#RMSE Train
res.reg <- train$bike_rent_count - predict(model.reg, newdata=train)
stdres.reg <- scale(res.reg)
rmse.SVR_train <- sqrt(mean(res.reg^2))
rmse.SVR_train
