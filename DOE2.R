workingDir = "I:/doe";
setwd(workingDir);
mydata<-read.csv("Car_sales.csv")

library(randomForest)
library(stats)
library(dplyr)
library(caret)
library(MASS)


carsdata=as.data.frame(mydata)
carsdata=carsdata[,-c(1,2,4,5,6,7,9:12,15)]
carsdata=na.omit(carsdata)
dim(carsdata)
View(carsdata)
str(carsdata)
attach(carsdata)

model=lm(Sales_in_thousands~.,data=carsdata)
print(model)

hist(carsdata$Sales_in_thousands, main = "Sales of carsdata",
     col =c("blue","red", "green","violet"))


mean(carsdata$Sales_in_thousands)

highsales = ifelse(carsdata$Sales_in_thousands<30, "No", "Yes") 
highsales= as.factor(highsales)
CD = data.frame(carsdata[2:5], highsales)
View(CD)
str(CD)

table(CD$highsales)


set.seed(123)
# Split the data into train and test in the ratio 70:30 respectively
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(123)
rf <- randomForest(highsales~., data=train)
rf 

attributes(rf)

pred1 <- predict(rf, train)
head(pred1)

head(train$highsales)
confusionMatrix(pred1, train$highsales) 

pred2 <- predict(rf, test)
head(test$highsales)
confusionMatrix(pred2, test$highsales)

plot(rf)

tune <- tuneRF(train[,-4], train[,4], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales) 

pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales) 


hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

varImpPlot(rf1)


importance(rf1)
varUsed(rf)

MDSplot(rf1, CD$highsales)

