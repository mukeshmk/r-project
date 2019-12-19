#install.packages("party")
#nstall.packages ("partykit") 
#nstall.packages ("rpart") 

library(party)
library("partykit") 
library("rpart") 
  
Data <- read.csv("D:\\code\\data-analytics\\ca-2\\data.csv", header=TRUE, sep= ",")
print(head(Data))
#head(Data$X2)
#handle null data
random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace = TRUE)
  return(imputed)
}

#fill null data of X
Data$X1 <- random.imp(Data$X1)
Data$X2 <- random.imp(Data$X2)
Data$X3 <- random.imp(Data$X3)
Data$X4 <- random.imp(Data$X4)
Data$X5 <- random.imp(Data$X5)
Data$X6 <- random.imp(Data$X6)
Data$X7 <- random.imp(Data$X7)
#fill null data of Y
Data$Y1 <- random.imp(Data$Y1)
Data$Y2 <- random.imp(Data$Y2)
Data$Y3 <- random.imp(Data$Y3)
Data$Y4 <- random.imp(Data$Y4)
Data$Y5 <- random.imp(Data$Y5)
Data$Y6 <- random.imp(Data$Y6)
Data$Y7 <- random.imp(Data$Y7)

#check if any null values
sum(is.na(Data))


#Decision tree using X data
print(head(Data[c(4:10)]))
# Create the input data frame.
input_X <- Data[c(4:10)]
str(input_X) 
attach(input_X)
DT_Model_X <-rpart(Data$Response~., data=input_X, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_X)) 
print(DT_Model_X)
#Prune the tree
opt <- which.min(DT_Model_X$cptable [, "xerror"])
cp <- DT_Model_X$cptable [opt,"CP"]
DT_Model_X_pruned <- prune(DT_Model_X, cp=cp)
plot(as.party(DT_Model_X_pruned))
#Calculate accuracy of tree
prediction_X = predict(DT_Model_X_pruned, Data, type="class")
table_mat_X <- table(Data$Response, prediction_X)
accuracy_Test_X <- sum(diag(table_mat_X)) / sum(table_mat_X)
print(paste("Accuracy for Decision tree using X data: ",accuracy_Test_X))
print(DT_Model_X_pruned)
summary(DT_Model_X_pruned)

#Decision tree using Y data
print(head(Data[c(11:17)]))
# Create the input data frame.
input_Y <- Data[c(11:17)]
str(input_Y) 
attach(input_Y)
DT_Model_Y <-rpart(Data$Response~., data=input_Y, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_Y)) 
print(DT_Model_Y)
#Prune the tree
opt <- which.min(DT_Model_Y$cptable [, "xerror"])
cp <- DT_Model_Y$cptable [opt,"CP"]
DT_Model_Y_pruned <- prune(DT_Model_Y, cp=cp)
plot(as.party(DT_Model_Y_pruned))
#Calculate accuracy of tree
prediction_Y = predict(DT_Model_Y_pruned, Data, type="class")
table_mat_Y <- table(Data$Response, prediction_Y)
accuracy_Test_Y <- sum(diag(table_mat_Y)) / sum(table_mat_Y)
print(paste("Accuracy for Decision tree using Y data: ",accuracy_Test_Y))
summary(DT_Model_Y_pruned)

#Decision tree using X & Y data 
print(head(Data[c(4:17)]))
# Create the input data frame.
input_XY <- Data[c(4:17)]
str(input_XY) 
attach(input_XY)
DT_Model_XY <-rpart(Data$Response~., data=input_XY, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_XY)) 
print(DT_Model_XY)
#Prune the tree
opt <- which.min(DT_Model_XY$cptable [, "xerror"])
cp <- DT_Model_XY$cptable [opt,"CP"]
DT_Model_XY_pruned <- prune(DT_Model_XY, cp=cp)
plot(as.party(DT_Model_XY_pruned))
#Calculate accuracy of tree
prediction_XY = predict(DT_Model_XY_pruned, Data, type="class")
table_mat_XY <- table(Data$Response, prediction_XY)
accuracy_Test_XY <- sum(diag(table_mat_XY)) / sum(table_mat_XY)
print(paste("Accuracy for Decision tree using X & Y data: ",accuracy_Test_XY))
summary(DT_Model_XY_pruned)

#Decision tree using X data for group 0
# Create the input data frame.
input_X_Group <- Data[c(2:10)]
input_X_G0 <- subset(input_X_Group, Group == 0)
print(head(input_X_G0))
str(input_X_G0) 
attach(input_X_G0)
DT_Model_X_G0 <-rpart(Response~., data=input_X_G0, method='class',control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_X_G0)) 
print(DT_Model_X_G0)
#Prune the tree
opt <- which.min(DT_Model_X_G0$cptable [, "xerror"])
cp <- DT_Model_X_G0$cptable [opt,"CP"]
DT_Model_X_G0_pruned <- prune(DT_Model_X_G0, cp=cp)
plot(as.party(DT_Model_X_G0_pruned))
#Calculate accuracy of tree
prediction_X_G0 = predict(DT_Model_X_G0_pruned, Data, type="class")
table_mat_X_G0 <- table(Data$Response, prediction_X_G0)
accuracy_Test_X_G0 <- sum(diag(table_mat_X_G0)) / sum(table_mat_X_G0)
print(paste("Accuracy for Decision tree using X data for group 0: ",accuracy_Test_X_G0))
summary(DT_Model_X_G0_pruned)

#Decision tree using Y data for group 0
# Create the input data frame.
input_Y_Group <- Data[c(2,3,11,12,13,14,15,16,17)]
input_Y_G0 <- subset(input_Y_Group, Group == 0)
str(input_Y_G0) 
attach(input_Y_G0)
DT_Model_Y_G0 <-rpart(Response~., data=input_Y_G0, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_Y_G0)) 
print(DT_Model_Y_G0)
#Prune the tree
opt <- which.min(DT_Model_Y_G0$cptable [, "xerror"])
cp <- DT_Model_Y_G0$cptable [opt,"CP"]
DT_Model_Y_G0_pruned <- prune(DT_Model_Y_G0, cp=cp)
plot(as.party(DT_Model_Y_G0_pruned))
#Calculate accuracy of tree
prediction_Y_G0 = predict(DT_Model_Y_G0_pruned, Data, type="class")
table_mat_Y_G0 <- table(Data$Response, prediction_Y_G0)
accuracy_Test_Y_G0 <- sum(diag(table_mat_Y_G0)) / sum(table_mat_Y_G0)
print(paste("Accuracy for Decision tree using Y data for group 0: ",accuracy_Test_Y_G0))
summary(DT_Model_Y_G0_pruned)

#Decision tree using X & Y data for group 0
# Create the input data frame.
input_XY_Group <- Data[c(2:17)]
input_XY_G0 <- subset(input_XY_Group, Group == 0)
str(input_XY_G0) 
attach(input_XY_G0)
DT_Model_XY_G0 <-rpart(Response~., data=input_XY_G0, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_XY_G0)) 
print(DT_Model_XY_G0)
#Prune the tree
opt <- which.min(DT_Model_XY_G0$cptable [, "xerror"])
cp <- DT_Model_XY_G0$cptable [opt,"CP"]
DT_Model_XY_G0_pruned <- prune(DT_Model_XY_G0, cp=cp)
plot(as.party(DT_Model_XY_G0_pruned))
#Calculate accuracy of tree
prediction_XY_G0 = predict(DT_Model_XY_G0_pruned, Data, type="class")
table_mat_XY_G0 <- table(Data$Response, prediction_XY_G0)
accuracy_Test_XY_G0 <- sum(diag(table_mat_XY_G0)) / sum(table_mat_XY_G0)
print(paste("Accuracy for Decision tree using X & Y data for group 0: ",accuracy_Test_XY_G0))
summary(DT_Model_XY_G0_pruned)

#Decision tree using X data for group 1
# Create the input data frame.
input_X_G1 <- subset(input_X_Group, Group == 1)
print(head(input_X_G1))
str(input_X_G1) 
attach(input_X_G1)
DT_Model_X_G1 <-rpart(Response~., data=input_X_G1, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_X_G1)) 
print(DT_Model_X_G1)
#Prune the tree
opt <- which.min(DT_Model_X_G1$cptable [, "xerror"])
cp <- DT_Model_X_G1$cptable [opt,"CP"]
DT_Model_X_G1_pruned <- prune(DT_Model_X_G1, cp=cp)
plot(as.party(DT_Model_X_G1_pruned))
#Calculate accuracy of tree
prediction_X_G1 = predict(DT_Model_X_G1_pruned, Data, type="class")
table_mat_X_G1 <- table(Data$Response, prediction_X_G1)
accuracy_Test_X_G1 <- sum(diag(table_mat_X_G1)) / sum(table_mat_X_G1)
print(paste("Accuracy for Decision tree using X data for group 1: ",accuracy_Test_X_G1))
summary(DT_Model_X_G1_pruned)

#Decision tree using Y data for group 1
# Create the input data frame.
input_Y_G1 <- subset(input_Y_Group, Group == 1)
print(head(input_Y_G1))
str(input_Y_G1) 
attach(input_Y_G1)
DT_Model_Y_G1 <-rpart(Response~., data=input_Y_G1, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_Y_G1)) 
print(DT_Model_Y_G1)
#Prune the tree
opt <- which.min(DT_Model_Y_G1$cptable [, "xerror"])
cp <- DT_Model_Y_G1$cptable [opt,"CP"]
DT_Model_Y_G1_pruned <- prune(DT_Model_Y_G1, cp=cp)
plot(as.party(DT_Model_Y_G1_pruned))
#Calculate accuracy of tree
prediction_Y_G1 = predict(DT_Model_Y_G1_pruned, Data, type="class")
table_mat_Y_G1 <- table(Data$Response, prediction_Y_G1)
accuracy_Test_Y_G1 <- sum(diag(table_mat_Y_G1)) / sum(table_mat_Y_G1)
print(paste("Accuracy for Decision tree using Y data for group 1: ",accuracy_Test_Y_G1))
summary(DT_Model_Y_G1_pruned)

#Decision tree using X & Y data for group 1
input_XY_G1 <- subset(input_XY_Group, Group == 1)
print(head(input_XY_G1))
str(input_XY_G1) 
attach(input_XY_G1)
DT_Model_XY_G1 <-rpart(Response~., data=input_XY_G1, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8))
plot(as.party(DT_Model_XY_G1)) 
print(DT_Model_XY_G1)
#Prune the tree
opt <- which.min(DT_Model_XY_G1$cptable [, "xerror"])
cp <- DT_Model_XY_G1$cptable [opt,"CP"]
DT_Model_XY_G1_pruned <- prune(DT_Model_XY_G1, cp=cp)
plot(as.party(DT_Model_XY_G1_pruned))
summary(DT_Model_XY_G1_pruned)
#Calculate accuracy of tree
prediction_XY_G1 = predict(DT_Model_XY_G1_pruned, Data, type="class")
table_mat_XY_G1 <- table(Data$Response, prediction_XY_G1)
accuracy_Test_XY_G1 <- sum(diag(table_mat_XY_G1)) / sum(table_mat_XY_G1)
print(paste("Accuracy for Decision tree using X & Y data for group 1: ",accuracy_Test_XY_G1))
summary(DT_Model_XY_G1_pruned)



print(paste("Accuracy for Decision tree using X data: ",accuracy_Test_X))
print(paste("Accuracy for Decision tree using Y data: ",accuracy_Test_Y))
print(paste("Accuracy for Decision tree using X & Y data: ",accuracy_Test_XY))
print(paste("Accuracy for Decision tree using X data for group 0: ",accuracy_Test_X_G0))
print(paste("Accuracy for Decision tree using Y data for group 0: ",accuracy_Test_Y_G0))
print(paste("Accuracy for Decision tree using X & Y data for group 0: ",accuracy_Test_XY_G0))
print(paste("Accuracy for Decision tree using X data for group 1: ",accuracy_Test_X_G1))
print(paste("Accuracy for Decision tree using Y data for group 1: ",accuracy_Test_Y_G1))
print(paste("Accuracy for Decision tree using X & Y data for group 1: ",accuracy_Test_XY_G1))

