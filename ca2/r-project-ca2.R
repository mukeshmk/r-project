library(readxl)
library(partykit)
library(rpart)
library(caret)
library(mice)

# install.packages('mice')

Project_Data <-read_excel("D:\\code\\data-analytics\\r-project\\ca2\\project-data.xlsx")

md.pattern(Project_Data)
tempData <- mice(Project_Data,m=5,maxit=50,meth='pmm',seed=500)
tempData$imp$Group
ImpProject_Data<-complete(tempData,1)

ImpProject_Data <- ImpProject_Data[,-1]

attach(ImpProject_Data)

set.seed(123)
train_ind <- sample(seq_len(nrow(ImpProject_Data)), size = floor(0.8 * nrow(ImpProject_Data)))

train_data <- data.frame(ImpProject_Data[train_ind, ])
test_data <- data.frame(ImpProject_Data[-train_ind, ])

proj_model_resp <-rpart(Response ~ ., data=train_data, method='class', control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))

plot(as.party(proj_model_resp))

# predict
predictions <- predict(proj_model_resp, test_data[c(2: 16)], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$Response, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# converting the 'Response' into a categorical value 'target'
target = ifelse(train_data$Response==1,'Y','N')

train_data <- data.frame(train_data, target)
# removing 'Response' from the data
train_data <- train_data[,-1]
str(train_data)

test_target = ifelse(test_data$Response==1,'Y','N')

test_data <- data.frame(test_data, test_target)
# removing 'Response' from the data
test_data <- test_data[,-1]
str(test_data)

# creating the same model with the Target variable as the categorical value
# model includes all the X's, Y's and Group
proj_model_tar <-rpart(train_data$target ~ ., data=train_data, method='class',control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
plot(as.party(proj_model_tar))

# predict
predictions <- predict(proj_model_tar, test_data[c(1: 15)], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# getting only X cols
xcols <- c(2:8)
Xdata = train_data[xcols]
names(Xdata)
# creating a model with only X's
proj_model_X <-rpart(target ~ ., data=Xdata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\X1-X7.png")
plot(as.party(proj_model_X))
dev.off()

# predict
predictions <- predict(proj_model_X, test_data[xcols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# getting only Y cols
ycols <- c(9:15)
Ydata = train_data[ycols]
names(Ydata)
# creating a model with only Y's
proj_model_Y <-rpart(target ~ ., data=Ydata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\Y1-Y7.png")
plot(as.party(proj_model_Y))
dev.off()

# predict
predictions <- predict(proj_model_Y, test_data[ycols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# creating a model with X's and Y's without Group
xycols = c(2:14)
XYdata = train_data[xycols]
names(XYdata)
proj_model_XY <-rpart(target ~ ., data=XYdata, method='class', 
                      control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\XYwoG.png")
plot(as.party(proj_model_XY))
dev.off()

# predict
predictions <- predict(proj_model_XY, test_data[xycols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy


# getting data of only X's and Group
XGcols <- c(1:8, 16)
XGdata = train_data[XGcols]
names(XGdata)
# spliting data based on Group
XGdata = split(XGdata, XGdata$Group)
# getting group 0 data
XGdata0 = XGdata[[1]]
names(XGdata0)
# creating a model with X's and Group 0
proj_model_XG0 <-rpart(XGdata0$target ~ ., data=XGdata0, method='class', 
                       control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\XwG.png")
plot(as.party(proj_model_XG0))
dev.off()

# predict
predictions <- predict(proj_model_XG0, test_data[XGcols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy


# getting group 1 data
XGdata1 = XGdata[[2]]
names(XGdata1)
# creating a model with X's and Group 1
proj_model_XG1 <-rpart(XGdata1$target ~ ., data=XGdata1, method='class', 
                       control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\XwG1.png")
plot(as.party(proj_model_XG1))
dev.off()

# predict
predictions <- predict(proj_model_XG1, test_data[XGcols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# getting data of only Y's and Group
YGcols <- c(1, 9:15, 16)
YGdata = train_data[YGcols]
names(YGdata)
# spliting data based on Group
YGdata = split(YGdata, YGdata$Group)
# getting group 0 data
YGdata0 = YGdata[[1]]
names(YGdata0)
# creating a model with Y's and Group 0
proj_model_YG0 <-rpart(YGdata0$target ~ ., data=YGdata0, method='class', 
                       control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\YwG0.png")
plot(as.party(proj_model_YG0))
dev.off()

# predict
predictions <- predict(proj_model_YG0, test_data[YGcols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# getting group 1 data
YGdata1 = YGdata[[2]]
names(YGdata1)
# creating a model with Y's and Group 1
proj_model_YG1 <-rpart(YGdata1$target ~ ., data=YGdata1, method='class', 
                       control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\YwG1.png")
plot(as.party(proj_model_YG1))
dev.off()

# predict
predictions <- predict(proj_model_YG1, test_data[YGcols], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# spliting data based on Group
XYGdata = split(train_data, train_data$Group)
# getting group 0 data
XYGdata0 = XYGdata[[1]]
names(XYGdata0)
# creating a model with XY's and Group 0
proj_model_XYG0 <-rpart(XYGdata0$target ~ ., data=XYGdata0, method='class', 
                        control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))
png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\XYwG0.png")
plot(as.party(proj_model_XYG0))
dev.off()

# predict
predictions <- predict(proj_model_XYG0, test_data, type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

# getting group 1 data
XYGdata1 = XYGdata[[2]]
names(XYGdata1)
# creating a model with XY's and Group 1
proj_model_XYG1 <-rpart(XYGdata1$target ~ ., data=XYGdata1, method='class', 
                        control=rpart.control(minsplit=30, minbucket=10, maxdepth=8 ))

png("D:\\code\\data-analytics\\r-project\\ca2\\plots\\XYG0.png")
plot(as.party(proj_model_XYG1))
dev.off()

# predict
predictions <- predict(proj_model_XYG1, test_data, type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
accuracy<-round(cm$overall[1],2)
accuracy

