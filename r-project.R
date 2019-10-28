library(rpart)
library(partykit)
library(readxl)
library(caret)

rm(list=ls())

proj_data = read_excel("project-data.xlsx")
# removing the 'id' column
proj_data <- proj_data[,-1]
# used to display the names of the columns
names(proj_data)
# used to attach the data_set to R's search path so that columns can be accessed by
# just using there names as variables
attach(proj_data) 
# structure of the data_set
str(proj_data)

# setting the random seed so that partition is reproducible
set.seed(456)
train_ind <- sample(seq_len(nrow(proj_data)), size = floor(0.8 * nrow(proj_data)))

# Spliting the Data Set into Training and Testing data
train_data <- data.frame(proj_data[train_ind, ])
test_data <- data.frame(proj_data[-train_ind, ])

# creating a model where all the data (.) is used and 'Response' is the the Target variable (~)
# method='class' is used for a classification tree
# model includes all the X's, Y's and Group
proj_model_resp <-rpart(Response ~ ., data=train_data, method='class', 
                        control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
# ploting the DT with all the data
plot(as.party(proj_model_resp))

# prediciting the values of target variable based on the above model
predictions <- predict(proj_model_resp, test_data[c(2: 16)], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$Response, predictions))
round(cm$overall[1],2)

# converting the 'Response' into a categorical value 'target'
target = ifelse(train_data$Response==1,'Y','N')

train_data <- data.frame(train_data, target)
# removing 'Response' from the data
train_data <- train_data[,-1]

test_target = ifelse(test_data$Response==1,'Y','N')
test_data <- data.frame(test_data, test_target)
test_data <- test_data[,-1]

# creating the same model with the Target variable as the categorical value
# model includes all the X's, Y's and Group
proj_model_tar <-rpart(train_data$target ~ ., data=train_data, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_tar))

# prediciting the values of target variable based on the above model
predictions <- predict(proj_model_tar, test_data[c(1: 15)], type='class')
# summarize results
cm<-confusionMatrix(table(test_data$test_target, predictions))
round(cm$overall[1],2)

# getting only X cols
Xdata = train_data[c(2:8)]
# creating a model with only X's
proj_model_X <-rpart(target ~ ., data=Xdata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_X))

# getting only Y cols
Ydata = train_data[c(9:15)]
# creating a model with only Y's
proj_model_Y <-rpart(target ~ ., data=Ydata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_Y))

# creating a model with X's and Y's without Group
XYdata = train_data[c(2:14)]
proj_model_XY <-rpart(target ~ ., data=XYdata, method='class', 
                      control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XY))

# getting data of only X's and Group
XGdata = train_data[c(1:8, 16)]
# spliting data based on Group
XGdata = split(XGdata, XGdata$Group)
XGdata0 = XGdata[[1]]
XGdata1 = XGdata[[2]]

# creating a model with X's and Group 0
proj_model_XG0 <-rpart(XGdata0$target ~ ., data=XGdata0, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XG0))

# creating a model with X's and Group 1
proj_model_XG1 <-rpart(XGdata1$target ~ ., data=XGdata1, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XG1))

# getting data of only Y's and Group
YGdata = train_data[c(1, 9:15, 16)]
# spliting data based on Group
YGdata = split(YGdata, YGdata$Group)
YGdata0 = YGdata[[1]]
YGdata1 = YGdata[[2]]

# creating a model with Y's and Group 0
proj_model_YG0 <-rpart(YGdata0$target ~ ., data=YGdata0, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_YG0))

# creating a model with Y's and Group 1
proj_model_YG1 <-rpart(YGdata1$target ~ ., data=YGdata1, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_YG1))

# spliting data based on Group
XYGdata = split(train_data, train_data$Group)
XYGdata0 = XYGdata[[1]]
XYGdata1 = XYGdata[[2]]
# creating a model with XY's and Group 0
proj_model_XYG0 <-rpart(XYGdata0$target ~ ., data=XYGdata0, method='class', 
                        control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XYG0))

# creating a model with XY's and Group 1
proj_model_XYG1 <-rpart(XYGdata1$target ~ ., data=XYGdata1, method='class', 
                        control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XYG1))
