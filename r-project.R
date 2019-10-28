library(rpart)
library(partykit)
library(readxl)

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

# creating a model where all the data (.) is used and 'Response' is the the Target variable (~)
# method='class' is used for a classification tree
# model includes all the X's, Y's and Group
proj_model_resp <-rpart(Response ~ ., data=proj_data, method='class', 
                        control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
# ploting the DT with all the data
plot(as.party(proj_model_resp))

# converting the 'Response' into a categorical value 'target'
target = ifelse(Response==1,'Y','N')

proj_data <- data.frame(proj_data, target)
# removing 'Response' from the data
proj_data <- proj_data[,-1]

# creating the same model with the Target variable as the categorical value
# model includes all the X's, Y's and Group
proj_model_tar <-rpart(target ~ ., data=proj_data, method='class', 
                       control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_tar))

# getting only X cols
Xdata = proj_data[c(2:8)]
# creating a model with only X's
proj_model_X <-rpart(target ~ ., data=Xdata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_X))

# getting only Y cols
Ydata = proj_data[c(9:15)]
# creating a model with only Y's
proj_model_Y <-rpart(target ~ ., data=Ydata, method='class', 
                     control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_Y))

# creating a model with X's and Y's without Group
XYdata = proj_data[c(2:14)]
proj_model_XY <-rpart(target ~ ., data=XYdata, method='class', 
                      control=rpart.control(minsplit=30, minbucket=15, maxdepth=8 ))
plot(as.party(proj_model_XY))

# getting data of only X's and Group
XGdata = proj_data[c(1:8, 16)]
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
