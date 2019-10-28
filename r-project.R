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