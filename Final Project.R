-----------------------------------------------------------------------------------------------
#SCI 01 A - Introduction do Data Science - Team Heart
#Team Member: Kishore and Vinicius
#Dateset: Heart Disease Data Set (UCI Machine Learning Repository)
#Description: Predict the presence of heart disease in the patients using 14 dataset attributes
-----------------------------------------------------------------------------------------------

#Dataset import----
    
library(utils)
library(caret)

Cleveland = read.csv("processed.cleveland.csv",header=FALSE) #dataset impor
colnames(Cleveland) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Cleveland["num2"] = Cleveland["num"]
Cleveland["num2"][Cleveland["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)

#Dataset visualization----

ggplot(Cleveland,aes(num))+geom_bar() #count of Heart Disease Diagnosis (>0)
ggplot(Cleveland,aes(as.character(num),thalach))+geom_boxplot() 
ggplot(Cleveland,aes(as.character(num),oldpeak))+geom_boxplot() 

#Heart Disease Classification----

#creating training and test sets
training = Cleveland[seq(1,nrow(Cleveland),2),c(seq(1,13,1),15)] 
testing = Cleveland[seq(2,nrow(Cleveland),2),c(seq(1,13,1),15)]

ggplot(testing,aes(num2))+geom_bar() #training set visualization
ggplot(training,aes(num2))+geom_bar() #testing set visualization

#training, testing and measuring accuracy
model = train(as.character(num2) ~.,training,method="rf")
model$results
varImp(model)
predictedNum = predict(model,testing)
accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing)

#Greedy feature selection-------------

#model prepare and baseline run
old = Sys.time() #measure runtime
initialfeature=sample(seq(1,13,1),1) #selecting an initial feature do run model
#initialfeature=8
selectedfeatures=c(initialfeature,15) #define first column to predict column 15
possiblefeatures=seq(1,ncol(Cleveland)-1,1) #define other columns as possible features
training=Cleveland[seq(1,nrow(Cleveland),2),selectedfeatures] #define the inicial training set
testing=Cleveland[seq(2,nrow(Cleveland),2),selectedfeatures] #define the inicial testing set
model = train(as.character(num2) ~.,training,method="rf") #train model
predictedNum = predict(model,testing) #test model
base_accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing) #define base acc
(new = Sys.time() - old) #measure runtime
print(new)
print(base_accuracy)

#features add loop
for (i in 1:length(possiblefeatures)){
  if (i!=14 & i!=initialfeature){ #exclude column "num" and initial feature from loop
    
    training=Cleveland[seq(1,nrow(Cleveland),2),c(selectedfeatures,possiblefeatures[i])] #add new feature training set
    testing=Cleveland[seq(2,nrow(Cleveland),2),c(selectedfeatures,possiblefeatures[i])] #add new feature to testing set
    model = train(as.character(num2) ~.,training,method="rf") #train new model
    predictedNum = predict(model,testing) #test new model
    accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing) #measure new accuracy
    
    if (accuracy>base_accuracy){ #replace base accuracy if new test is better
      selectedfeatures=c(selectedfeatures,possiblefeatures[i])
      base_accuracy=accuracy
    }
  }
  (new = Sys.time() - old) #measure runtime
  print (new)
  print(i)
  print(selectedfeatures)
  print(base_accuracy)
}
