-----------------------------------------------------------------------------------------------
  #SCI 01 A - Introduction do Data Science - Team Heart
  #Team Member: Kishore and Vinicius
  #Dateset: Heart Disease Data Set (UCI Machine Learning Repository)
  #Description: Predict the presence of heart disease in the patients using 14 dataset attributes
-----------------------------------------------------------------------------------------------
  
#Attribute Information:
#-- Only 14 used 
#-- 1. #3  (age)      3 age: age in years       
#-- 2. #4  (sex)      4 sex: sex (1 = male; 0 = female)       
#-- 3. #9  (cp)       9 cp: chest pain type (1=typical angina, 2=atypical angina, 3=non-anginal pain, 4=asymptomatic
#-- 4. #10 (trestbps) 10 trestbps: resting blood pressure (in mm Hg on admission to the hospital)  
#-- 5. #12 (chol)     12 chol: serum cholestoral in mg/dl      
#-- 6. #16 (fbs)      16 fbs: (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)       
#-- 7. #19 (restecg)  19 restecg: resting electrocardiographic results (0,1,2) 
#-- 8. #32 (thalach)  32 thalach: maximum heart rate achieved   
#-- 9. #38 (exang)    38 xang: exercise induced angina (1 = yes; 0 = no)     
#-- 10. #40 (oldpeak) 40 oldpeak = ST depression induced by exercise relative to rest  
#-- 11. #41 (slope)   41 slope: the slope of the peak exercise ST segment (1: upsloping, 2: flat, 3: downsloping)
#-- 12. #44 (ca)      44 ca: number of major vessels (0-3) colored by flourosopy  
#-- 13. #51 (thal)    51 thal: 3 = normal; 6 = fixed defect; 7 = reversable defect  
#-- 14. #58 (num)     (the predicted attribute)

library(utils)
library(caret)
featureselection = 1 # change to 1 to use feature selection

#Step 1: Import Data
Cleveland = read.csv("processed.cleveland.csv",header=FALSE) #cleveland dataset impor
colnames(Cleveland) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Cleveland["num2"] = Cleveland["num"]
Cleveland["num2"][Cleveland["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)
Cleveland[Cleveland=="?"]=NA #change "?" character found for NAs
Cleveland=Cleveland[complete.cases(Cleveland),] #remove NAs
Cleveland = Cleveland[,c(1:13,15)]  #exclude column num

#convert columns that are factors and not numeric
Cleveland$sex = as.factor(as.character(Cleveland$sex))
Cleveland$cp = as.factor(as.character(Cleveland$cp))
Cleveland$fbs = as.factor(as.character(Cleveland$fbs))
Cleveland$restecg = as.factor(as.character(Cleveland$restecg))
Cleveland$exang = as.factor(as.character(Cleveland$exang))
Cleveland$slope = as.factor(as.character(Cleveland$slope))

#Step 2: Feature Selection

old = Sys.time() #measure runtime
initialfeature=sample(seq(1,13,1),1) #selecting an initial feature do run model
#initialfeature=1

selectedfeatures=c(initialfeature,14) #define first column to predict num2
possiblefeatures=seq(1,ncol(Cleveland)-1,1) #define other columns as possible features

training=Cleveland[seq(1,nrow(Cleveland),2),selectedfeatures] #define the inicial training set
testing=Cleveland[seq(2,nrow(Cleveland),2),selectedfeatures] #define the inicial testing set
model = train(as.character(num2) ~.,training,method="rf") #train model

predictedNum = predict(model,testing) #test model
base_accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing) #define base acc

(new = Sys.time() - old) #measure runtime
print(base_accuracy)

#feature selection loop
for (i in 1:length(possiblefeatures)){ #features add loop for all possible features
  if (i!=initialfeature){ #exclude initial feature from loop
    training=Cleveland[seq(1,nrow(Cleveland),2),c(selectedfeatures,possiblefeatures[i])] #define the inicial training set
    testing=Cleveland[seq(2,nrow(Cleveland),2),c(selectedfeatures,possiblefeatures[i])] #define the inicial testing set
    model = train(as.character(num2) ~.,training,method="rf") #train new model
    predictedNum = predict(model,testing) #test new model
    accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing) #measure new accuracy
    
    if (accuracy>base_accuracy){ #replace base accuracy if new test is better
      selectedfeatures=c(selectedfeatures,possiblefeatures[i])
      base_accuracy=accuracy
    }
  }
  (new = Sys.time() - old) #measure runtime
  print(i)
  print(selectedfeatures)
  print(base_accuracy)
}

#Step 3: Heart Disease Classification

if (featureselection==1){ #if not using feature selection, use all features
    Cleveland = Cleveland[,c(selectedfeatures)]
    } 

train_control = trainControl(method="repeatedcv", number = 10, repeats = 3)
m1 = train(as.character(num2)~.,data=Cleveland, trControl=train_control, method="rf")
m2 = train(as.character(num2)~.,data=Cleveland, trControl=train_control, method="LogitBoost")
m3 = train(as.character(num2)~.,data=Cleveland, trControl=train_control, method="kknn")
m4 = train(as.character(num2)~.,data=Cleveland, trControl=train_control, method="nnet")
m5 = train(as.character(num2)~.,data=Cleveland, trControl=train_control, method="svmLinear")

allModels=resamples(list(RandomForest=m1,LogitBoost=m2,KKNN=m3,NeuralNetwork=m4,SVM=m5)) #label the model to compare
bwplot(allModels,scales=list(relation="free"))

#Step 4: Dataset visualization - TBD----
ggplot(Cleveland,aes(num))+geom_bar() #count of Heart Disease Diagnosis (>0)
ggplot(Cleveland,aes(as.character(num),thalach))+geom_boxplot() 
ggplot(Cleveland,aes(as.character(num),oldpeak))+geom_boxplot()
