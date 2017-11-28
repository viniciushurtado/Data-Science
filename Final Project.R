-----------------------------------------------------------------------------------------------
#SCI 01 A - Introduction do Data Science - Team Heart
#Team Member: Kishore and Vinicius
#Dateset: Heart Disease Data Set (UCI Machine Learning Repository)
#Description: Predict the presence of heart disease in the patients using 14 dataset attributes
-----------------------------------------------------------------------------------------------

#run lines first if want to change the seletion  
library(utils)
library(caret)
allcities = 0 #cleveland = 0 or All_Citiies = 1 ?
featureseletion = 1 #use greety feature selection?
  
####0.Attribute Information:----
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
  
####1.Dataset import----
#cleveland import
Cleveland = read.csv("processed.cleveland.csv",header=FALSE) #dataset impor
colnames(Cleveland) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Cleveland["num2"] = Cleveland["num"]
Cleveland["num2"][Cleveland["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)

#Hungarian import
Hungarian = read.csv("processed.hungarian.csv",header=FALSE) #dataset impor
colnames(Hungarian) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Hungarian["num2"] = Hungarian["num"]
Hungarian["num2"][Hungarian["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)

#Switzerland import
Switzerland = read.csv("processed.switzerland.csv",header=FALSE) #dataset impor
colnames(Switzerland) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Switzerland["num2"] = Switzerland["num"]
Switzerland["num2"][Switzerland["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)

#VA import
VA = read.csv("processed.va.csv",header=FALSE) #dataset impor
colnames(VA) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
VA["num2"] = VA["num"]
VA["num2"][VA["num2"]>0] = 1 #create a new column with 0 (w/o heart disease) and 1 (w/ heart disease)

#join 4 table + removing missing columns + remove NAs

All_Cities = rbind(Cleveland,Hungarian,Switzerland,VA)
All_Cities = All_Cities[,c(1:10,14:15)]
All_Cities[All_Cities=="?"]=NA
All_Cities=All_Cities[complete.cases(All_Cities),] #remove NAs

####2.Greedy feature selection using rf (0 or 1)-------------
#model prepare and baseline run
if (allcities==0){
    train_data = Cleveland[,c(1:13,15)]  
    initialfeature=sample(seq(1,13,1),1) #selecting an initial feature do run model
    #initialfeature=8
    selectedfeatures=c(initialfeature,14) #define first column to predict column 15
    } else {
    train_data = All_Cities[,c(1:13,15)]  
    initialfeature=sample(seq(1,10,1),1) #selecting an initial feature do run model
    #initialfeature=8
    selectedfeatures=c(initialfeature,12) #define first column to predict column 15
    } 
old = Sys.time() #measure runtime
possiblefeatures=seq(1,ncol(train_data)-1,1) #define other columns as possible features
training=train_data[seq(1,nrow(train_data),2),selectedfeatures] #define the inicial training set
testing=train_data[seq(2,nrow(train_data),2),selectedfeatures] #define the inicial testing set
model = train(as.character(num2) ~.,training,method="rf") #train model
predictedNum = predict(model,testing) #test model
base_accuracy = sum(predictedNum==as.character(testing$num2))/nrow(testing) #define base acc
(new = Sys.time() - old) #measure runtime
print(new)
print(base_accuracy)

#features add loop
for (i in 1:length(possiblefeatures)){
  if (i!=14 & i!=initialfeature){ #exclude column "num" and initial feature from loop
    training=train_data[seq(1,nrow(train_data),2),selectedfeatures] #define the inicial training set
    testing=train_data[seq(2,nrow(train_data),2),selectedfeatures] #define the inicial testing set
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

####3.Heart Disease Classification (0 or 1)----
#step 1 - Select the data
if (allcities==0){
  if (featureseletion==0){
    train_data = Cleveland[,c(1:13,15)]  
  } else {
    train_data = Cleveland[,c(selectedfeatures,15)]
  } 
} 

if (allcities==1){
  if (featureseletion==0){
    train_data = All_Cities[,c(1:13,15)]  
  } else {
    train_data = All_Cities[,c(selectedfeatures,15)]
  } 
} 
#Step 2 - train the data
train_control = trainControl(method="repeatedcv", number = 10, repeats = 3)
m1 = train(as.character(num2)~.,data=train_data, trControl=train_control, method="rf")
m2 = train(as.character(num2)~.,data=train_data, trControl=train_control, method="LogitBoost")
m3 = train(as.character(num2)~.,data=train_data, trControl=train_control, method="kknn")

#Step 3 - compare the models
allModels=resamples(list(RandomForest=m1,LogitBoost=m2,KKNN=m3)) #label the model to compate
bwplot(allModels,scales=list(relation="free"))

####Dataset visualization - TBD----

ggplot(Cleveland,aes(num))+geom_bar() #count of Heart Disease Diagnosis (>0)
ggplot(Cleveland,aes(as.character(num),thalach))+geom_boxplot() 
ggplot(Cleveland,aes(as.character(num),oldpeak))+geom_boxplot()

