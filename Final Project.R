#-----------------------------------------------------------------------------------------------
#SCI 01 A - Introduction do Data Science - Team Heart
#Team Member: Kishore and Vinicius
#Dateset: Heart Disease Data Set (UCI Machine Learning Repository)
#Description: Predict the presence of heart disease in the patients using 14 dataset attributes
#-----------------------------------------------------------------------------------------------
#Attribute Information:
#-- Only 14 used 
#-- 1. #3  (age)      3 age: age in years       
#-- 2. #4  (sex)      4 sex: sex (1 = male; 0 = female)       
#-- 3. #9  (cp)       9 cp: chest pain type (1=typical angina, 2=atypical angina, 3=non-anginal pain, 4=asymptomatic)
#-- 4. #10 (trestbps) 10 trestbps: resting blood pressure (in mm Hg on admission to the hospital)  
#-- 5. #12 (chol)     12 chol: serum cholestoral in mg/dl      
#-- 6. #16 (fbs)      16 fbs: (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)       
#-- 7. #19 (restecg)  19 restecg: resting electrocardiographic results (0,1,2) 
#-- 8. #32 (thalach)  32 thalach: maximum heart rate achieved   
#-- 9. #38 (exang)    38 xang: exercise induced angina (1 = yes; 0 = no)
#-- 10. #40 (oldpeak) 40 oldpeak = ST depression induced by exercise relative to rest
#-- 11. #41 (slope)   41 slope: the slope of the peak exercise ST segment (1: upsloping, 2: flat, 3: downsloping)
#-- 12. #44 (ca)      44 ca: number of major vessels (0-3) colored by fluoroscopy
#-- 13. #51 (thal)    51 thal: 3 = normal; 6 = fixed defect; 7 = reversable defect  
#-- 14. #58 (num)     (the predicted attribute)

#Step 1: Import data----
library(utils)
library(caret)
library(corrplot)

Cleveland = read.csv("processed.cleveland.csv",header=FALSE) #cleveland dataset impor
colnames(Cleveland) = c("age","sex","cp","trestbps","chol","fbs",
                        "restecg","thalach","exang","oldpeak","slope","ca","thal","num") #rename columns
Cleveland["num"][Cleveland["num"]>0] = 1

#convert columns that are factors and not numeric
Cleveland$sex = as.factor(as.character(Cleveland$sex))
Cleveland$cp = as.factor(as.character(Cleveland$cp))
Cleveland$fbs = as.factor(as.character(Cleveland$fbs))
Cleveland$restecg = as.factor(as.character(Cleveland$restecg))
Cleveland$exang = as.factor(as.character(Cleveland$exang))
Cleveland$slope = as.factor(as.character(Cleveland$slope))
Cleveland$num = as.factor(as.character(Cleveland$num))

Cleveland[Cleveland=="?"]=NA #change "?" character found for NAs
Cleveland=Cleveland[complete.cases(Cleveland),] #remove NAs

#create a table with numeric variables (will be used for correlogram visualization)
Correlogram = Cleveland[,c(1,4,5,8,10)]

#Step 2: Best model Selection----

train_control = trainControl(method="repeatedcv", number = 10, repeats = 3)
m1 = train(as.character(num)~.,data=Cleveland, trControl=train_control, method="rf")
m2 = train(as.character(num)~.,data=Cleveland, trControl=train_control, method="LogitBoost")
m3 = train(as.character(num)~.,data=Cleveland, trControl=train_control, method="kknn")
m4 = train(as.character(num)~.,data=Cleveland, trControl=train_control, method="nnet") #added this model
m5 = train(as.character(num)~.,data=Cleveland, trControl=train_control, method="svmLinear") #added this model

allModels=resamples(list(RandomForest=m1,LogitBoost=m2,KKNN=m3,NeuralNetwork=m4,SVM=m5)) #label the model to compare
bwplot(allModels,scales=list(relation="free"))

m1$results #show model results
predictors(m1) #show selected features by rf model
varImp(m1) #show importance of each variable

#Step 3: Dataset visualization----

#Renaming factor levels for plots
levels(Cleveland$sex) = c("women","men","")
levels(Cleveland$cp) = c("typical angina","atypical angina","non-anginal pain","asymptomatic")
levels(Cleveland$sex) = c("women","men","")
levels(Cleveland$num) = c("No disease","Disease")
levels(Cleveland$thal) = c("","normal","fixed defect","reversable defect")

#Correlogram between numeric variables - to investigate if there is correlation between numeric variables
M = cor(Correlogram)
corrplot(M, method = "circle", type = "upper", order = "hclust")

#Features importance visualization
plot_varImp = varImp(m1, scale = FALSE)
ggplot(plot_varImp, top = 10)+theme_bw()+ggtitle("Features relevence - Top 10")+theme(plot.title = element_text(hjust = 0.5))

#Disease vs features visualization

ggplot(Cleveland,aes(num,age))+geom_boxplot()+theme_bw()+xlab("Heart Disease")+
  ggtitle("Heart Disease by Age")+theme(plot.title = element_text(hjust = 0.5))

ggplot(Cleveland,aes(num,thalach))+geom_boxplot()+theme_bw()+xlab("Heart Disease")+
  ylab("Maximum heart rate achieved")+ggtitle("Heart Disease by Maximum heart rate achieved")+
  theme(plot.title = element_text(hjust = 0.5))

mosaicplot(Cleveland$sex ~ Cleveland$num, main = "Heart Disease by Gender", 
           xlab = "Gender", ylab = "Heart disease", color = TRUE, shade = FALSE)

mosaicplot(Cleveland$ca ~ Cleveland$num, main = "Heart Disease by number of major vessels colored by fluoroscopy",
           xlab = "number of major vessels colored by fluoroscopy", ylab = "Heart disease", 
           color = TRUE, shade = FALSE)