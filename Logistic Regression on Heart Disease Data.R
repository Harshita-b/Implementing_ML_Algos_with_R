#-------------------------HEART DISEASE DATASET-----------------

#importing the dataset
heart <- read.csv("C:/Users/DELL/OneDrive/Desktop/DATA SCIENCE COURSE (Datasets)/Heart.csv")
View(heart)

#importing the necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(psych)

#running basic functions to explore the dataframe
dim(heart)
head(heart)
tail(heart)
summary(heart)
describe(heart)
str(heart)


#---------------------Data Cleaning-----------------

#Step 1: Replacing null values

#checking all the null values
length(which(is.na(heart)))
lapply(heart,function(x) { length(which(is.na(x)))} )  #the dataframe does not have any null values


#Step 2: Changing the data types of the columns
sapply(heart,n_distinct)   #checking the no of uniques values in every column
h_fac = heart[c(2,3,6,7,9,11,12,13,14)]  #creating a dataframe of columns whose data type is to be changed to factor
View(h_fac)

#using lapply function to change the data type from int to factor
c_names <- names(h_fac)
heart[c_names] <- lapply(heart[c_names],factor)



#------------------------Exploratory Data Analysis---------------------

#--------------boxplot--------------

#boxplot for checking the outliers
#Observations :-
#1. trestbps and chol has some outliers towards higher end
#2. thalach has few outliers towards lower end
boxplot(heart$ï..age,heart$trestbps,heart$chol,heart$thalach,heart$oldpeak,names = c("Age","trestbps","chol","thalach","oldpeak"))


#-------------histogram---------

#histogram to analyse the distribution of Age
#Observations :-
#1. Histogram is negatively skewed
#2. Maximum people are from age group 50-60
#3. People with age group less than 40 and greater than 70 are quite less
hist(heart$ï..age,col = "red",breaks = 10,main = "Distribution of Age")


#histogram to analyse the distribution of trestbps
#Observations :-
#1. Histogram is positively skewed
#2. Maximum people have resting blood pressure in the range 110 - 140
#3. People with resting blood pressure above 160 is quite less
hist(heart$trestbps,col = "yellow",breaks = 10,main = "Distribution of trestbps")


#histogram to analyse the distribution of chol
#Observations :-
#1. Histogram is positively skewed
#2. Maximum people have cholesterol value in range 200 - 300
#3. People with cholesterol value less than 150 and above 350 are quite less
hist(heart$chol,col = "blue",breaks = 10,main = "Distribution of chol")


#histogram to analyse the distribution of thalach
#Observations :-
#1. Histogram is negatively skewed
#2. Maximum people have thalach value in range 140 - 180
#3. People with thalach value less than 100 and above 190 are quite less
hist(heart$thalach,col = "orange",breaks = 10,main = "Distribution of thalach")


#histogram to analyse the distribution of oldpeak
#Observations :-
#1. Histogram is positively skewed
#2. Maximum people have oldpeak value in range 0 - 0.5
#3. People with oldpeak value above 2 are quite less
hist(heart$oldpeak,col = "cyan",breaks = 10,main = "Distribution of oldpeak")


#--------countplot w.r.t Target--------

#countplot for Sex w.r.t Target (1 = male , 0 = female)
#Observations :-
#1. Frequency of male patients is more as compared to females.
#2. More no of male patients are diagnosed with heart disease as compared to female patients
ggplot(heart,aes(x = sex,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("Countplot for Sex w.r.t Target")


#countplot for Chest Pain w.r.t Target 
#Observations :-
#1. Majority of patients with typical angina chest pain(cp = 0) are not diagnosed with heart disease
#2. Majority of patients with atypical,non-anginal and asymptomatic chest pain(cp = 1,2,3) are diagnosed with heart disease
ggplot(heart,aes(x = cp,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Chest Pain") +
  ylab("Count") +
  ggtitle("Countplot for Chest Pain w.r.t Target") 


#countplot for Fasting Blood Sugar w.r.t Target 
#Observations :-
#1. More no of patients with fbs less than 120 mg/dl(fbs = 0) are diagnosed with heart disease
#2. Almost equal no of patients with fbs greater than 120mg/dl(fbs = 1) are diagnosed with heart disease
ggplot(heart,aes(x = fbs,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Fasting Blood Sugar") +
  ylab("Count") +
  ggtitle("Countplot for Fasting Blood Sugar w.r.t Target")


#countplot for restecg w.r.t Target
#Observations :-
#1. More no of patients with restecg value 1 are diagnosed with heart disease as compared to patients with restecg value 0
#2. Patients with restecg value 2 who are diagnosed with heart disease are significantly low
ggplot(heart,aes(x = restecg,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Resting Electrocardiographic Results") +
  ylab("Count") +
  ggtitle("Countplot for Resting Electrocardiographic Results w.r.t Target")


#countplot for Exercise Induced Angina w.r.t Target 
#Observations :-
#1. More no of patients with exercise induced angina are diagnosed with heart disease
#2. Less no of patients with no exercise induced are diagnosed with heart disease
ggplot(heart,aes(x = exang,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Exercise Induced Angina") +
  ylab("Count") +
  ggtitle("Countplot for Exercise Induced Angina w.r.t Target")


#countplot for Exercise Induced Angina w.r.t Target 
#Observations :-
#1. More no of patients with slope value 2 are diagnosed with heart disease
#2. Almost equal no of patients with slope value 0 are diagnosed with heart disease 
#3. Majority of patients with slope value 1 are diagnosed with heart disease
ggplot(heart,aes(x = slope,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Exercise Induced Angina") +
  ylab("Count") +
  ggtitle("Countplot for Exercise Induced Angina w.r.t Target")


#countplot for Major vessels w.r.t Target 
#Observations :-
#1. Majority of patients with major vessels 0 and 4 are diagnosed with heart disease
#2. Majority of patients with major vessels 1,2,3 are not diagnosed with heart disease
ggplot(heart,aes(x = ca,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Mjor Vessels") +
  ylab("Count") +
  ggtitle("Countplot for Major Vessels w.r.t Target")


#countplot for Hemoglobin w.r.t Target 
#Observations :-
#1. Majority of patients with hemoglobin level with fixed defect(thal = 2) are diagnosed with heart disease
ggplot(heart,aes(x = thal,fill = target)) +
  geom_bar(position = 'dodge') +
  xlab("Hemoglobin Level") +
  ylab("Count") +
  ggtitle("Countplot for Hemoglobin Level w.r.t Target")


#--------boxplot w.r.t Target--------

#boxplot for Age w.r.t Target 
#Observations :-
#1. Average age of patients diagnosed with heart disease is less as compared to patients who are not diagnosed with heart disease
#2. This shows that patients with lower age groups are more prone to heart diseases
boxplot(heart$ï..age ~ heart$target, main = "Boxplot for Age w.r.t Target",ylab = "Age",xlab = "Heart Disease")


#boxplot for Resting Blood Pressure w.r.t Target 
#Observations :-
#1. Average trestbps is nearly equal for patients diagnosed with heart disease and patients not diagnosed with heart disease
#2. Maximum and minimum values of trestbps for patients diagnosed with heart disease is less as compared to patients who are not diagnosed by heart disease
#3. Some outliers towards the higher end are detected in the both the boxplots
boxplot(heart$trestbps ~ heart$target, main = "Boxplot for Resting Blood Pressure w.r.t. Target",ylab = "trestbps",xlab = "Heart Disease")


#boxplot for Cholesterol w.r.t Target 
#Observations :-
#1. Average cholestrol level of patients diagnosed with heart disease is less as compared to patients who are not diagnosed by heart disease
#2. Maximum and minimum values of cholestrol level for patients diagnosed with heart disease is nearly same to patients who are not diagnosed by heart disease
#3. More outliers are detected in the boxplot of patients diagnosed with heart disease
boxplot(heart$chol ~ heart$target, main = "Boxplot for cholestrol w.r.t Target",ylab = "chol",xlab = "Heart Disease")


#boxplot for Maximum Heart Rate w.r.t Target 
#Observations :-
#1. Average thalach value of patients diagnosed with heart disease is higher than patients who are not diagnosed by heart disease
#2. More outliers are detected in the boxplot of patients diagnosed with heart disease
boxplot(heart$thalach ~ heart$target, main = "Boxplot for maximum heart rate w.r.t Target",ylab = "thalach",xlab = "Heart Disease")


#boxplot for oldpeak w.r.t Target 
#Observations :-
#1. There is a significant difference in the oldpeak value of patients who are diagnosed with heart disease and patients who are not diagnosed with heart disease
#2. Some outliers towards the higher end are detected in the both the boxplots
boxplot(heart$oldpeak ~ heart$target, main = "Boxplot for oldpeak w.r.t Target",ylab = "oldpeak",xlab = "Heart Disease")


#------------------------Logistic Regression Model---------------------
set.seed(15)
id <- sample(2,nrow(heart),prob = c(0.8,0.2),replace = TRUE)
id 


#dividing the data into training and testing data
#training data = 80% of the data
#testing data = 20% of the data
training <- heart[id == 1,]
testing <- heart[id == 2,]
View(training)
View(testing)


#Model 1:
#taking target as response variable 
#and rest of the variables as independent variable
model1 <- glm(target ~.,training,family = "binomial")
summary(model1)   #value of AIC is 178.07

#printing confusion matrix
res1 <- predict(model1,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res1 > 0.5)


#Model 2:
#taking target as response variable 
#and rest of the variables except chol as independent variable
model2 <- glm(target ~.-chol,training,family = "binomial")
summary(model2)   #value of AIC is 176.76

#printing confusion matrix
res2 <- predict(model2,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res2 > 0.5)


#Model 3:
#taking target as response variable 
#and rest of the variables except thalach and restecg as independent variable
model3 <- glm(target ~.-thalach-restecg,training,family = "binomial")
summary(model3)    #value of AIC is 174.86

#printing confusion matrix
res3 <- predict(model3,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res3 > 0.5)


#Model 4:
#taking target as response variable 
#and rest of the variables except chol,fbs and restecg as independent variable
model4<-glm(target ~.-chol-fbs-restecg,training,family = "binomial")
summary(model4)   #value of AIC is 171.64

#printing confusion matrix
res4 <- predict(model4,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res4 > 0.5)


#AIC value for first 4 models is nearly same


#Model 5:
#taking target as response variable 
#and rest of the variables except restecg,exang,thal and slope as independent variable
model5 <- glm(target ~.-restecg-exang-thal-slope,training,family = "binomial")
summary(model5)   #value of AIC is 184.45

#printing confusion matrix
res5 <- predict(model5,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res5 > 0.5)

#AIC value has increased by a significant value but the confusion matrix is nearly same


#Model 6:
#taking target as response variable 
#and ca and cp as independent variable
model6 <-glm(target ~ ca+cp,training,family = "binomial")
summary(model6)     #value of AIC is 226.02

#printing confusion matrix
res6 <- predict(model6,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res6 > 0.5)

#AIC value has increased by an significant value but the confusion matrix is nearly same


#Model 7:
#taking target as response variable 
#and ca,cp and trestbps as independent variable
model7 <-glm(target ~ ca+cp+trestbps,training,family = "binomial")
summary(model7)    #value of AIC is 220.71

#printing confusion matrix
res7 <- predict(model7,testing,target = "response")
table(ActualValue = testing$target,PredictedValue = res7 > 0.5)

#AIC value for model 6 and 7 is nearly same.

#It can be observed that confusion matrix is nearly same for all the models but there is 
#a significant increase in value of AIC in model 6 and 7
#Model 6 and 7 can be considered as an effective model for dealing with this problem statement
