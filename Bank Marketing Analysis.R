#Packages
install.packages("tidyverse")
install.packages("backports")
install.packages("DataExplorer")
install.packages("Hmisc")
install.packages("ggthemes")

#Libraries
library(tidyverse)
library(DataExplorer)
library(caret)
library(Hmisc)
library(ggplot2)
library(ggthemes)
library(fastDummies)
library(MASS)


rm(list=ls())

#Loading the data
bank.data <- read.table("Data/bank-full.csv", sep=";", header=T)

#Check for duplicate rows 
sum(duplicated(bank.data))

#Check for missing data
sum(!complete.cases(bank.data))
sum(is.na(bank.data))

#Exploring the data
dim(bank.data)
introduce(bank.data) 
str(bank.data)

#Summary Statistics
describe(bank.data)   

#Plots
plot_bar(bank.data)
x<-filter(bank.data,y=="yes")
ggplot(x,aes(job))+geom_bar(aes(fill=y))
table(bank.data$job,bank.data$y)
ggplot(x,aes(contact))+geom_bar(aes(fill=y))
table(bank.data$contact,bank.data$y)
ggplot(x,aes(age))+geom_bar(aes(fill=y))
ggplot(x,aes(marital))+geom_bar(aes(fill=y))

#Convert character variables to factor variables
bank.data<-bank.data %>% mutate_if(is.character, as.factor) 

#Converting outcome variable to numeric
bank.data$y<-ifelse(bank.data$y=='no',0,1)
str(bank.data)


#Creating dummy variables

dummy_data <- fastDummies::dummy_cols(bank.data, remove_first_dummy = TRUE)
str(dummy_data)  

#Extracting only int columns
nums <- unlist(lapply(dummy_data, is.numeric))

#Final Data
bank.final<- dummy_data[ , nums] 
str(bank.final)

#Converting the dependent variable back to categorical
bank.final$y<-as.factor(bank.final$y)

#Splitting data set into two parts: 70% and 30%
index<-createDataPartition(bank.final$y,p=0.70, list=FALSE)
bank.train<-bank.final[index,]
bank.test<-bank.final[-index,]
dim(bank.train)
dim(bank.test)

#Logistic Regression
glm.bank<- glm(y~.,data=bank.train,family = "binomial")
summary(glm.bank)

glm.probs<-predict(glm.bank,bank.test,type = "response")

confusionMatrix(as.factor(ifelse(glm.probs>0.5,1,0)),as.factor(bank.test$y))

# Tweaking the model to increase efficiency 
confusionMatrix(as.factor(ifelse(glm.probs>0.7,1,0)),as.factor(bank.test$y))

#LDA model
lda.bank<- lda(y~.,data = bank.train)
lda.predict<- predict(lda.bank,bank.test)
confusionMatrix((lda.predict$class ),bank.test$y )

#QDA model
qda.bank<- qda(y~.,data = bank.train)
qda.predict<- predict(qda.bank,bank.test)
confusionMatrix((qda.predict$class ),bank.test$y )





