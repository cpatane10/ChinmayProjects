library(dplyr)  #enables dply functions
library(pROC) #contains the roc functions to be used
library(tidyr)
churnTrain=read.csv('Desktop/Resume/Repository/BA/Churn_Train.csv')  #reads the original csv file in R
summary(churnTrain) #view summary statistics of churnTrain
churnTrain.unique<-churnTrain[!duplicated(churnTrain),] #Finds and Removes Duplicates
summary(churnTrain.unique) #view summary statistics
hist(churnTrain.unique$number_vmail_messages)
churnTrain.unique$number_vmail_messages[churnTrain.unique$number_vmail_messages < 0] <- NA #replaces negative values in this column with NA
library(quantreg)
library(VIM) #contains the kNN function to be used
colMeans(is.na(churnTrain.unique))
missing_imputed_knn<-kNN(churnTrain.unique,k=4)
colMeans(is.na(missing_imputed_knn))
churnTrain2<-missing_imputed_knn[,c(1:20)] #removes the 20 columns added via kNN
churnTrain2$churn=factor(churnTrain2$churn,levels(churnTrain2$churn)[c(2,1)]) #re-orders the levels
outlier_values_number_customer_service_calls <- boxplot.stats(churnTrain2$number_customer_service_calls)$out  #identifies the outliers for this variable
outlier_values_number_customer_service_calls  #lists the identified outliers for this variable
churnTrain2$number_customer_service_calls[churnTrain2$number_customer_service_calls==outlier_values_number_customer_service_calls]=NA  #replaces outliers for this Variable with "NA"
churnTrain2[!is.na(churnTrain2$number_customer_service_calls),]  #removes those observations containing NAs (formerly outliers) from this variable column
churnTrain2<-churnTrain2[!is.na(churnTrain2$number_customer_service_calls),]
outlier_values_total_intl_calls  <- boxplot.stats(churnTrain2$total_intl_calls)$out  #identifies the outliers for this variable
outlier_values_total_intl_calls  #lists the identified outliers for this variable
churnTrain2$total_intl_calls[churnTrain2$total_intl_calls==outlier_values_total_intl_calls]=NA  #replaces outliers for this Variable with "NA"
churnTrain2[!is.na(churnTrain2$total_intl_calls),] #removes those observations containing NAs (formerly outliers) from this variable column
churnTrain2<-churnTrain2[!is.na(churnTrain2$total_intl_calls),]
outlier_values_total_day_charge  <- boxplot.stats(churnTrain2$total_day_charge)$out  #identifies the outliers for this variable
outlier_values_total_day_charge  #lists the identified outliers for this variable
churnTrain2$total_day_charge[churnTrain2$total_day_charge==outlier_values_total_day_charge]=NA  #replaces outliers for this Variable with "NA"
churnTrain2[!is.na(churnTrain2$total_day_charge),]  #removes those observations containing NAs (formerly outliers) from this variable column
churnTrain2<-churnTrain2[!is.na(churnTrain2$total_day_charge),]
Model=glm(churn~.,family = "binomial",data=churnTrain2) #creates a Logistic Regression to predict Churn using all Variables
summary(Model)  #Shows a Summary of the "Model"
Predicted_Values<-predict(Model, newdata=churnTrain2,type='response')
roc(churnTrain2$churn, Predicted_Values)
plot(roc(churnTrain2$churn, Predicted_Values),col='red',lwd=3)
FinalLeague_Test=read.csv('Desktop/Resume/Repository/BA/FinalLeague_Test.csv') #creates a R dataframe from Excel
predict(Model,FinalLeague_Test,type='response')
