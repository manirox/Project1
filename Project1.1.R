Diabities_data = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",sep = ',',quote = "\"")
#print(housing_data_import1)
head(Diabities_data)
colnames(Diabities_data) = c("NTP","PGC-2","DBP","TSFT","2-HSI","BMI","DPF","AGE","CLASS")
(Diabities_data)
#PickupSample=sample(Diabities_data,replace = TRUE,40)
#(PickupSample)
write.csv(Diabities_data,"/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/Dstaset.csv",row.names = FALSE)
?write.csv
varMis = c(NA,3,4,5,22,NA)

sum(is.na(varMis))
##Code to insert null values in the dta set 







########Code for preprossesing of data 
rm(list=ls())

install.packages("VIM")
require(imputation)
library(VIM)
Diabities_data1= read.csv("/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/Dstaset.csv")
Diabities_data1
?read.csv
sum(is.na(Diabities_data1))
#there are 15 nas in the data set that are introduced by me 
##Count the no of classes in each class
Diabities_data1[,3]
for(i in 1:ncol(Diabities_data1))
{
  print(i)
  #print(Diabities_data1[,i])
  print(length(Diabities_data1[,i]))
}
boxplot(Diabities_data1)
summary(Diabities_data1)
plot(Diabities_data1$NTP,Diabities_data1$CLASS)
plot(Diabities_data1$PGC.2,Diabities_data1$CLASS)
plot(Diabities_data1$DBP,Diabities_data1$CLASS)
plot(Diabities_data1$TSFT,Diabities_data1$CLASS)
plot(Diabities_data1$HSI,Diabities_data1$CLASS)
plot(Diabities_data1$BMI,Diabities_data1$CLASS)
plot(Diabities_data1$DPF,Diabities_data1$CLASS)
plot(Diabities_data1$AGE,Diabities_data1$CLASS)




#######Using Knn Impute ##

library(DMwR)
ImputedDs = knnImputation(Diabities_data1, k = 10)
print(ImputedDs)
#Writing Imputed values into a new csv file 
write.csv(ImputedDs,"/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/ImputedValues1.csv",row.names = FALSE)

CleanResult1=read.csv("/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/ImputedValues1.csv")


#getting Unique values in the data set 
table(CleanResult1$CLASS)

#applying  scale 
scale(CleanResult1, center = TRUE, scale = TRUE)



#DataSubsetting
#Breaking the funciton into 5 fold cross vaidation

SplittingTheData <- function()
  
{
  #sampling the dta
  Datasplit1 <- CleanResult1[sample(nrow(CleanResult1)),]
  print(Datasplit1)
  datafolds1 <- cut(seq(1,nrow(Datasplit1)),breaks = 5,labels = FALSE)
  print(datafolds1)
}
SplittingTheData()
