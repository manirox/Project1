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


#######Using Knn Impute ##


ImputedDs = knnImputation(Diabities_data1, k = 10)
print(ImputedDs)
#Writing Imputed values into a new csv file 
write.csv(ImputedDs,"/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/ImputedValues1.csv",row.names = FALSE)
