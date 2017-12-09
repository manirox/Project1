Diabities_data = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",sep = ',',quote = "\"")
#print(housing_data_import1)
head(Diabities_data)
colnames(Diabities_data) = c("NTP","PGC-2","DBP","TSFT","2-HSI","BMI","DPF","AGE","CLASS")
(Diabities_data)
#PickupSample=sample(Diabities_data,replace = TRUE,40)
#(PickupSample)
write.csv(Diabities_data,"/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/Dstasettemp1.csv",row.names = FALSE)
?write.csv
varMis = c(NA,3,4,5,22,NA)

sum(is.na(varMis))
##Code to insert null values in the dta set 







########Code for preprossesing of data 
rm(list=ls())

install.packages("VIM")
require(imputation)
library(VIM)
Diabities_data1= read.csv("/Users/manishreddybendhi/Desktop/Fun/Rprogramming/Assigenment4DataScience/Dstasettemp1.csv")
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
table(CleanResult1$NTP)
table(CleanResult1$BMI)
summary(CleanResult1)
#applying  scale 
scale(CleanResult1, center = TRUE, scale = TRUE)







#Using support wector meashines to process the data 




#DataSubsetting

#Using the best cos and gama 
pargrid = expand.grid(gamma=c(.5,1,2,10),cost=10^(-1:3))


#importing th library for svm and using it 

library("e1071")
set.seed(123)
List1=list()
List2=list()
List3=list()



#Breaking the funciton into 5 fold cross vaidation





SplittingTheData <- function()
  
{
  #sampling the dta
  Datasplit1 <- CleanResult1[sample(nrow(CleanResult1)),]
  print(Datasplit1)
  folds <- cut(seq(1,nrow(Datasplit1)),breaks = 5,labels = FALSE)
  print(folds)
  for(j in 1:5)
  {
    
    
    
    testIndex = which(folds==j,arr.ind = TRUE)
    print(testIndex)
    train1 <- Datasplit1[-testIndex, ]
    test1  <- Datasplit1[testIndex, ]
    
    for(i in 1:nrow(pargrid))
    {
      
      
      cost=pargrid[i,2]
      gama=pargrid[i,1]
      
      svm_model_aftertune = svm(CLASS ~. ,data = train1,kernel="radial",cost=cost,gamma=gama)
      
      
      predictFeatures = predict(svm_model_aftertune,train1[,-9])
      
      result= (sum ((predictFeatures-test1 [9])^2))/nrow (train1)
      #result = ((sum((predictFeatures-test1[14])^2))/nrow(train1))
      
      List1[[length(List1)+1]] = result
      List2[[length(List2)+1]] = cost
      List3[[length(List3)+1]] = gama
      #print(List1)
      
      
    }
  }
  CaliculatingMinVal(List1,List2,List3)
  
}
SplittingTheData()





#function that caliculates the minimum value 




List1= NULL
CaliculatingMinVal  <- function(List1,List2,List3)
{
  df <- data.frame(matrix(unlist(List1), nrow = nrow(pargrid), byrow  = FALSE))
  df2 <- data.frame(matrix(unlist(List2), nrow = nrow(pargrid), byrow = FALSE))
  df3 <- data.frame(matrix(unlist(List3), nrow = nrow(pargrid), byrow = FALSE))
  df_combined =data.frame(df2,df3,df)
  # print(df_combined)
  resultmeanval=rowMeans(df_combined[,-1:-2])
  df_resultvalue=data.frame(df_combined,resultmeanval)
  #print(resultmeanval)
  print(df_resultvalue)
  res=min(df_resultvalue[,ncol(df_resultvalue)])
  #df_resultvalue[which.min(df_resultvalue$resultmeanval)]
  print(res)
  result3 =subset(df_resultvalue, resultmeanval == res )
  print(result3)
  print(result3[,1:2])
  print(result3[,2])
  #print(List1)
  print(df_resultvalue[,1])
  print(df_resultvalue[,8])
  #Plotting the graphs 
  #ResultatnGraphs(df_resultvalue)
  
  #hist(ConvCol2,xlab = "Cost",ylab = "Avg_Cv_Error",main = "Graph for the average of cos and avg error",par(mar=c(1,1,1,1)))
}
