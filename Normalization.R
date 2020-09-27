rm(list=ls())
library(data.table)
#library(mltools)
data<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/diabetes_data_upload.csv")

for (i in 1:1){
  minimum=min(data[,i])
  maximum=max(data[,i])
  for(j in 1:520){
    x=data[j,i]
    data[j,i]=(x-minimum)/(maximum-minimum)
  }
}

write.csv(data,"F:/Thesis/DataMing+MachieLeaning/Diabetes/Normalized_Data.csv")
