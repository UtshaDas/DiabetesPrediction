rm(list=ls())
data<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/Normalized_Dataset_Train80p.csv")

library(caret)


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="LOOCV")
# run the RFE algorithm
system.time(results <- rfe(data[,1:16], as.factor(data[,17]),sizes =c(1:9), rfeControl=control))
# summarize the results
print(results)
# list the chosen features
rfeRanked = predictors(results)
# plot the results

write.csv(results[["variables"]],"F:/Thesis/DataMing+MachieLeaning/Diabetes/RFE-RF/RFE-RF Results with Ranks.csv")

plot(results, type=c("g", "o"))