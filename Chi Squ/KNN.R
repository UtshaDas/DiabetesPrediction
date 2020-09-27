rm(list=ls())
library(caret)
library(pROC)
training<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/Chi Squ/Normalized_Dataset_Train80p_top10.csv")
testing<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/Chi Squ/Normalized_Dataset_Test20p_top10.csv")
training$class<-as.factor(training$class)
testing$class<-as.factor(testing$class)
knnFit <- train(class ~ ., data = training, method = "knn",
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5))


print(knnFit)

predicted=predict(knnFit,newdata=testing[-11])

cm=table(predicted,testing[,11],dnn=c("Prediction","Actual"))
acc=((sum(diag(cm))/sum(cm)))
tp<-cm[2,2]
tn<-cm[1,1]
fn<-cm[1,2]
fp<-cm[2,1]

sen=tp/(tp+fn)
spe=tn/(tn+fp)
mcc=((tp*tn) - (fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
f1=2*tp/((2*tp)+fp+fn)

roc_obj<-roc(testing[,11],as.numeric(predicted))
rocauc<-auc(roc_obj)

print('Accuracy')
print(acc)
print('sensitivity')
print(sen)
print('Specificity')
print(spe)
print('MCC')
print(mcc)
print('F1')
print(f1)
print('AUC')
print(rocauc)

