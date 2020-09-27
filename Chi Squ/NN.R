library(neuralnet)
library(pROC)
trainDF<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/Chi Squ/Normalized_Dataset_Train80p_top10.csv")
testDF<-read.csv("F:/Thesis/DataMing+MachieLeaning/Diabetes/Chi Squ/Normalized_Dataset_Test20p_top10.csv")

#trainDF$class<-as.factor(trainDF$class)
#testDF$class<-as.factor(testDF$class)


allVars<-colnames(trainDF) 
predictorVars<-allVars[!allVars%in%"class"]
predictorVars<-paste(predictorVars,collapse = "+")
form=as.formula(paste("class~",predictorVars,collapse = "+")) 


#neuralModel<-neuralnet(formula =form,hidden = c(16,8,2),linear.output = T,data =trainDF)
neuralModel<-neuralnet(formula=form, data=trainDF, hidden = c(6,3), threshold = 0.001,
                       stepmax = 1e+05, rep = 1, startweights = NULL,
                       learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,plus = 1.2),
                       learningrate = NULL, lifesign = "none",
                       lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse",
                       act.fct = "logistic", linear.output = TRUE, exclude = NULL,
                       constant.weights = NULL, likelihood = FALSE)

PredictionsWithClass <- compute(neuralModel,testDF[,1:10]) 
PredictionsWithClass<-round(PredictionsWithClass$net.result*(max(testDF$class)-min(testDF$class))+
  min(testDF$class))

cm<-table(predictions=PredictionsWithClass, actual=testDF$class)

acc=((sum(diag(cm))/sum(cm)))
tp<-cm[2,2]
tn<-cm[1,1]
fn<-cm[1,2]
fp<-cm[2,1]

sen=tp/(tp+fn)
spe=tn/(tn+fp)
mcc=((tp*tn) - (fp*fn))/(sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
f1=2*tp/((2*tp)+fp+fn)

roc_obj<-roc(testDF$class,as.numeric(PredictionsWithClass))
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

