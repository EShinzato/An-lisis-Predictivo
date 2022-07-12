library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

df=read.csv("smoking.csv")
df$ID=NULL
df$oral=NULL
summary(df)
qweight05=quantile(df$weight.kg.,0.05)
qeyel95=quantile(df$eyesight.left.,0.95)
qeyer95=quantile(df$eyesight.right.,0.95)
df$hearing.left.=as.factor(df$hearing.left.)
df$hearing.right.=as.factor(df$hearing.right.)
qsugar95=quantile(df$fasting.blood.sugar,0.95)
qtrigly95=quantile(df$triglyceride,0.95)
qhdl95=quantile(df$HDL,0.95)
qldl95=quantile(df$LDL,0.95)
df$Urine.protein=as.factor(df$Urine.protein)
qserum95=quantile(df$serum.creatinine,0.95)
qast95=quantile(df$AST,0.95)
qalt95=quantile(df$ALT,0.95)
qgtp95=quantile(df$Gtp,0.95)
df$dental.caries=as.factor(df$dental.caries)
df$tartar=as.factor(df$tartar)
df$smoking=as.factor(df$smoking)
df=df %>% filter(weight.kg.>qweight05, eyesight.left.<qeyel95, eyesight.right.<qeyer95, fasting.blood.sugar<qsugar95, triglyceride<qtrigly95, ALT<qalt95, HDL<qhdl95, LDL<qldl95, serum.creatinine<qserum95, AST<qast95, Gtp<qgtp95)

write.csv(df,"smokers.csv")

set.seed(123);rf=train(smoking~.,df,method="rf",maxit=10000,trControl=trainControl(method="oob",10),tuneGrid=expand.grid(mtry=4))
rf
rf$finalModel #acc=0.86, sens=0.79,esp= 0.9

set.seed(123);nv=train(smoking~.,df,method="naive_bayes",trControl=trainControl(method="cv",10),tuneGrid=expand.grid(laplace=0,usekernel=F,adjust=c(1,.7)))
nv
nv$finalModel #acc=0.72, sens=0.81, esp=0.67

part=createDataPartition(y=df$smoking,p=0.75,list=F)
entreno=df[part,]
testeo=df[-part,]
arbolgrande=rpart(smoking~.,entreno,method="class",minsplit=0,cp=0)
rpart.plot(arbolgrande,extra=1,type=5)
plotcp(arbolgrande)
printcp(arbolgrande)
arbolgrande$cptable[which.min(arbolgrande$cptable[,"xerror"]),"CP"]
arbolpodado=prune(arbolgrande,cp=0.000143020594965675)
rpart.plot(arbolpodado,extra=1,type=5)
pred=predict(arbolpodado,testeo,type="class")
confusionMatrix(pred,testeo$smoking)
arbolpodado$variable.importance
summary(arbolpodado) #acc=0.79, sens=0.66, esp=0.86

set.seed(123);rn=train(smoking~.,df,method="nnet",maxit=10000, trControl = trainControl(method="cv",10),tuneGrid=expand.grid(size=10,decay=.5))
rn
confusionMatrix(rn) #acc=0.77, sens=0.68, esp= 0.82

set.seed(123);svm=train(smoking~.,df,method="svmRadial",maxit=10000, trControl = trainControl(method="cv",10),tuneGrid=expand.grid(sigma=c(.05,.1,.2),C=c(.8,1,1.4)))
svm
confusionMatrix(svm) #acc=0.8, sens=0.64,esp=0.87