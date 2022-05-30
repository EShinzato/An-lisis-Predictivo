library(tidyverse)
library(corrplot)
library(caret)
library(e1071)
library(hydroGOF)
library(caTools)
library(randomForest)
library(univariateML)
#Está un poco desordenado el archivo, perdón! Me olvidé que lo tenía que entregar

df=read.csv("base_train.csv")
val=read.csv("base_val.csv")

colnames(df)
summary(df) #No hay NAs
df$X=NULL #Elimino id, no aporta info
hist(df$price)
summary(df$price)
df=df %>% filter(price>0)
hist(log(df$price))
ks.test(log(df$price),'pnorm')
hist(df$price,
     main = "Distribución precio",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mllnorm(df$price), lwd = 2, lty = 1, col = "blue")
lines(mlinvgauss(df$price), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("lnorm", "invgauss"),
       col = c("blue", "red"), lty = 1:2)
q95=quantile(df$price,0.95)
q05=quantile(df$price,0.05)
df=df %>% filter(price<q95,price>q05)
hist(df$symboling)
df %>% group_by(symboling) %>% summarise(n=n())
df$symboling=as.factor(df$symboling)
val$symboling=as.factor(val$symboling)
hist(df$engine.size)
q95=quantile(df$engine.size,0.95)
q05=quantile(df$engine.size,0.05)
df=df %>% filter(engine.size<q95,engine.size>q05)
hist(df$compression.ratio)
q95=quantile(df$compression.ratio,0.95)
q05=quantile(df$compression.ratio,0.05)
df=df %>% filter(compression.ratio<q95,compression.ratio>q05)
hist(df$peak.rpm)
M = cor(df, method = "spearman")
corrplot(M, method = "ellipse",title="Correlación")
View(df)
set.seed(4);particion=createDataPartition(y=df$price,p=0.025,list=F)
muestra=df[particion,]
set.seed(4);modelo=train(price~.,muestra,method="nnet",maxit=10000,MaxNWts=100000, trControl = trainControl(method="cv",5),tuneGrid=expand.grid(size=c(1500,1000,700),decay=0))
modelo
names(modelo)

testeo=df[-particion,]
set.seed(4);particion=createDataPartition(y=testeo$price,p=0.025,list=F)
testeo=testeo[particion,]

svm=svm(price~.,muestra,kernel="radial",scale=T,gamma=1/13, cost=1.20)
pred=predict(svm,testeo)
rmse(pred,testeo$price)
svm=svm(price~.,muestra,kernel="polynomial",scale=T,gamma=1/11, cost=1.29,degree=4) #0.2442 (3)
pred=predict(svm,testeo)
rmse(pred,testeo$price)
svm=svm(price~.,muestra,kernel="linear",scale=T,gamma=1/11, cost=1.29) #0.2471
pred=predict(svm,testeo)
rmse(pred,testeo$price)

pred=predict(svm,val[-1])
submit=cbind(val$X,pred)
colnames(submit) = c('id','price')
write.csv(submit,"submit11.csv",row.names = F)

set.seed(4);rforest=randomForest(x=muestra[-16],y=muestra$price,ntree=1000)
plot(rforest)
importance(rforest)
pred=predict(rforest,val)
submit=cbind(val$X,pred)
colnames(submit) = c('id','price')
write.csv(submit,"submit7.csv",row.names = F)


set.seed(4);km=kmeans(df[16],3)#meter outliers
km$centers
clusters=data.frame(df,km$cluster)
clusters %>% group_by(km.cluster) %>% summarise(n=n())

set.seed(4);particion=createDataPartition(y=clusters$km$cluster,p=0.025,list=F)
m2=clusters[particion,]
t2=clusters[-particion,]
set.seed(4);particion=createDataPartition(y=t2$km.cluster,p=0.025,list=F)
t2=t2[particion,]

svm=svm(km.cluster~.,m2,kernel="radial",scale=T,gamma=1/22, cost=1.13) #0.2310 (13)
pred=predict(svm,t2,type="class")
confusionMatrix(pred,t2$km.cluster)
svm=svm(km.cluster~.,m2,kernel="polynomial",scale=T,gamma=1/11, cost=1.29,degree=3) #0.2442 (3)
pred=predict(svm,t2,type="class")
svm=svm(km.cluster~.,m2,kernel="linear",scale=T,gamma=1/11, cost=1.29) #0.2471
pred=predict(svm,t2,type="class")

c1=clusters %>% filter(km.cluster==1)
c2=clusters %>% filter(km.cluster==2)
c3=clusters %>% filter(km.cluster==3)
s1 = c1[sample(nrow(c1),10000),]
s2 = c2[sample(nrow(c2),10000),]
s3 = c3[sample(nrow(c3),10000),]
balanced=rbind(s1,s2,s3)
balanced$km.cluster=NULL
set.seed(4);rforest=randomForest(x=muestra[-16],y=muestra$price,ntree=500,mtry=6) #(6=2843)
pred=predict(rforest,testeo)
rmse(pred,testeo$price)
plot(rforest)
importance(rforest)
pred=predict(rforest,val[-1])
submit=cbind(val$X,pred)
colnames(submit) = c('id','price')
write.csv(submit,"submit12.csv",row.names = F)
hist(balanced$price)
