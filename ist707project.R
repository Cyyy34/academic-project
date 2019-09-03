train<- read.csv('//Mac/Home/Desktop/SU/2019spring/IST707/project/Bank-trainset-0408.csv')
test <- read.csv('//Mac/Home/Desktop/SU/2019spring/IST707/project/Bank-testset-0408.csv')
test1 <- test[-1,]
ar <- rbind(train,test1)
library(plyr)
library(dplyr)
library(arules)
#install.packages("arulesViz")
library("arulesViz")
detach("package:arulesViz", unload=TRUE)

good <- apriori(ar,parameter = list(supp=0.026,conf=0.01,minlen=2,maxtime=40),appearance = list(rhs="y=yes",default="lhs"))
g_supp <- sort(good,by="support",decreasing = TRUE)
g_supp <- head(g_supp)
inspect(g_supp)
g_conf <- sort(good,by="confidence",decreasing = TRUE)
g_conf <- head(g_conf)
inspect(g_conf)
g_lift <- sort(good,by="lift",decreasing = TRUE)
g_lift <- head(g_lift)
inspect(g_lift)

plot(g_supp, method="graph")

bad <- apriori(ar,parameter = list(supp=0.25,conf=0.7,minlen=2),appearance = list(rhs="y=no",default="lhs"))
b_supp <- sort(bad,by="support",decreasing = TRUE)
b_supp <- head(b_supp)
inspect(b_supp)
b_conf <- sort(bad,by="confidence",decreasing = TRUE)
b_conf <- head(b_conf)
inspect(b_conf)
b_lift <- sort(bad,by="lift",decreasing = TRUE)
b_lift <- head(b_lift)
inspect(b_lift)

arX <- as(ar,"transactions")
inspect(arX)
itemFrequency(arX)
itemFrequencyPlot(arX)

plot(good, method = NULL, measure = "support", shading = "lift", 
     interactive = FALSE, data = NULL, control = NULL)
plot(good,method ='scatterplot',measure =  c('support','confidence'), 
shading = 'lift', max  =  1000)

#evaluation
install.packages("caret")
install.packages("RWeka")
install.packages("RSNNS")
library("RSNNS")
library(RWeka)
library(class)
library(kernlab)
library(caret)

Fact <- test$y

#decision tree
DT_M1 <- J48(y~., data = train, control=Weka_control(U=FALSE, M=2, C=0.5))
DT_PTT1 <- predict(DT_M1, test[,-4])
DT_TPRECISION1 <- precision(data = DT_PTT1, reference = Fact, relevant = "yes")
DT_TRECALL1 <- recall(data = DT_PTT1, reference = Fact, relevant = "yes")
DT_TF1 <- F_meas(data = DT_PTT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
DT_TPRECISION1
DT_TRECALL1
DT_TF1
confusionMatrix(DT_PTT1, Fact)
DT_ACCURACY <-(13294+299)/(13294+1495+284+299)
DT_ACCURACY

#naive bayes
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
NB_M1 <- NB(y~., data=train, control=Weka_control(D=TRUE))
NB_PT1 <- predict (NB_M1, test[,-4])
NB_TPRECISION1 <- precision(data = NB_PT1, reference = Fact, relevant = "yes")
NB_TRECALL1 <- recall(data = NB_PT1, reference = Fact, relevant = "yes")
NB_TF1 <- F_meas(data = NB_PT1, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
NB_TPRECISION1
NB_TRECALL1
NB_TF1
confusionMatrix(NB_PT1, Fact)
DT_ACCURACY <-(12905+471)/(12905+1323+673+471)
DT_ACCURACY

#KNN
library(class)
kNNtrain <- data.frame(lapply(strain, function(x) as.numeric(x)))
KNNtest <- data.frame(lapply(test, function(x) as.numeric(x)))
cl <- kNNtrain$y
Tpred3 <- knn(kNNtrain,cl,k = 3, l = 2, prob = F, use.all = T)
kNN_TPRECISION3 <- precision(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TRECALL3 <- recall(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TF3 <- F_meas(data = factor(Tpred3), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TPRECISION3
kNN_TRECALL3
kNN_TF3
confusionMatrix(factor(Tpred3), factor(as.numeric(Fact)))
KNN3_ACCURACY <-(13360+628)/(13360+628+34+291)
KNN3_ACCURACY

######Precision for k = 5 is 0.9708589, Recall is 0.6519053, F Measure is 0.780037
Tpred5 <- knn(kNNtrain,KNNtest,cl,k = 5, l = 4, prob = F, use.all = T)
kNN_TPRECISION5 <- precision(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TRECALL5 <- recall(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TF5 <- F_meas(data = factor(Tpred5), reference = factor(as.numeric(Fact)), relevant = "2")
kNN_TPRECISION5
kNN_TRECALL5
kNN_TF5
confusionMatrix(factor(Tpred5), factor(as.numeric(Fact)))
KNN5_ACCURACY <-(13146+306)/(13146+306+189+4)
KNN5_ACCURACY

#############
finalFact <- ar$y
KNNfinal <- data.frame(lapply(ar, function(x) as.numeric(x)))
KNN_finalpred <- knn(kNNtrain,KNNfinal,cl,k = 5, l = 4, prob = F, use.all = T)
write.csv(KNN_finalpred, file="//mac/Home/Desktop/SU/2019spring/IST707/knn-pred.csv", row.names=FALSE)
kNN_FPRECISION5 <- precision(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FRECALL5 <- recall(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FF5 <- F_meas(data = factor(KNN_finalpred), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FPRECISION5
kNN_FRECALL5
kNN_FF5
confusionMatrix(factor(KNN_finalpred), factor(as.numeric(finalFact)))

KNNfinal <- data.frame(lapply(ar, function(x) as.numeric(x)))
KNN_finalpred1 <- knn(kNNtrain,KNNfinal,cl,k = 3, l = 2, prob = F, use.all = T)
kNN_FPRECISION3 <- precision(data = factor(KNN_finalpred1), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FRECALL3 <- recall(data = factor(KNN_finalpred1), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FF3 <- F_meas(data = factor(KNN_finalpred1), reference = factor(as.numeric(finalFact)), relevant = "2")
kNN_FPRECISION3
kNN_FRECALL3
kNN_FF3
confusionMatrix(factor(KNN_finalpred1), factor(as.numeric(finalFact)))

#Random forest
library(randomForest)

#Build model(1min)
RF_M500 <- randomForest(y~., data = train, ntree = 500, na.action=na.fail)
RF_M100 <- randomForest(y~., data = train, ntree = 100, na.action=na.fail)
newtrain <- data.frame(y=NA,test[,-4])

#Precision for 500 trees is 0.4922179, Recall is 0.1410256, F Measure is 0.2192374
RF_TP500 <- predict(RF_M500,newtrain)
RF_TPRECISION500 <- precision(data = RF_TP500, reference = Fact, relevant = "yes")
RF_TRECALL500 <- recall(data = RF_TP500, reference = Fact, relevant = "yes")
RF_TF500 <- F_meas(data = RF_TP500, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
confusionMatrix(RF_TP500, Fact)
KNN5_ACCURACY <-(13317+252)/(13317+252+261+1541)
KNN5_ACCURACY#0.8828

#Precision for 100 trees is 0.4841897, Recall is 0.1365663, F Measure is 0.2130435
RF_TP100 <- predict(RF_M100,newtrain)
RF_TPRECISION100 <- precision(data = RF_TP100, reference = Fact, relevant = "yes")
RF_TRECALL100 <- recall(data = RF_TP100, reference = Fact, relevant = "yes")
RF_TF100 <- F_meas(data = RF_TP100, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)
confusionMatrix(RF_TP100, Fact)
KNN5_ACCURACY <-(13317+245)/(13317+245+261+1549)
KNN5_ACCURACY#0.8823

#SVM
library(kernlab)

SVM_M12 <- ksvm(y~., data = train, na.action = na.omit, kernel="besseldot",C=3,cross=10)
SVM_PTT12 <- predict(SVM_M12, test[,-4])

SVM_TPRECISION12 <- precision(data = SVM_PTT12, reference = Fact, relevant = "yes")
SVM_TRECALL12 <- recall(data = SVM_PTT12, reference = Fact, relevant = "yes")
SVM_TF12 <- F_meas(data = SVM_PTT12, reference = Fact, relevant = "yes", beta = 1, na.rm = TRUE)

SVM_TPRECISION12
SVM_TRECALL12
SVM_TF12
confusionMatrix(SVM_PTT12, Fact)

