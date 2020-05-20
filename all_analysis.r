## load all libraries
> library(MASS)
> library(ISLR)
> library(class)
> library(gmodels)
> library(ROCR)

## PART 1
> mba <- read.csv("Admission.csv")
> head(mba)

> set.seed(0820)
> dim(mba)

> std_gpa <- scale(mba$GPA)
> std_gmat <- scale(mba$GMAT)
> std_mba <- data.frame(mba$Decision, std_gpa, std_gmat)

> n = length(std_mba$mba.Decision)
> nt = 70
> train <- sample(1:n, nt)
> test <- std_mba[-train,]
> training <- std_mba[train,]

## LDA
> ldafit <- lda(mba.Decision ~ std_gpa + std_gmat, data = std_mba, subset = train)
> ldafit 

> ldatestpred <- predict(ldafit, test)
> ldatrainpred <- predict(ldafit, training)

> postest <- as.data.frame(round(ldatestpred$posterior), digits = 4)
> postrain <- as.data.frame(round(ldatrainpred$posterior, digits = 4))

> ldatestclass <- ldatestpred$class
> ldatrainclass <- ldatrainpred$class
> table(ldatestclass, test$mba.Decision)
> table(ldatrainclass, training$mba.Decision)

> mean(ldatestclass == test$mba.Decision)

> qdafit <- qda(mba.Decision ~ std_gpa + std_gmat, data = std_mba, subset = train)
> qdafit 

> qdatestpred <- predict(qdafit, test)
> qdatrainpred <- predict(qdafit, training)
> qpostest <- as.data.frame(round(qdatestpred$posterior, digits = 4))
> qpostrain <- as.data.frame(round(qdatrainpred$posterior, digits = 4))

> qdatestclass <- qdatestpred$class
> qdatrainclass <- qdatrainpred$class
> table(qdatestclass, test$mba.Decision)
> table(qdatrainclass, training$mba.Decision)

> mean(qdatestclass == test$mba.Decision)

## PART 2
> set.seed(0820)
> Default$defaultn <- as.numeric(Default$default)-1 
> Default$studentn <- as.numeric(Default$student)-1 
> Default$balancer <- Default$balance/1000 
> Default$incomer <- Default$income/1000

> n = length(Default$defaultn)
> nt = 9000
> dtrain <- sample(1:n, nt)
> dtest <- Default[-dtrain,]
> dtraining <- Default[dtrain,]

## LDA 
> dldafit <- lda(defaultn ~ studentn + balancer, data = Default, subset = dtrain)
> dldafit

> dldatestpred <- predict(dldafit, dtest)
> dldatrainpred <- predict(dldafit, dtraining)

> dprobtest <- as.data.frame(round(dldatestpred$posterior, digits = 4))
> dprobtrain <- as.data.frame(round(dldatrainpred$posterior, digits = 4))

> dprobtestclass <- dldatestpred$class
> dprobtrainclass <-dldatrainpred$class 
> table(dprobtestclass, dtest$defaultn)
> mean(dprobtestclass == dtest$defaultn)

> sensitivity=dim(1000)
> specificity=dim(1000)
> total=dim(1000)
> cutoff=dim(1000) 

> for (k in 1:1000) {
+ p <- k/1000
+ pred <- dprobtrain[,2]
+ predg <- as.numeric(pred >= p)
+ sens <- sum(dtraining$defaultn*predg)/sum(dtraining$defaultn)
+ spec <- 1-sum((1-dtraining$defaultn)*predg)/sum(1-dtraining$defaultn) 
+ sensitivity[k]=sens
+ specificity[k]=spec
+ total[k]=sens+spec
+ cutoff[k]=p
+ }
> evallda <- data.frame(cutoff,total,sensitivity,specificity) 
> evallda[which(evallda$total == max(evallda$total)),]

## QDA 
> dqdafit <- qda(defaultn ~ studentn + balancer, data = Default, subset = dtrain)
> dqdafit

> dqdatestpred <- predict(dqdafit, dtest)
> dqdatrainpred <- predict(dqdafit, dtraining)

> dqprobtest <- as.data.frame(round(dqdatestpred$posterior, digits = 4))
> dqprobtrain <- as.data.frame(round(dqdatrainpred$posterior, digits = 4))

> dqprobtestclass <- dqdatestpred$class
> dqprobtrainclass <-dqdatrainpred$class 
> table(dqprobtestclass, dtest$defaultn)
> mean(dqprobtestclass == dtest$defaultn)

> sensitivity=dim(1000)
> specificity=dim(1000)
> total=dim(1000)
> cutoff=dim(1000) 

> for (k in 1:1000) {
+ p <- k/1000
+ pred <- dqprobtrain[,2]
+ predg <- as.numeric(pred >= p)
+ sens <- sum(dtraining$defaultn*predg)/sum(dtraining$defaultn)
+ spec <- 1-sum((1-dtraining$defaultn)*predg)/sum(1-dtraining$defaultn) 
+ sensitivity[k]=sens
+ specificity[k]=spec
+ total[k]=sens+spec
+ cutoff[k]=p
+ }
> evallda <- data.frame(cutoff,total,sensitivity,specificity) 
> evallda[which(evallda$total == max(evallda$total)),]

## KNN
> attach(Default)
> nearest1 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=1)
> nearest3 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=3)
> nearest5 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=5)
> nearest7 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=7)
> nearest9 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=9)
> nearest11 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=11)
> nearest13 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=13)
> nearest15 <- knn(train=predictors[train,],test=predictors[-train,],cl=defaultn[train],k=15)

> results <- data.frame(Default[-train],nearest1,nearest3,nearest5,nearest7, nearest9, nearest11,nearest13,nearest15)
> results 

> pcorrn1 <- sum(defaultn[-train] == nearest1) / (n-nt)
> pcorrn3 <- sum(defaultn[-train] == nearest3) / (n-nt)
> pcorrn5 <- sum(defaultn[-train] == nearest5) / (n-nt)
> pcorrn7 <- sum(defaultn[-train] == nearest7) / (n-nt)
> pcorrn9 <- sum(defaultn[-train] == nearest9) / (n-nt)
> pcorrn11 <- sum(defaultn[-train] == nearest11) / (n-nt)
> pcorrn13 <- sum(defaultn[-train] == nearest13) / (n-nt)
> pcorrn15 <- sum(defaultn[-train] == nearest15) / (n-nt)
> correct <- data.frame(pcorrn1, pcorrn3, pcorrn5, pcorrn7, pcorrn9, pcorrn11, pcorrn13, pcorrn15)
> correct

> predictors <- data.frame(studentn, balancer)
> pcorr = dim(15)
> for (k in 1:15) {
+ pred = knn.cv(predictors[dtrain,],defaultn[dtrain],k)
+ pcorr[k] = sum(defaultn[dtrain] == pred) / n
+ }
> plot(pcorr, type = "b")

> table(nearest13, defaultn[-train])
> table(nearest15, defaultn[-train])

## LOGISTIC REGRESSION
> logm <- glm(defaultn ~ studentn + balancer, data = Default, subset = dtrain, family = binomial)
> summary(logm)
> drop1(logm, test = "LRT")

> dtest$pred <- predict(logm, dtest, type = "response")
> dtest$pred.c <- ifelse(dtest$pred >= 0.5, c("Yes"), c("No"))
> CrossTable(dtest$defaultn,dtest$pred.c,expected=FALSE, prop.chisq=FALSE, prop.t=FALSE, prop.c=FALSE, chisq=FALSE, format="SPSS")

> Default$pred <- predict(logm, dtest, type = "response")

> pred <- prediction(Default$pred,Default$defaultn)
> perf <- performance(pred,"tpr","fpr") 
> plot(perf, col="red", lty="solid", lwd=2, xaxs="i", yaxs="i", main="ROC- curve")
> abline(a=0,b=1, lwd=2)







