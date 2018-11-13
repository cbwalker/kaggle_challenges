
#Tree based methods
library(ISLR)
library(tree)
library(MASS)
library(vtreat)
set.seed (1)

#Data
leaderboard<-read.csv('./leaderboard_features.csv')
training_treatment<-designTreatmentsN(training,colnames(training),'ALSFRS_slope')
training_treated <- prepare(training_treatment,training,pruneSig=c(),scale=FALSE)
test_treatment<-designTreatmentsN(test,colnames(test),'ALSFRS_slope')
test_treated <- prepare(test_treatment,test,pruneSig=c(),scale=FALSE)
leaderboard_treatment<-designTreatmentsN(leaderboard,colnames(leaderboard),'subject.id')
leaderboard_treated <- prepare(leaderboard_treatment,leaderboard,pruneSig=c(),scale=FALSE)


#Drop unused variables in the test set
training_shrink<-training_treated[,colnames(training_treated) %in% colnames(test_treated)]
test_shrink<-test_treated[,colnames(test_treated) %in% colnames(training_treated)]
leaderboard_shrink<-leaderboard_treated[,colnames(leaderboard_treated) %in% colnames(training_treated)]
leaderboard_shrink<-leaderboard_shrink[,colnames(leaderboard_shrink) %in% colnames(test_treated)]
training_shrink<-training_shrink[,colnames(training_shrink) %in% colnames(leaderboard_shrink)]
test_shrink<-test_shrink[,colnames(test_shrink) %in% colnames(leaderboard_shrink)]
training_shrink$ALSFRS_slope<-training_treated$ALSFRS_slope
test_shrink$ALSFRS_slope<-test_treated$ALSFRS_slope


training_basic<-as.data.frame(lapply(training, function(x) {x[is.na(x)] <- median(x, na.rm=T);x}))
test_basic<-as.data.frame(lapply(test, function(x) {x[is.na(x)] <- median(x, na.rm=T);x}))

#Tree
tree.model=tree(ALSFRS_slope~.,training_shrink)
plot(tree.model)
text(tree.model, pretty = 0)
sqrt(mean((predict(tree.model, test_shrink) - test_basic$ALSFRS_slope)^2))

#Pruned Tree
#Cross validate to see if pruning improves
cv.tree=cv.tree(tree.model)
plot(cv.tree$size ,cv.tree$dev ,type='b')
#The most complex tree is selected by cross-validation
prune.tree=prune.tree(tree.model ,best=8)
plot(prune.tree)
text(prune.tree ,pretty=0)

#Bagging
library(randomForest)
set.seed(1)
bag.Kaggle=randomForest(ALSFRS_slope~.,data=training_shrink,mtry=1,importance =TRUE)
#Test Error
sqrt(mean((predict(bag.Kaggle, test_shrink) - test_basic$ALSFRS_slope)^2))

#Boosting
library(gbm)
lambda<-seq(0, 0.01, 0.001)
testMSE<-NA
for(i in 1:length(lambda)){
boost.Kaggle<-gbm(ALSFRS_slope~.,data=training_shrink,distribution="gaussian",n.trees=1000, shrinkage = lambda[i])
#Test Error
testMSE[i]<-sqrt(mean((predict(boost.Kaggle, test_shrink, n.trees=1000) - test_basic$ALSFRS_slope)^2))
}
plot(lambda, testMSE, type='l')
lambda_boost<-lambda[which.min(testMSE)] #=0.5998755

boost.Kaggle<-gbm(ALSFRS_slope~.,data=training_shrink,distribution="gaussian",n.trees=1000, shrinkage = lambda_boost)
predict(boost.Kaggle, leaderboard_shrink, n.trees=1000)

#Random Forests
set.seed(1)
testError<-NA
for(i in 1:5){
  rf.Kaggle=randomForest(ALSFRS_slope~.,data=training_shrink,mtry=i,importance =TRUE)
  testError[i]<--sqrt(mean((predict(rf.Kaggle, test_shrink, n.trees=100) - test_basic$ALSFRS_slope)^2))
}
#Take min test error:
which.min(testError)
#Show how testError varies with M. 
plot(1:5, testError, type='l', xlab='M', main='Test Errors as f(M)')

