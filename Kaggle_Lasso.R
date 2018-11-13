
##Initial data analysis

##Check why we drop the intercept from model.matrix!


#Look at training data
training_target<-read.csv('./training_target.csv')
validation_target<-read.csv('./validation_target.csv')
training_features<-read.csv('./training_features.csv')
validation_features<-read.csv('./validation_features.csv')

#Merge data
training<-inner_join(training_target, training_features, by='subject.id')
test<-inner_join(validation_target, validation_features, by='subject.id')

#Run VTreat Package to clean the data
#this highlights potential proble values due to missingness
training_treatment<-designTreatmentsN(training,colnames(training),'ALSFRS_slope',TRUE)
training_treated <- prepare(training_treatment,training,pruneSig=c(),scale=FALSE)
test_treatment<-designTreatmentsN(test,colnames(test),'ALSFRS_slope',TRUE)
test_treated <- prepare(test_treatment,test,pruneSig=c(),scale=FALSE)
#Drop unused variables in the test set
training_shrink<-training_treated[,colnames(training_treated) %in% colnames(test_treated)]
test_shrink<-test_treated[,colnames(test_treated) %in% colnames(training_treated)]
# all input variables should be mean 0
#varsN <- setdiff(colnames(training_treated),'')
#sapply(training_treated[,varsN,drop=FALSE],mean) 

#Instead run basic data clean
training_basic<-as.data.frame(lapply(training, function(x) {x[is.na(x)] <- median(x, na.rm=T);x}))
test_basic<-as.data.frame(lapply(test, function(x) {x[is.na(x)] <- median(x, na.rm=T);x}))

#Choose model

#Fit data to training set
#Predict on test set saving the MSE


#Ridge / Lasso
#Set up data
x=model.matrix(ALSFRS_slope~.,training_shrink)[,-1]
y=training_shrink$ALSFRS_slope
x_test=model.matrix(ALSFRS_slope~.,test_shrink)[,-1]
y_test=test_shrink$ALSFRS_slope

###Ridge Regression
library(glmnet)
#Cross validate the training set to extract lambda
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#Fit the model
ridge.mod=glmnet(x,y,alpha=0,lambda=bestlam)

#Use the CVd lambda on the test set. 
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x_test)
ridge.pred_train=predict(ridge.mod,s=bestlam ,newx=x)

#Calculate Training MSE
mean((training_shrink$ALSFRS_slope - ridge.pred_train)^2)

#Calculate Test MSE
mean((test_shrink$ALSFRS_slope - ridge.pred)^2)


##Lasso
#Cross validate the training set to extract lambda
cv.out_lasso=cv.glmnet(x,y,alpha=1)
plot(cv.out_lasso)
bestlam_lasso=cv.out_lasso$lambda.min
bestlam_lasso

#Fit the model
lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam_lasso)
coef(lasso.mod)
#Use the CVd lambda on the test set. 
lasso.pred=predict(lasso.mod,s=bestlam_lasso ,newx=x_test)
lasso.pred_train=predict(lasso.mod,s=bestlam_lasso ,newx=x)

#Calculate Training MSE
sqrt(mean((training_shrink$ALSFRS_slope - lasso.pred_train)^2))

#Calculate Test MSE
sqrt(mean((test_shrink$ALSFRS_slope - lasso.pred)^2))





