# ================================================================================
# - Decision Tress - Important Terms - with ROC/AUC
# ================================================================================
#For ROC curve and AUROC
library(pROC)

#For train-test split
library(caTools)

#For k fold cross validation
library(caret)


setwd("C:\\Users\\SONY VAIO\\Desktop\\Advanced_Assignment_2")

# read data set
dd = read.csv('public.csv', header=TRUE, stringsAsFactors=TRUE)


#Preprocessing data
##Create AGE variable from DOB
##Age = 0 implies age is unknown
dd$AGE <- 99-dd$DOB

#Remove DOB column
dd$DOB <- NULL

##Convert PHON,NKID,DEP to Categorical
dd$PHON <- as.factor(dd$PHON)
dd$NKID <- as.factor(dd$NKID)
dd$DEP <- as.factor(dd$DEP)

##Remove non-important Variables (based on IV)
# Removing DHVAL DOUTCC DOUTL SINC NKID DOUTHP PHON DEP
dd$DHVAL <- NULL
dd$DOUTCC <- NULL
dd$DOUTL <- NULL
dd$SINC <- NULL
dd$NKID <- NULL
dd$DOUTHP <- NULL
dd$PHON <- NULL
dd$DEP <- NULL

head(dd)

# k-Fold Cross Validation
# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(dd$BAD, SplitRatio = 0.75)
dd_train = subset(dd, split == TRUE)
dd_test = subset(dd, split == FALSE)

##Training set has 75% values - Test set has 25% values

tree_model = rpart(formula = BAD ~ .,data = dd_train, method = "class")
summary(tree_model)
rpart.plot(tree_model)

full_pred = predict(tree_model, type = "class", newdata = dd_test[-6])

pred = prediction(dd_test$BAD, full_pred)

roc = performance(pred, measure="tpr", x.measure="fpr")

plot(roc, col="orange", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')

# Applying k-Fold Cross Validation


folds = createFolds(dd_train$BAD, k = 10)

par(mfrow=c(4,4))


cross_validate = lapply(folds, function(x) {
  
  training_fold = dd_train[-x, ]
  
  test_fold = dd_train[x, ]
  
  tree_model = rpart(formula = BAD ~ .,method = "class",data = training_fold)
  
  ##Removing BAD from test data
  class_pred = predict(tree_model, type = 'class', newdata = test_fold[-6])
  
  conf_matrix = table(test_fold[, 6], class_pred)
  
  #accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  
  #return(accuracy)
  return(conf_matrix)
})


comb_conf_matrix <- cross_validate


accuracy = 0
TotSens = 0
TotSpec = 0
for(folds in comb_conf_matrix){
  accuracy = accuracy + (folds[1]+folds[4])/(folds[1]+folds[2]+folds[3]+folds[4])*100
  TotSens = TotSens + folds[4]/(folds[2]+folds[4])*100
  TotSpec = TotSpec + folds[1]/(folds[3]+folds[1])*100
}

cat("Mean Accuracy :",accuracy/10,"\n Mean Sensitivity :", TotSens/10, "\nMean Specificity :", TotSpec/10)
cat("False Positive Rate :", 100 - TotSpec/10, "\nFalse Negative Rate :", 100 - TotSens/10)

# k-Fold Cross Validation
# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(publicc$BAD, SplitRatio = 0.75)
publicc_train = subset(publicc, split == TRUE)
publicc_test = subset(publicc, split == FALSE)

##Training set has 75% values - Test set has 25% values

tree_model = rpart(formula = BAD ~ .,data = publicc_train, method = "class")
summary(tree_model)
rpart.plot(tree_model)

full_pred = predict(tree_model, type = "class", newdata = publicc_test[-6])

pred = prediction(publicc_test$BAD, full_pred)

roc = performance(pred, measure="tpr", x.measure="fpr")

plot(roc, col="orange", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')

# Applying k-Fold Cross Validation


folds = createFolds(publicc_train$BAD, k = 10)

par(mfrow=c(4,4))


cross_validate = lapply(folds, function(x) {
  
  training_fold = publicc_train[-x, ]
  
  test_fold = publicc_train[x, ]
  
  tree_model = rpart(formula = BAD ~ .,method = "class",data = training_fold)
  
  ##Removing BAD from test data
  class_pred = predict(tree_model, type = 'class', newdata = test_fold[-6])
  
  conf_matrix = table(test_fold[, 6], class_pred)
  
  #accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  
  #return(accuracy)
  return(conf_matrix)
})


comb_conf_matrix <- cross_validate


accuracy = 0
TotSens = 0
TotSpec = 0
for(folds in comb_conf_matrix){
  accuracy = accuracy + (folds[1]+folds[4])/(folds[1]+folds[2]+folds[3]+folds[4])*100
  TotSens = TotSens + folds[4]/(folds[2]+folds[4])*100
  TotSpec = TotSpec + folds[1]/(folds[3]+folds[1])*100
}

cat("Mean Accuracy :",accuracy/10,"\n Mean Sensitivity :", TotSens/10, "\nMean Specificity :", TotSpec/10)
cat("False Positive Rate :", 100 - TotSpec/10, "\nFalse Negative Rate :", 100 - TotSens/10)

