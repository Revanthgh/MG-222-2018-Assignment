# ================================================================================
# APPENDIX 11 _ Recursive Partitioning Tress - Binning Weight of Evidence - Plotting ROC+AUC
# ================================================================================
#For ROC curve and AUROC
library(pROC)

#For train-test split
library(caTools)

#For k fold cross validation
library(caret)

#For stepAIC()
library(MASS)

#For rpart
library(rpart)
library(rpart.plot)

#For Decision Tree ROC/AUC Plot
library(ROCR)


#For automatic binning
sapply(c('dplyr', 'car', 'caret', 'e1071', 'knitr', 'reshape2', 'corrplot','rpart', 
         'scales', 'shiny', 'survival', 'gridExtra', 'devtools', 'pec', 'MASS', 'pROC', 
         'manipulate'), 
       install.packages)

sapply(c('dplyr', 'car', 'caret', 'e1071', 'knitr', 'reshape2', 'corrplot','rpart', 
         'scales', 'survival', 'gridExtra', 'devtools', 'pec', 'MASS', 'pROC', 
         'manipulate'), 
       require, character.only = TRUE)

devtools::install_github('JianhuaHuang/streamlineR')
library(streamlineR)



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


##Total Dependents
dd$TDEP <- as.factor(dd$NKID + dd$DEP)

dd$NKID <- NULL
dd$DEP <- NULL

View(dd)

##Binning the Numerical values
##Binning takes place based on Recursive Partitioning Tress - this function checks if there is a split on the
##data if we form a tree based on just BAD and the variable. If a split is formed, bins are created
##according to the splits
lg.bin.age <- bin.rpart(formula = BAD ~ AGE, data = dd, 
                        rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.age)


lg.bin.sinc <- bin.rpart(formula = BAD ~ SINC, data = dd, 
                         rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.sinc)


lg.bin.dainc <- bin.rpart(formula = BAD ~ DAINC, data = dd, 
                          rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.dainc)

lg.bin.dhval <- bin.rpart(formula = BAD ~ DHVAL, data = dd, 
                          rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.dhval)

lg.bin.dmort <- bin.rpart(formula = BAD ~ DMORT, data = dd, 
                          rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.dmort)

lg.bin.doutm <- bin.rpart(formula = BAD ~ DOUTM, data = dd, 
                          rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.doutm)

lg.bin.doutl <- bin.rpart(formula = BAD ~ DOUTL, data = dd, 
                          rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.doutl)

lg.bin.douthp <- bin.rpart(formula = BAD ~ DOUTHP, data = dd, 
                           rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.douthp)

lg.bin.doutcc <- bin.rpart(formula = BAD ~ DOUTCC, data = dd, 
                           rcontrol = rpart.control(minbucket = .01 * nrow(dd)))

str(lg.bin.doutcc)

## We can see that binning has occured only for the following variables :
#  AGE, DAINC, DOUTCC, DOUTM

##Replacing the variables with their corresponding Bins in the Data
dd$AGE <- lg.bin.age$bins
dd$DAINC <- lg.bin.dainc$bins
dd$DOUTM <- lg.bin.doutm$bins
dd$DOUTCC <- lg.bin.doutcc$bins

#Total Number of Dependents = NKID + DEP

View(dd)

##Visualizing the Weight of Evidence for the Categorical Variables - only those with Bins
col.x <- c('AGE', 'DAINC', 'DOUTM', 'DOUTCC', 'PHON', 'AES', 'RES', 'TDEP')

# col.x <- names(dd)
# col.x <- col.x[-14]

stat.train <- level.stat(dd, x = col.x, y = 'BAD', flag.0 = 0, flag.1 = 1)
stat.train

ggstat(data = stat.train, var = "Variable.IV", x = "Group", y = "Rate.1", 
       y.label = "Perc.1", y.label.col = "red", y.title = NULL, 
       bar.col = "cornflowerblue", width = "Rate.group", width.label = "Perc.group", 
       width.label.col = "black", ncol = NULL, theme = "classic", 
       background = "white")

ggstat(stat.train, y = 'WOE', y.label = 'WOE.round', width = .2, 
       width.label = NULL, ncol = 4)


##Replacing categories with their WoE Values
nn_inp_data <- replace.woe(data = dd, stat = stat.train, 
                           replace = TRUE)
View(nn_inp_data)

##Remove those variables that weren't binned
nn_inp_data$SINC <- NULL
nn_inp_data$DHVAL <- NULL
nn_inp_data$DMORT <- NULL
nn_inp_data$DOUTHP <- NULL
nn_inp_data$DOUTL <- NULL

##Removing TDEP as it seems to give Inf IV
nn_inp_data$TDEP <- NULL

View(nn_inp_data)


# k-Fold Cross Validation

# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(nn_inp_data$BAD, SplitRatio = 0.75)
dd_train = subset(nn_inp_data, split == TRUE)
dd_test = subset(nn_inp_data, split == FALSE)

##Training set has 75% values - Test set has 25% values

# Fitting Rpart Tree to the Training set

# Full Training set model and Prediction on Test Model
tree_model = rpart(formula = BAD ~ .,data = dd_train, method = "class")
summary(tree_model)

full_pred = predict(tree_model, type = "class", newdata = dd_test[-7])

pred = prediction(dd_test$BAD, full_pred)

roc = performance(pred, measure="tpr", x.measure="fpr")

confusionMatrix(full_pred, dd_test$BAD)

auc = performance(pred, 'auc')
slot(auc, 'y.values')

# Applying k-Fold Cross Validation


folds = createFolds(dd_train$BAD, k = 10)

par(mfrow=c(4,4))

cross_validate = lapply(folds, function(x) {
  
  training_fold = dd_train[-x, ]
  
  test_fold = dd_train[x, ]
  
  tree_model = rpart(formula = BAD~.,method = "class",data = training_fold)
  
  ##Removing BAD from test data
  class_pred = predict(tree_model, type = "class", newdata = test_fold[-7])
  
  conf_matrix = table(test_fold[, 7], class_pred)
  
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

##Removing TDEP as it seems to give Inf IV
nn_inp_data$TDEP <- NULL

View(nn_inp_data)


# k-Fold Cross Validation

# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(nn_inp_data$BAD, SplitRatio = 0.75)
publicc_train = subset(nn_inp_data, split == TRUE)
publicc_test = subset(nn_inp_data, split == FALSE)

##Training set has 75% values - Test set has 25% values

# Fitting Rpart Tree to the Training set

# Full Training set model and Prediction on Test Model
tree_model = rpart(formula = BAD ~ .,data = publicc_train, method = "class")
summary(tree_model)

full_pred = predict(tree_model, type = "class", newdata = publicc_test[-7])

pred = prediction(publicc_test$BAD, full_pred)

roc = performance(pred, measure="tpr", x.measure="fpr")

confusionMatrix(full_pred, publicc_test$BAD)

auc = performance(pred, 'auc')
slot(auc, 'y.values')

# Applying k-Fold Cross Validation


folds = createFolds(publicc_train$BAD, k = 10)

par(mfrow=c(4,4))

cross_validate = lapply(folds, function(x) {
  
  training_fold = publicc_train[-x, ]
  
  test_fold = publicc_train[x, ]
  
  tree_model = rpart(formula = BAD~.,method = "class",data = training_fold)
  
  ##Removing BAD from test data
  class_pred = predict(tree_model, type = "class", newdata = test_fold[-7])
  
  conf_matrix = table(test_fold[, 7], class_pred)
  
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



