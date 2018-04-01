# MG-222-2018-Assignment
Credit Scoring and Its Applications


This assignment is to develop a credit score based on data. The data are from the book “Credit Scoring and Its Applications” by Thomas, Edelman, and Crook. They are available in the attached files public.xls and publicdict.xls. The first file has the data and the second file describes the variables. Data from Excel can be read into R in a variety of ways such as >read.table and >read.csv. 
The variable ‘Bad’ is a binary variable indicating bad credit. The task of credit scoring is to develop a score by which we can predict whether an individual will end up with bad credit. You can welcome to read up the literature on credit scoring if you so choose. 
Use logistic regression and classification trees to build a model that ‘best’ predicts bad credit. To do so, you may need to iteratively build many models, compare and contrast them, and possibly combine models. The following are suggested, but should not limit you. 
Systematic selection of variables, by removing insignificant variables and adding significant variables. 
Consider transformations or combinations of variables. 
Evaluate the goodness-of-fit of models by looking at aspects such as misclassification rates, sensitivity, specificity, ROC curves, AUC, etc. 
Validate models by looking at predictions for out-of-sample data, i.e. by using validation sets. 
Investigate the influence of individual observations on your models by deleting them and seeing the effect on coefficients. 
