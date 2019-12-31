### SVM Assignment--Hand Written Digit Recognition
#### Required packages for this Assignment
# caret,kernlab,dplyr,readr,ggplot2,gridExtra
###### Loading the packages
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
#######################################################################################################
############ Load the train,test data after extracting from rar file ##################################
#######################################################################################################
# read_csv() is a function in library of readr and is used for quick import of huge size data
#Here in the csv file we don't have any column names assigned 
#so we are setting col_names =F while importing the data
# header=F will work in read.csv() but in read_csv() header=F will not be worked.

mnist_train <- read_csv("mnist_train.csv",col_names = FALSE)
# we are setting the first column which has digits as label
names(mnist_train)[1]<-"label" 
dim(mnist_train)#60000 rows and 785 columns

mnist_test <-read_csv('mnist_test.csv',col_names = FALSE)
names(mnist_test)[1]<-"label"
dim(mnist_test)#10000 rows and 785 columns

# Combining the train and test data and storing it as combined
combined <-rbind(mnist_train,mnist_test)
dim(combined) #70000 rows and 785 columns

######################################################################################################
############################### Data understanding####################################################
######################################################################################################

# Some information about MNIST dataset
# The MNIST dataset consists  of handwritten digits with a training set of 60,000 examples, 
# and a test set of 10,000 examples.
# The 784 columns apart from the label consist of  28*28 matrix describing the scanned image of the digits
# The digits have been size-normalized and centered in a fixed-size image

str(mnist_train) # variables integers,60000 observations, 785 variables
str(mnist_test) # variables are integers, 10000 observations, 785 variables
str(combined) # variables are integers, 70000 observations,785 variables

summary(mnist_train) 
summary(mnist_test)
summary(combined)

#######################################################################################################
################################## Data Cleaning and Data Preparation #################################
#######################################################################################################
# Checking for incomplete cases,missing values in rows
sum(!complete.cases(mnist_train)) #0(No missing values)
sum(!complete.cases(mnist_test)) #0 (No missing values)
sum(!complete.cases(combined)) #0 (No missing values)

# Checking for NAs in train,test
sum(sapply(mnist_test, function(x) sum(is.na(x)))) #0--- no NAs
sum(sapply(mnist_train, function(x) sum(is.na(x)))) #0--- no NAs
sum(sapply(combined, function(x) sum(is.na(x)))) #0--- no NAs

# check for duplicates
sum(duplicated(mnist_test)) #0--- no duplicate rows
sum(duplicated(mnist_train)) #0--- no duplicate rows
sum(duplicated(combined)) #0---- no duplicated rows

# The given data is already cleaned and we need not do any further cleaning.
# we can't remove the columns completely filled with 0 because the data is image of 28*28=784 pixels
# so if we remove any column it means we are removing pixels which results in loss of information

# Factorising label column
mnist_train$label <-as.factor(mnist_train$label)
mnist_test$label <-as.factor(mnist_test$label)
combined$label <-as.factor(combined$label)

max(mnist_train[ ,2:ncol(mnist_train)]) # max pixel value is 255
min(mnist_train[ ,2:ncol(mnist_train)]) # min pixel value is 0
max(mnist_test[ ,2:ncol(mnist_test)]) # max pixel value is 255
min(mnist_test[ ,2:ncol(mnist_test)]) # min pixel value is 0

########################### Scaling of data
### Reason to do scaling:we can see that the range of value for pixel is 0 to 255
# But there are few columns filled with values around 100 
# SVM tries to maximize the distance between the hyperplane and support vectors.
# If any one feature has very large value then it will dominate other features while calculating distance
# By scaling all features have same influence on the distance metric.

# so let's do scaling for all columns except label column

################### scaling pixels such that maximum value is 1 and minimum value is 0
# Formula for scaling is ((x-min)/(max-min)) here min=0,max=255,x is the value in a column
# so here it is (x-0)/(255-0)=x/255
# Therefore for scaling we just need to divide columns by 255
mnist_train[ , 2:ncol(mnist_train)] <- mnist_train[ , 2:ncol(mnist_train)]/255
max(mnist_train[ ,2:ncol(mnist_train)]) # max pixel value is 1
min(mnist_train[ ,2:ncol(mnist_test)]) # min pixel value is 0


mnist_test[ , 2:ncol(mnist_test)] <- mnist_test[ , 2:ncol(mnist_test)]/255
max(mnist_test[ ,2:ncol(mnist_test)]) # max pixel value is 1
min(mnist_test[ ,2:ncol(mnist_test)]) # min pixel value is 0

######################################################################################################
#################################### Sampling train dataset###########################################
######################################################################################################
# we can clearly see that for analysing the complete data it takes more time.
# so we can take samples and perform analysis,model building
# Taking 15% of mnist_train data (15% of 60000 = 9000)

set.seed(100) # to reproduce the same data we set seed
sample_indices <- sample(1: nrow(mnist_train), 9000) # extracting subset of 9000 samples(15% of 60000)
train <- mnist_train[sample_indices, ]
dim(train) #9000 rows and 785 columns

######################################################################################################
################################### Visualisation of data#############################################
######################################################################################################
# Let's plot scaled mnist_train data and see it's distribution
plot1 <- ggplot(mnist_train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar(fill="orange") + 
  theme_light() +labs(y = "Frequency (%)", title = "Train dataset") +
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))+
  theme(plot.title = element_text(hjust = 0.5))
plot1

# Let's plot scaled mnist_test data and see it's distribution
plot2 <- ggplot(mnist_test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar(fill="blue") + 
  theme_light() +labs(y = "Frequency (%)", title = "Test dataset") +
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))+
  theme(plot.title = element_text(hjust = 0.5))
plot2

# Let's plot combined data which isn't scaled and see the distribution of digits.
plot3 <- ggplot(combined, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar(fill="green") + 
  theme_light() +labs(y = "Frequency (%)", title = "combined dataset") +
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))+
  theme(plot.title = element_text(hjust = 0.5))
plot3

grid.arrange(plot1, plot2, plot3,nrow =3)

# Let's see the distribution of digits for the sampled mnist train dataset
plot4 <- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar(fill="violet") + 
  theme_light() +labs(y = "Frequency (%)", title = "sampled train dataset") +
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))+
  theme(plot.title = element_text(hjust = 0.5))
plot4

grid.arrange(plot1,plot4,nrow=2)

grid.arrange(plot1,plot2,plot4,nrow=3)

# We can see that sampled train dataset has good relative frequency of digits just like the original dataset

#######################################################################################################
############################# Model Building and Model Evaluation######################################
#######################################################################################################
# Let's start with the simple Linear model
############################ Linear model - SVM  at Cost(C) = 1
model1 <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 1)
print(model1) # Training error=0.001778
# Valdiating the model on test data
eval1 <- predict(model1,mnist_test, type = "response")
confusionMatrix(eval1,mnist_test$label)

# Accuracy on test data is 92.03%
# Specificity > 98% 
# Sensitivity  > 84%

################################## Linear kernel using higher value of C (C=10)

model2 <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
print(model2) # Training error=0 #overfit
# Valdiating the model on test data
eval2 <- predict(model2,mnist_test, type = "response")
confusionMatrix(eval2,mnist_test$label) 

# Overall accuracy on test data is 91.83% (Low than the one with C=1 because of overfitting)
# Specificity  > 98%
# Sensitivity  > 84%

# observation:-
# Model performance has slightly decreased, model is becoming complex so overfitting

####################################################################################################
########## Cross validation for Linear Kernel
# Let's get the best value of C using 5-fold cross-validation
# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"

set.seed(100)
# making a grid of C values. 
grid <- expand.grid(C=c(0.001,0.005,0.1,0.2,0.5,1,2,5,10,50,100))

# Performing 5-fold cross validation
fit.svm <- train(label~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Printing cross validation result
print(fit.svm)

#Support Vector Machines with Linear Kernel 
#9000 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 7200, 7201, 7201, 7199, 7199 
#Resampling results across tuning parameters:

#C      Accuracy   Kappa    
#1e-03  0.8955553  0.8839255
#5e-03  0.9181123  0.9089963
#1e-01  0.9258897  0.9176360 (Best C value)
#2e-01  0.9222222  0.9135589
#5e-01  0.9149991  0.9055304
#1e+00  0.9111097  0.9012081
#2e+00  0.9092213  0.8991094
#5e+00  0.9078880  0.8976274
#1e+01  0.9077769  0.8975040
#5e+01  0.9077769  0.8975040
#1e+02  0.9077769  0.8975040
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was C = 0.1.

plot(fit.svm) # we can see that highest accuracy is at C=0.1

# Valdiating the model on test data
eval_cv <- predict(fit.svm,mnist_test)
confusionMatrix(eval_cv, mnist_test$label)

# Overall accuracy on test data is 93.1% (Higher than for C=1)
# Specificity  > 98% 
# Sensitivity  > 87% 

#observation:-
# Results obtained by cross validation for linear kernel are good,for C=0.1 it is giving highest accuracy

#########################################################################################################
######################################## Non-Linear Kernels

## Radial kernel using default parameters
model1_rbf <- ksvm(label ~ ., data =train,scaled=FALSE, kernel = "rbfdot")
print(model1_rbf) # Training error=0.017889
# Valdiating the model on test data
eval1_rbf <- predict(model1_rbf,mnist_test, type = "response")
confusionMatrix(eval1_rbf, mnist_test$label) 

# Overall accuracy on test data is 95.54% (Higher accuracy than Linear Kernel)
# specificity  > 99%
# sensitivity  > 92% 

#Observation:-
# we can see that the Non-Linear kernel with default parameters is giving good accuracy,sensitivity 
#and specificity when compared to Linear Kernels.

#####################################################################################################
################## Cross Validation for Non-Linear Kernel to get best C,sigma values
# Making grid of "sigma" and C values. 
grid <- expand.grid(C= c(0.01, 0.1, 1, 5, 10), sigma = c(0.001, 0.01, 0.1, 1, 5))

# Performing 5-fold cross validation
fit.svm_radial <- train(label~., data=train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Took 10 hrs for this model (more complex and more time consuming but yield good accuracy)
# Printing cross validation result
print(fit.svm_radial)

#Support Vector Machines with Radial Basis Function Kernel 

#9000 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 7199, 7201, 7199, 7200, 7201 
#Resampling results across tuning parameters:
  
#  C      sigma  Accuracy   Kappa    
#0.01  0.001  0.1071111  0.0000000
#0.01  0.010  0.6848883  0.6491686
#0.01  0.100  0.1071111  0.0000000
#0.01  1.000  0.1071111  0.0000000
#0.01  5.000  0.1071111  0.0000000
#0.10  0.001  0.8325549  0.8138350
#0.10  0.010  0.9172218  0.9080101
#0.10  0.100  0.2349998  0.1453251
#0.10  1.000  0.1071111  0.0000000
#0.10  5.000  0.1071111  0.0000000
#1.00  0.001  0.9105552  0.9005984
#1.00  0.010  0.9536661  0.9485088
#1.00  0.100  0.8567756  0.8408158
#1.00  1.000  0.1585542  0.0599387
#1.00  5.000  0.1071111  0.0000000
#5.00  0.001  0.9283332  0.9203549
#5.00  0.010  0.9637766  0.9597449
#5.00  0.100  0.8651095  0.8500793
#5.00  1.000  0.1628879  0.0647692
#5.00  5.000  0.1071111  0.0000000
#10.00  0.001  0.9332224  0.9257881
#10.00  0.010  0.9637769  0.9597451 (Best hyperparameter values)
#10.00  0.100  0.8651095  0.8500793
#10.00  1.000  0.1628879  0.0647692
#10.00  5.000  0.1071111  0.0000000

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.01 and C = 10.

plot(fit.svm_radial)
# we can conclude that sigma=0.01,c=10 are the best hyperparameter values

# Validating the model results on test data
evaluate_radial<- predict(fit.svm_radial, mnist_test)
confusionMatrix(evaluate_radial,mnist_test$label)

# Overall accuracy on test data is 96.35% (Higher than Non-linear Kernel with default parameters)
# specificity  >99.45% 
# sensitivity  >93.90% 

# Observation:-
# Accuracy on test data is 96.35% (Best model)
# We can see that accuracy,senstivity and specificity are highest for C=10,sigma=0.01

#######################################################################################################
################# Polynomial Kernel
# with default parameters
model1_polynomial <-ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE)
print(model1_polynomial) #Training error : 0.001778 

# Hyperparameters : degree =  1  scale =  1  offset =  1,cost C=1

# Number of Support Vectors : 2555 
# Training error : 0.001778
# Valdiating the model on test data
eval1_polynomial <- predict(model1_polynomial,mnist_test, type = "response")
confusionMatrix(eval1_polynomial, mnist_test$label) 

# Overall accuracy on test data is 92.03%
# specificity >98% 
# sensitivity >84% 

##########################################################################################################
################### Cross-validation on polynomial kernel
# Perform 2-fold cross-validation for this polynomial kernel as it is taking more computation time

# Making grid of C,degree,scale values 
grid_poly <- expand.grid(C= c(0.01, 0.1, 1, 5),degree=c(1,2,3,4),scale=c(-2,-1,1,2))

fit.svm_polynomial <- train(label~., data=train, method="svmPoly", metric="Accuracy",tuneGrid=grid_poly, 
        trControl = trainControl(method = "cv", number =2,verboseIter = TRUE), preProcess = NULL,Verbose=TRUE)

# By making verboseIter=TRUE in traincontrol we can see the progress of the model during Cross validation
# Even 2 folds took 5 hrs
print(fit.svm_polynomial)

#Support Vector Machines with Polynomial Kernel 
#9000 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#No pre-processing
#Resampling: Cross-Validated (2 fold) 
#Summary of sample sizes: 4500, 4500 
#Resampling results across tuning parameters:

#C    degree  scale  Accuracy     Kappa 
#0.01  2        1   0.953666667   0.9485103

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were degree = 2, scale = 1 and C = 0.01.
plot(fit.svm_polynomial)

# Valdiating the model on test data
eval_polynomial <- predict(fit.svm_polynomial,mnist_test)
confusionMatrix(eval_polynomial,mnist_test$label)

# Overall accuracy on test data is 95.94%
# specificity >99% (Very High)
# sensitivity >93.5% (high)

# Accuracy on test data is 95.94%

########################################################################################################
#################### Best Model and final results#######################################################
########################################################################################################
# we can see that for the given data Non-linear RBF kernel gives the best model
# with highest accuracy,sensitivity and specificity

# final model is fit.svm_radial (RBF Kernel with C=10,sigma=0.01)
final_model<-ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot",
                  C = 10, kpar = list(sigma =0.01))
print(final_model)
# Valdiating the model on test data
eval_final <- predict(final_model,mnist_test)
confusionMatrix(eval_final,mnist_test$label)
# Overall accuracy on test data is 96.35%
# specificity >99% (Very High)
# sensitivity >93.5% (high)

########## Conclusion
# Linear Kernel-hyperparameters C=0.1,accuracy on test data is 93.1%
# Polynomial Kernel-hyperparameters degree = 2, scale = 1 and C = 0.01,accuracy on test data is 95.94%
# RBF kernel-hyperparameters sigma=0.01,c=10,accuracy on test data is 96.35% (Best model)
# RBF kernel is more time consuming however gives best accuracy.