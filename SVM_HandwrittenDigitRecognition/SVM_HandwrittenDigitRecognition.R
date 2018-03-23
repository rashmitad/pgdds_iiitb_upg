############################ SVM Handwritten Digit Recognition #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#An image of a digit submitted by a user via a scanner, a tablet
#or other digital devices. The goal is to develop a model that can correctly identify 
#handwritten digits (between 0-9) written in an image. 

#####################################################################################

# 2. Data Understanding: 
# pixel values given as features in the train and test datasets
# The first column indicates the digit
# No of features (remaining columns) : 


#3. Data Preparation:

#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(doParallel)
library(e1071)

#Loading Data
setwd("C:/Users/Rashmita/Downloads")

test <- read_csv("mnist_test.csv",col_names = FALSE)
colnames(test)[1]<- "label"

train <- read_csv("mnist_train.csv",col_names = FALSE)
colnames(train)[1]<- "label"


#Understanding Dimensions

dim(test)
dim(train)

#Structure of the dataset

str(test)
str(train)

#printing first few rows

head(test)
head(train)

#Exploring the data

summary(test)
summary(train)

#checking missing value

sapply(test, function(x) sum(is.na(x)))
sapply(train, function(x) sum(is.na(x)))

#Making our target class to factor

test$label<-factor(test$label)

train$label<-factor(train$label)

#Normalize factors - Explanation X = (X - min) / (max - min) => X = (X - 0) / (255 - 0) => X = X / 255.
min(test[-1])
max(test[-1])
test_n <- test[-1]/255
min(train[-1])
max(train[-1])
train_n <- train[-1]/255

test <- cbind.data.frame(test[1],test_n)
train <- cbind.data.frame(train[1],train_n)

#Constructing Model

# Split training data containing digit and pixel values for training and cross validation.

set.seed(1)
training.indices = sample(1:nrow(train), 0.1*nrow(train))
testing.indices = sample(1:nrow(test), 0.1*nrow(test))
training = train[training.indices, ]
testing = train[testing.indices, ]


#Using Linear Kernel
Model_linear <- ksvm(label~ ., data = training, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, testing)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,testing$label)

#The accuracy obtained using a Linear Kernel was 0.92


#Using RBF Kernel
Model_RBF <- ksvm(label~ ., data = training, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, testing)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,testing$label)

#The accuracy obtained using a RBF Kernel was around 0.96


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )



# Enable parallel processing.
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
Sys.time()
fit.svm <- train(label~., data=training, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
Sys.time()

print(fit.svm)

plot(fit.svm)

######Conclusion#######

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.025 and C = 2. 
#Accuracy obtained with the final model was 0.9609989
#The accuracy obtained using a Linear Kernel was 0.92 whereas the final model with RBF kernel and tuned hyperparameters was 0.9609989.
#Thus Radial Basis Function for the kernel can provide the most accurate result using SVM for the digit recognition problem
