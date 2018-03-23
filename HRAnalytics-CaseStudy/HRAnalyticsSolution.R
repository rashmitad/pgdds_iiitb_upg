#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the past and current customer information,
# the company has maintained a database containing personal/demographic information,
# the services availed by a customer and the expense information related to each customer.

## AIM:

# The aim is to automate the process of predicting 
# if a customer would telecom or not and to find the factors affecting the telecom. 
# Whether a customer will telecom or not will depend on data from the following three buckets:

# 1. Demographic Information
# 2. Services Availed by the customer
# 3. Overall Expenses

################################################################

### Data Understanding

# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)


#Helper funtions 
#Function to convert Factor variables to dummy variables
#data  - input dataframe
#column_factors - list of Factor variable names
#output - converted dataframe with dummy variables
convert_to_dummies <- function(data , column_factors) {
  
  for (col_name in column_factors) {
    
    #check whether the column is available data
    #if not available return blank
    if (!(col_name %in% names(data))) {
      return("")
      
    }
    
    #getting column index
    col_index = which(names(data) == col_name)
    
    #process each column for dummy variable creation
    if (is.factor(data[, col_index]) == F) {
      data[, col_index] <- as.factor(data[, col_index])
    }
    
    #check levels of column
    
    level_length <- length(levels(data[, col_index]))
    
    #if level <2 remove the column
    #if level ==2 create level as 1,0
    #if level > 2 create with dummy variable model.matrix function
    if (level_length < 2) {
      data <- col_index[, -col_index]
    } else if (level_length == 2) {
      levels(data[, col_index]) <- c(1, 0)
      data[, col_index] <-
        as.numeric(levels(data[, col_index]))[data[, col_index]]
    } else {
      dummy <-
        data.frame(model.matrix(as.formula(paste("~", col_name)), data = data))
      data <- cbind(data[, -col_index], dummy[, -1])
    }
    
  }
  #if everything ok return the processed data.frame
  return(data)
}

#Loading dataset
general_data <- read.csv("general_data.csv",stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F,check.names = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F,check.names = F  )

#Viewing the structure of the dataframes
str(general_data) #24 variables including target variable
str(employee_survey_data)
str(manager_survey_data)
str(in_time)
str(out_time)

##############################################################################
##########################  Data Preparation ##########################
##############################################################################
#Process in_time and out_time dataframe to get meaningful data
names(in_time)[1] <- "EmployeeID"
in_time <- in_time[order(in_time$EmployeeID),]

names(out_time)[1] <- "EmployeeID"
out_time <- out_time[order(out_time$EmployeeID),]

#Check all variable are unique
sum(duplicated(in_time$EmployeeID)) #Unique
sum(duplicated(out_time$EmployeeID))#Unique

#Check two data.frame are same length
length(in_time$EmployeeID) #4410
length(out_time$EmployeeID) #4410


#Convert time data to datatime format
in_time_ymd_hms <- data.frame(lapply(in_time[,-1],ymd_hms))
out_time_ymd_hms <- data.frame(lapply(out_time[,-1],ymd_hms))

#Check is there any difference in EmployeeID in both dataframe
setdiff(out_time$EmployeeID,in_time$EmployeeID) #No difference


#Calculate working hours of employees
#Since both having same employeeid and length, by substracting we will get working hours per day
#
work_hours <- out_time_ymd_hms - in_time_ymd_hms
work_hours <- as.data.frame(lapply(work_hours, as.numeric))
work_hours <- round(work_hours,2)
head(work_hours)

#Remove columns having all values as NA 
#These are considered as holidays
work_hours <- work_hours[vapply(work_hours, function(x) length(unique(x)) >1, logical(1L))]

total_workingdays <- length(names(work_hours)) #249

#Calcluate average working hour for each employee considering their presence 

work_hours$total_workhours <- rowSums(work_hours,na.rm =T)
work_hours$total_leave <- rowSums(is.na(work_hours))
work_hours$total_present <- total_workingdays- work_hours$total_leave
work_hours$average_workhr <- work_hours$total_workhours/work_hours$total_present

time_detail <- work_hours[,c(251,252,253)]
emp_time_detail <- cbind(in_time[,1],time_detail)
names(emp_time_detail)[1] <- "EmployeeID"


#Merge All dataframe to a masterdataframe
#Employee Merged data
emp_merged <- merge(general_data,emp_time_detail,by="EmployeeID",all = F)
emp_merged <- merge(emp_merged,manager_survey_data,by="EmployeeID",all = F)
emp_merged <- merge(emp_merged,employee_survey_data,by="EmployeeID",all = F)


#Remove columns which have no variations (i.e same value to all rows)
emp_merged <- emp_merged[vapply(emp_merged, function(x) length(unique(x)) >1, logical(1L))]


#Remove rows which are having NA values
emp_merged <- emp_merged[complete.cases(emp_merged),]

#############################################################################
### Data Preparation & Exploratory Data Analysis##########################
##############################################################################
# Understanding the structure of the collated file
str(emp_merged) #4300 obs. of  29 variables:

# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(emp_merged,aes(x=Department,fill=Attrition)) + geom_bar(),
          ggplot(emp_merged,aes(x=Education,fill=Attrition)) + geom_bar()+ bar_theme1,
          ggplot(emp_merged,aes(x=JobLevel,fill = Attrition)) + geom_bar()+ bar_theme1,
          ggplot(emp_merged,aes(x=BusinessTravel,fill = Attrition)) + geom_bar()+ bar_theme1,
          align = "h")

plot_grid(ggplot(emp_merged,aes(x=Gender,fill = Attrition)) + geom_bar(),
          ggplot(emp_merged,aes(x=MaritalStatus,fill = Attrition)) + geom_bar()+bar_theme1,
          ggplot(emp_merged,aes(x=StockOptionLevel,fill = Attrition)) + geom_bar()+ bar_theme1,
          ggplot(emp_merged,aes(x=JobRole,fill = Attrition)) + geom_bar()+ bar_theme1,
          align="h")


plot_grid(ggplot(emp_merged,aes(x=EnvironmentSatisfaction,fill = Attrition)) + geom_bar(),
          ggplot(emp_merged,aes(x=JobSatisfaction,fill = Attrition)) + geom_bar()+bar_theme1,
          ggplot(emp_merged,aes(x=WorkLifeBalance,fill = Attrition)) + geom_bar()+bar_theme1,
          align="h")

plot_grid(ggplot(emp_merged,aes(x=JobInvolvement,fill = Attrition)) + geom_bar(),
          ggplot(emp_merged,aes(x=PerformanceRating,fill = Attrition)) + geom_bar()+bar_theme1,
          align="h")



#Numeric variable distribution Box Plots

box_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="none")


plot_grid(ggplot(emp_merged, aes(MonthlyIncome,fill='MonthlyIncome'))+ geom_histogram(binwidth = 10000,colour='blue'),
          ggplot(emp_merged, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.3)+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(Age,fill='Age'))+ geom_histogram(binwidth = 3,colour='blue'),
          ggplot(emp_merged, aes(x="",y=Age))+ geom_boxplot(width=0.3)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(YearsAtCompany,fill='YearsAtCompany'))+ geom_histogram(binwidth = 1,colour='blue'),
          ggplot(emp_merged, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(TotalWorkingYears,fill='TotalWorkingYears'))+ geom_histogram(binwidth = 5,colour='blue'),
          ggplot(emp_merged, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(YearsSinceLastPromotion,fill='YearsSinceLastPromotion'))+ geom_histogram(binwidth = 5,colour='blue'),
          ggplot(emp_merged, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(average_workhr,fill='Average_workhr'))+ geom_histogram(binwidth = 1,colour='blue'),
          ggplot(emp_merged, aes(x="",y=average_workhr))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(TrainingTimesLastYear,fill='TrainingTimesLastYear'))+ geom_histogram(binwidth = 1,colour='blue'),
          ggplot(emp_merged, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.2)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(emp_merged, aes(YearsWithCurrManager,fill='YearsWithCurrManager'))+ geom_histogram(binwidth =1,colour='blue'),
          ggplot(emp_merged, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.3)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Outlier treatment and imputing missing value
#Boxplot showed outliers
monthlyincome_outliers <- boxplot.stats(general_data$MonthlyIncome)$out
length(monthlyincome_outliers)
#Since 342 outliers are there 8% of distribution might loss so we wil keep the data as well 

#yearswithcurrMangaer TrainingTimesLastYear average_workhr YearsSinceLastPromotion YearsAtCompany 
#also showing outliers since by business understanding these can happen. So we are excluding those data as outliers


##################################################################################################
#Category variables
#general_data categories
general_categories <- c(
  "BusinessTravel",
  "Department",
  "Education",
  "EducationField",
  "Gender",
  "JobLevel",
  "JobRole",
  "MaritalStatus",
  "StockOptionLevel"
)

employee_survey_data_categories <- c(
  "EnvironmentSatisfaction",
  "JobSatisfaction",
  "WorkLifeBalance"
)

manager_survey_data_categories <- c(
  "JobInvolvement",
  "PerformanceRating"
)

emp_category_list <- c(general_categories,employee_survey_data_categories,manager_survey_data_categories)
continuous_list <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                     "TrainingTimesLastYear",
                     "YearsAtCompany",
                     "YearsSinceLastPromotion",
                     "YearsWithCurrManager",
                     "total_leave",
                     "total_present",
                     "PercentSalaryHike",
                     "average_workhr")

##################################################################################################

##################################################################################################
# Correlation between numeric variables
library(GGally)
ggpairs(emp_merged[, continuous_list])






#Convert Factor variables into dummy variables
emp_master <- convert_to_dummies(emp_merged,emp_category_list)
emp_master$Attrition<- ifelse(emp_master$Attrition=="Yes",1,0)

#Normalize continous variables
for (clist in continuous_list){
  col_index <- which(names(emp_master) == clist)
  emp_master[,col_index] = scale( emp_master[,col_index])
}

#Attrition ratio
attrition <- sum(emp_master$Attrition)/nrow(emp_master)
attrition  # 16 % attrition is happening


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(emp_master$EmployeeID, SplitRatio = 0.7)

emp_train = emp_master[indices,-1]

emp_test = emp_master[!(indices),-1] 


########################################################################
########################### Model building ##########################
########################################################################
emp_model_1 = glm(Attrition ~ ., data = emp_train, family = "binomial")
summary(emp_model_1)

emp_stepAIC_model <- stepAIC(emp_model_1,direction = "both")
summary(emp_stepAIC_model)
sort(vif(emp_stepAIC_model),decreasing = T)

#List of selected columns from StepAIC funtion
emp_set_2 <- names(emp_stepAIC_model$model)[-1]


#Analyze model 2 
emp_model_2 <- glm(paste("Attrition ~",paste(emp_set_2,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_2)
sort(vif(emp_model_2),decreasing = T)



############################# Model 3 Validation #############################

#YearsAtCompany column have higher vif and insignificant 
#Removing YearsAtCompany
emp_set_3 <- emp_set_2[-which(emp_set_2 =="YearsAtCompany")]

#Model 3
emp_model_3 <-glm(paste("Attrition ~",paste(emp_set_3,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_3)
sort(vif(emp_model_3),decreasing = T)

############################# Model 4 Validation #############################

#BusinessTravelTravel_Rarely column have higher vif and insignificant 
#Removing BusinessTravelTravel_Rarely
emp_set_4 <- emp_set_3[-which(emp_set_3 =="BusinessTravelTravel_Rarely")]

#Model 4
emp_model_4 <-glm(paste("Attrition ~",paste(emp_set_4,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_4)
sort(vif(emp_model_4),decreasing = T)


#At this point variable haveing vif values >2 having higher significance.
#lets look into p value for variable selection

############################# Model 5 Validation #############################

#Gender  having higher p values
#Romove Gender  from the variable set
emp_set_5 <- emp_set_4[-which(emp_set_4 =="Gender")]

#Model 5
emp_model_5 <-glm(paste("Attrition ~",paste(emp_set_5,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_5)




############################# Model 6 Validation #############################

# JobRoleManufacturing.Director have higher pvalue 
#Removing JobRoleManufacturing.Director from the variable set
emp_set_6 <- emp_set_5[-which(emp_set_5 =="JobRoleManufacturing.Director")]

#Model 6
emp_model_6 <-glm(paste("Attrition ~",paste(emp_set_6,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_6)
sort(vif(emp_model_6),decreasing = T)


############################# Model 7 Validation #############################

# MonthlyIncome have higher pvalue
#Removing MonthlyIncome from the variable set
emp_set_7 <- emp_set_6[-which(emp_set_6 =="MonthlyIncome")]

#Model 7
emp_model_7 <-glm(paste("Attrition ~",paste(emp_set_7,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_7)
sort(vif(emp_model_7),decreasing = T)


############################# Model 8 Validation #############################

# EducationFieldLife.Sciences have higher vif and insignificant 
#Removing EducationFieldLife.Sciences from the variable set
emp_set_8 <- emp_set_7[-which(emp_set_7 =="EducationFieldLife.Sciences")]

#Model 8
emp_model_8 <-glm(paste("Attrition ~",paste(emp_set_8,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_8)
sort(vif(emp_model_8),decreasing = T)


############################# Model 9 Validation #############################

# JobRoleLaboratory.Technician have higher vif and insignificant 
#Removing JobRoleLaboratory.Technician from the variable set
emp_set_9 <- emp_set_8[-which(emp_set_8 =="JobRoleLaboratory.Technician")]

#Model 9
emp_model_9 <-glm(paste("Attrition ~",paste(emp_set_9,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_9)
sort(vif(emp_model_9),decreasing = T)


############################# Model 10 Validation #############################

# Education3have higher vif and insignificant 
#Removing Education3 from the variable set
emp_set_10 <- emp_set_9[-which(emp_set_9 =="Education3")]

#Model 10
emp_model_10 <-glm(paste("Attrition ~",paste(emp_set_10,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_10)
sort(vif(emp_model_10),decreasing = T)

############################# Model 11 Validation #############################

# Education5 have higher vif and insignificant 
#Removing Education5 from the variable set
emp_set_11 <- emp_set_10[-which(emp_set_10 =="Education5")]

#Model 11
emp_model_11 <-glm(paste("Attrition ~",paste(emp_set_11,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_11)
sort(vif(emp_model_11),decreasing = T)

############################# Model 12 Validation #############################

# Education4 have higher pvalue
#Removing Education4 from the variable set
emp_set_12 <- emp_set_11[-which(emp_set_11 =="Education4")]

#Model 12
emp_model_12 <-glm(paste("Attrition ~",paste(emp_set_12,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_12)
sort(vif(emp_model_12),decreasing = T)


############################# Model 13 Validation #############################

# JobRoleResearch.Director have higher pvalue 
#Removing JobRoleResearch.Director from the variable set
emp_set_13 <- emp_set_12[-which(emp_set_12 =="JobRoleResearch.Director")]

#Model 13
emp_model_13 <-glm(paste("Attrition ~",paste(emp_set_13,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_13)
sort(vif(emp_model_13),decreasing = T)

############################# Model 14 Validation #############################

# JobRoleSales.Executive have higher pvalue 
#Removing JobRoleSales.Executive from the variable set
emp_set_14 <- emp_set_13[-which(emp_set_13 =="JobRoleSales.Executive")]

#Model 14
emp_model_14 <-glm(paste("Attrition ~",paste(emp_set_14,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_14)
sort(vif(emp_model_14),decreasing = T)


############################# Model 15 Validation #############################

# JobInvolvement3 have higher pvalue 
#Removing JobInvolvement3 from the variable set
emp_set_15 <- emp_set_14[-which(emp_set_14 =="JobInvolvement3")]

#Model 15
emp_model_15 <-glm(paste("Attrition ~",paste(emp_set_15,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_15)
sort(vif(emp_model_15),decreasing = T)


############################# Model 16 Validation #############################

# JobSatisfaction2 have higher pvalue 
#Removing JobSatisfaction2 from the variable set
emp_set_16 <- emp_set_15[-which(emp_set_15 =="JobSatisfaction2")]

#Model 16
emp_model_16 <-glm(paste("Attrition ~",paste(emp_set_16,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_16)
sort(vif(emp_model_16),decreasing = T)



############################# Model 17 Validation #############################

# JobSatisfaction3 have higher pvalue 
#Removing JobSatisfaction3 from the variable set
emp_set_17 <- emp_set_16[-which(emp_set_16 =="JobSatisfaction3")]

#Model 17
emp_model_17 <-glm(paste("Attrition ~",paste(emp_set_17,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_17)
sort(vif(emp_model_17),decreasing = T)


############################# Model 18 Validation #############################

# TrainingTimesLastYear have higher pvalue 
#Removing TrainingTimesLastYear from the variable set
emp_set_18 <- emp_set_17[-which(emp_set_17 =="TrainingTimesLastYear")]

#Model 18
emp_model_18 <-glm(paste("Attrition ~",paste(emp_set_18,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_18)
sort(vif(emp_model_18),decreasing = T)

############################# Model 19 Validation #############################

# JobLevel5 have higher pvalue 
#Removing JobLevel5 from the variable set
emp_set_19 <- emp_set_18[-which(emp_set_18 =="JobLevel5")]

#Model 19
emp_model_19 <-glm(paste("Attrition ~",paste(emp_set_19,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_19)
sort(vif(emp_model_19),decreasing = T)


#Remove higher p values among and check if there any variance in variance 

############################# Model 20 Validation #############################

# DepartmentResearch...Developmenthave higher pvalue 
#Removing DepartmentResearch...Development from the variable set
emp_set_20 <- emp_set_19[-which(emp_set_19 =="DepartmentResearch...Development")]

#Model 20
emp_model_20 <-glm(paste("Attrition ~",paste(emp_set_20,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_20)
sort(vif(emp_model_20),decreasing = T)

############################# Model 21 Validation #############################

# DepartmentSales becomes insignificant 
#Removing DepartmentSales from the variable set
emp_set_21 <- emp_set_20[-which(emp_set_20 =="DepartmentSales")]

#Model 21
emp_model_21 <-glm(paste("Attrition ~",paste(emp_set_21,collapse = "+")),data = emp_train,family = "binomial")

summary(emp_model_21)
sort(vif(emp_model_21),decreasing = T)

#We can stop and check at this point. 
#All variables become highly significant 


#################################### Model Evaluation ############################################
##################################################################################################
#Lets validate our model accuracy

#predicted probabilities of Churn 1 for test data

emp_test_pred = predict(emp_model_21, type = "response", 
                        newdata = emp_test)


#Summary 
summary(emp_test_pred)

#Add Predicted probability to Test data
emp_test$prob <- emp_test_pred


#

emptest_pred_attrition <- factor(ifelse(emp_test_pred >= 0.50, "Yes", "No"))
emptest_actual_attrition <- factor(ifelse(emp_test$Attrition==1,"Yes","No"))


table(emptest_pred_attrition,emptest_actual_attrition)

emp_test_conf <- confusionMatrix(emptest_pred_attrition,emptest_actual_attrition, positive = "Yes")

emp_test_conf

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(emp_test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, emptest_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


#Graph for Accuracy, ,sensitivity and specificity

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))  


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff # 0.169


# Let's choose a cutoff value of 0.17 for final model
#

emptest_pred_attrition <- factor(ifelse(emp_test_pred >= .17, "Yes", "No"))
emptest_actual_attrition <- factor(ifelse(emp_test$Attrition==1,"Yes","No"))


table(emptest_pred_attrition,emptest_actual_attrition)

conf_final <- confusionMatrix(emptest_pred_attrition, emptest_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #76.27 %

sens # 76.12%

spec #76.31 %

##################################################################################################
### KS -statistic - Test Data ######

emptest_cutoff_attrition <- ifelse(emptest_pred_attrition=="Yes",1,0)
emptest_actual_attrition <- ifelse(emptest_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(emptest_cutoff_attrition, emptest_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#KS value .52 (A good model should have more than 40%)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(emptest_actual_attrition, emp_test_pred, groups = 10)



#From the graph our model is better than the Random model
#Since A perfect model reaches 100 % in its 3rd decile our model is in between the two
ggplot(Attrition_decile, aes(bucket)) + 
  geom_line(aes(y = Gain, colour = "Model")) + 
  geom_line(aes(y = seq(10,100,10), colour = "Random Model"))




#Lift Chart
Lift <- Attrition_decile$Gain/seq(10,100,10)
plot(c(1:10),Lift)






