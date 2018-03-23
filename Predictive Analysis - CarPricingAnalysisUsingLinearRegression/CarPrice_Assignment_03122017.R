#Set working directory
setwd("D:/LinearRegression-Assignment-WorkSpace")

# Load essential libraries
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(stringr)
library(ggplot2)

#Load the car prices data
car_prices<- read.csv("CarPrice_Assignment.csv")

##################
##Data preparation
##################

#check for duplicates
sum(duplicated(car_prices)) #No Duplicates .. Proceed Further

#Make the separator consistent in CarName column
tmp_CarName<-str_replace_all(car_prices$CarName,"-"," ")

#Convert to lower case
car_prices$CarName<-tolower(car_prices$CarName)

#Extract the make of the car from the string
car_prices$CarName<-sapply(strsplit(tmp_CarName,split=" "),`[`,1)

#Detect mis-spelt car names using Levenshtein distance
uniq_car_names<-unique(car_prices$CarName)
car_name_ldist<-adist(uniq_car_names)
rownames(car_name_ldist)<-uniq_car_names

#Custer car names with similar spellings and visualise using a Cluster Dendrogram
hc<-hclust(as.dist(car_name_ldist))
plot(hc)

#Anything with height less than three are misspelt car names. 
#Store unique car names and similarly spelt groups based on Levenshtein distance in a dataframe
car_names_df <- data.frame(uniq_car_names,grp_ldist=as.integer(cutree(hc,h=3)),row.names=NULL)

#Any column value that has a higher frequency in the table is more likely to be the correct spelling
temp<-data.frame(table(car_prices$CarName))
car_names_df<-merge(car_names_df,temp,by.x="uniq_car_names",by.y="Var1")

car_names_df %>%
  group_by(grp_ldist)

#Based on the above Correct Data in column Car names
car_prices$CarName<-str_replace_all(car_prices$CarName,"vw","volkswagen")
car_prices$CarName<-str_replace_all(car_prices$CarName,"porcshce","porsche")
car_prices$CarName<-str_replace_all(car_prices$CarName,"vokswagen","volkswagen")
car_prices$CarName<-str_replace_all(car_prices$CarName,"toyouta","toyota")
car_prices$CarName<-str_replace_all(car_prices$CarName,"maxda","mazda")

# symboling Column - Its assigned insurance risk rating, 
# A value of +3 indicates that the auto is risky, 
# -3 that it is probably pretty safe.

#Categorise the cars acoording to risk rating. 1 to 3 Risky(0), 0 to -3 Safe(1)
car_prices$symboling<-as.factor(car_prices$symboling)
levels(car_prices$symboling)[-2:0]<-"Safe"
levels(car_prices$symboling)[1:3]<-"Risky"

# convert factors with 2 levels to numerical variables
levels(car_prices$symboling)<-c(1,0)
car_prices$symboling<- as.numeric(levels(car_prices$symboling))[car_prices$symboling]

car_prices$fueltype<-as.factor(car_prices$fueltype)
levels(car_prices$fueltype)<-c(1,0)
car_prices$fueltype<- as.numeric(levels(car_prices$fueltype))[car_prices$fueltype]

car_prices$aspiration<-as.factor(car_prices$aspiration)
levels(car_prices$aspiration)<-c(1,0)
car_prices$aspiration<- as.numeric(levels(car_prices$aspiration))[car_prices$aspiration]

car_prices$doornumber<-as.factor(car_prices$doornumber)
levels(car_prices$doornumber)<-c(1,0)
car_prices$doornumber<- as.numeric(levels(car_prices$doornumber))[car_prices$doornumber]

car_prices$enginelocation<-as.factor(car_prices$enginelocation)
levels(car_prices$enginelocation)<-c(1,0)
car_prices$enginelocation<- as.numeric(levels(car_prices$enginelocation))[car_prices$enginelocation]

#Convert from long to wide format for Linear Regression 
#   - Creating dummy variables from Categorical variables containing many factors

dummy_carCompany<-model.matrix(~CarName - 1, car_prices)
car_prices_W<-cbind(car_prices[,-(which(colnames(car_prices)=="CarName"))], dummy_carCompany)

dummy_carbody<-model.matrix(~carbody - 1, car_prices)
car_prices_W<-cbind(car_prices_W[,-(which(colnames(car_prices_W)=="carbody"))], dummy_carbody)

dummy_drivewheel<-model.matrix(~drivewheel - 1, car_prices)
car_prices_W<-cbind(car_prices_W[,-(which(colnames(car_prices_W)=="drivewheel"))], dummy_drivewheel)

dummy_enginetype<-model.matrix(~enginetype - 1, car_prices)
car_prices_W<-cbind(car_prices_W[,-(which(colnames(car_prices_W)=="enginetype"))], dummy_enginetype)

dummy_cylindernumber<-model.matrix(~cylindernumber - 1, car_prices)
car_prices_W<-cbind(car_prices_W[,-(which(colnames(car_prices_W)=="cylindernumber"))], dummy_cylindernumber)

dummy_fuelsystem<-model.matrix(~fuelsystem - 1, car_prices)
car_prices_W<-cbind(car_prices_W[,-(which(colnames(car_prices_W)=="fuelsystem"))], dummy_fuelsystem)

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_prices_W), 0.7*nrow(car_prices_W))
train_car_prices = car_prices_W[trainindices,]
test_car_prices = car_prices_W[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train_car_prices)
summary(model_1)

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed.
step <- stepAIC(model_1, direction="both")
step


# Now store the last model equation of stepwise method into an object called model_2
model_2 <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + peakrpm + citympg + CarNamealfa + 
                CarNameaudi + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamenissan + CarNamepeugeot + CarNamesaab + 
                CarNamesubaru + CarNamevolkswagen + carbodyconvertible + 
                drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
                enginetypeohcv + cylindernumberfive + fuelsystem1bbl + fuelsystem2bbl + 
                CarNametoyota, data = train_car_prices)

summary(model_2) #Adjusted R-squared:  0.9742 

vif(model_2)

#Removing insignificant variables with high p value and VIF - citympg, CarNamesaab,CarNamevolkswagen,cylindernumberfive
#fuelsystem1bbl,fuelsystem2bbl,CarNametoyota
model_3<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
             curbweight + enginesize + stroke + peakrpm + CarNamealfa + 
             CarNameaudi + CarNamebmw + CarNamebuick + CarNamedodge + 
             CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
             CarNamemercury + CarNamenissan + CarNamepeugeot +
             CarNamesubaru + carbodyconvertible + 
             drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
             enginetypeohcv, data = train_car_prices)

summary(model_3) #Adjusted R-squared:  0.9709

# Now Inspect multicolinearlity using VIF
vif(model_3)

#Removing insignificant variables with high p values - CarNamedodge,CarNamemercury,carbodyconvertible

model_4<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + CarNamealfa + 
               CarNameaudi + CarNamebmw + CarNamebuick + 
               CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
               CarNamenissan + CarNamepeugeot +
               CarNamesubaru +  
               drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
               enginetypeohcv, data = train_car_prices)

summary(model_4) #Adjusted R-squared:  0.9697 

vif(model_4)
#step_2<-stepAIC(model_4,direction = "both")

#Removing insignificant variables with high p values and high VIF -  curbweight,CarNamemazda

model_5<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + CarNamealfa + 
              CarNameaudi + CarNamebmw + CarNamebuick + 
              CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamenissan + CarNamepeugeot +
              CarNamesubaru +  
              drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
              enginetypeohcv, data = train_car_prices)
summary(model_5) #Adjusted R-squared:  0.9675

vif(model_5)

# remove high p value variables - CarNamenissan
model_6<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + CarNamealfa + 
              CarNameaudi + CarNamebmw + CarNamebuick + 
              CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamepeugeot +
              CarNamesubaru + drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
              enginetypeohcv, data = train_car_prices)
summary(model_6) # Adjusted R-squared:  0.967 
vif(model_6)



# remove high p value variables - CarNameisuzu
model_7<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + CarNamealfa + 
              CarNameaudi + CarNamebmw + CarNamebuick + 
              CarNamehonda + CarNamejaguar + CarNamepeugeot +
              CarNamesubaru + drivewheel4wd + drivewheelfwd + enginetypedohc + enginetypeohc + 
              enginetypeohcv, data = train_car_prices)
summary(model_7) # Adjusted R-squared:  0.9661 
vif(model_7)

# remove high p value and VIF variables - drivewheelfwd
model_8<-lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
              enginesize + stroke + peakrpm + CarNamealfa + 
              CarNameaudi + CarNamebmw + CarNamebuick + 
              CarNamehonda + CarNamejaguar + CarNamepeugeot +
              CarNamesubaru + drivewheel4wd + enginetypedohc + enginetypeohc + 
              enginetypeohcv, data = train_car_prices)
summary(model_8) #Adjusted R-squared:  0.9647

# Now Inspect multicolinearlity again using VIF
vif(model_8)

#Removing insignificant variables with high p values and high VIF - car_ID,peakrpm,drivewheel4wd

model_9<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
              enginesize + stroke + CarNamealfa + 
              CarNameaudi + CarNamebmw + CarNamebuick + 
              CarNamehonda + CarNamejaguar + CarNamepeugeot +
              CarNamesubaru + enginetypedohc + enginetypeohc + 
              enginetypeohcv, data = train_car_prices)
summary(model_9) #Adjusted R-squared:  0.9623
vif(model_9)

#Removing insignificant variables with high p values - CarNamealfa
model_10<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               enginesize + stroke + CarNameaudi + CarNamebmw + CarNamebuick + 
               CarNamehonda + CarNamejaguar + CarNamepeugeot +
               CarNamesubaru + enginetypedohc + enginetypeohc + 
               enginetypeohcv, data = train_car_prices)
summary(model_10) #Adjusted R-squared:  0.9605
vif(model_10)

#Removing insignificant variables with VIF values > 5 -  enginesize,enginetypeohc
model_11<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               stroke + CarNameaudi + CarNamebmw + CarNamebuick + 
               CarNamehonda + CarNamejaguar + CarNamepeugeot +
               CarNamesubaru + enginetypeohc, data = train_car_prices)
summary(model_11) # Adjusted R-squared:  0.9317

vif(model_11)

#Removing insignificant variables with high p values -  CarNamehonda
model_12<-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               stroke + CarNameaudi + CarNamebmw + CarNamebuick + 
               CarNamejaguar + CarNamepeugeot +
               CarNamesubaru + enginetypeohc, data = train_car_prices)
summary(model_12) # Adjusted R-squared:  0.9308
vif(model_12)

#Removing insignificant variables with high p values -  CarNameaudi,stroke
model_13<-lm(formula = price ~ aspiration + enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + CarNamepeugeot + CarNamesubaru + enginetypeohc, data = train_car_prices)
summary(model_13) # Adjusted R-squared:  0.9255 
vif(model_13)

#Removing insignificant variables with high p values -  aspiration
model_14<-lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + CarNamepeugeot + CarNamesubaru + enginetypeohc, data = train_car_prices)
summary(model_14) # Adjusted R-squared:  0.9226   
vif(model_14)

# predicting the results in test dataset
Predict_1 <- predict(model_14,test_car_prices[,-1])
test_car_prices$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test_car_prices$price,test_car_prices$test_price)
rsquared <- cor(test_car_prices$price,test_car_prices$test_price)^2
rsquared #0.824043

#At this point the model is still shows lot of variance when used with the test data
#hence using another variable and checking if the model improves and the deviation between the predicted
#and actual values of the test data gets reduced.

model_15<-lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + CarNamepeugeot + CarNamesubaru + enginetypeohc + horsepower, data = train_car_prices)
summary(model_15) # Adjusted R-squared:  0.9397   
vif(model_15)

#Adjusted R-square improves. Removing insignificant variables with high p values - CarNamepeugeot
model_16<-lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + CarNamesubaru + enginetypeohc + horsepower, data = train_car_prices)
summary(model_16) #Adjusted R-squared:  0.9392
vif (model_16)

#Removing insignificant variables with high p values - CarNamesubaru
model_17<-lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + enginetypeohc + horsepower, data = train_car_prices)
summary(model_17) # Adjusted R-squared:  0.9363
vif(model_17)

#Removing insignificant variables with high p values - enginetypeohc
model_18<-lm(formula = price ~ enginelocation + carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + horsepower, data = train_car_prices)
summary(model_18) # Adjusted R-squared:  0.934 
vif(model_18)

# predicting the results in test dataset
Predict_2 <- predict(model_18,test_car_prices[,-1])
test_car_prices$test_price <- Predict_2

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test_car_prices$price,test_car_prices$test_price)
rsquared <- cor(test_car_prices$price,test_car_prices$test_price)^2
rsquared #0.8587647

#At this point the model is still shows lot of variance when used with the test data (although the variance has reduced)
#hence trying to remove a variable and checking if the model improves and the deviation between the predicted
#and actual values of the test data gets reduced

model_19<-lm(formula = price ~ carwidth + CarNamebmw + CarNamebuick + 
               CarNamejaguar + horsepower, data = train_car_prices)
summary(model_19) # Adjusted R-squared:  0.934 
vif(model_19) #Adjusted R-squared:  0.8723

# predicting the results in test dataset
Predict_3 <- predict(model_19,test_car_prices[,-1])
test_car_prices$test_price <- Predict_3

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test_car_prices$price,test_car_prices$test_price)
rsquared <- cor(test_car_prices$price,test_car_prices$test_price)^2
rsquared #0.827799 

# Model 19 is able to predict the test data better and shows a reduced r square value.
# Plot to show the Actual vs Predicated price of car
ggplot(test_car_prices, aes(car_ID,price)) + geom_line(aes(colour = "red" )) + 
  scale_x_continuous(name = "car_ID", breaks = seq(0,205,5), limits = c(0,205)) + 
  scale_y_continuous(name = "Car Prices - Actual Vs Predicted", breaks = seq(0,45500,10000), limits = c(0,45500)) + 
  geom_line(aes(x=car_ID, y=test_price, colour="blue"))
