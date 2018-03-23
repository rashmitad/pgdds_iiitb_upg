##############################################
##############################################
#Case Study - Global Mart Sales Prediction
#For the next 6 months
##############################################
##############################################

#Set working directory
setwd("C:\\Users\\Rashmita\\Documents\\Time Series")

# Load essential libraries

library(forecast)
library(ggplot2)
library(tseries)
#Time series analysis

#Load the car prices data
globalstore_ds <- read.csv("Global Superstore.csv",stringsAsFactors = F)
str(globalstore_ds)

##################
##################
##Data preparation
##################
##################


nrow(globalstore_ds) #51290 Rows
ncol(globalstore_ds) # 24 Columns/Attributes representing customer transactions

levels(globalstore_ds$Market) 
#"Africa","APAC","Canada","EMEA","EU","LATAM","US" - 7 levels in the Market Column
#representing the geographical market sector that the customer belongs to. 
levels(globalstore_ds$Segment)
#"Consumer","Corporate","Home Office" - 3 Levels in the Segment Column
#pertaining to 3 segments that customer belongs to


#check for duplicates
sum(duplicated(globalstore_ds))#No Duplicates .. Proceed Further

# Missing value Detection
sapply(globalstore_ds, function(x) sum(is.na(x))) # shows that Postal.Code column contains 41296 NAs
View(subset(globalstore_ds, is.na(Postal.Code)))  # 41296 NAs belong to contries other than US
View(subset(globalstore_ds, !is.na(Postal.Code))) # 9994 Entries pertaining to areas in US have zipcode

# Bringing the variables in the correct format
str(globalstore_ds)


#filterout important columns
# Filter only, "Order.Date" "Segment"    "Market"     "Sales"      "Quantity"   "Profit"  
globalstore<- globalstore_ds[,c(3,8,13,19,20,22)]

names(globalstore)

#Convert to date
globalstore$Order.Date <- as.Date(globalstore$Order.Date,"%d-%m-%Y")
globalstore$Order.Date <- format(globalstore$Order.Date,"%y-%m")


#Create 21 segments
globalstore$MarketSegment <- paste(globalstore$Segment,globalstore$Market,sep = "_")

globalstore$MarketSegment <- factor(globalstore$MarketSegment)

##Check the Nas and No NAs found
sum(is.na(globalstore))

##################
##################
##Data analysis
##################
##################


#to find out the topmost consistently profitable segment 
marketsegments <- unique(globalstore$MarketSegment)

#COV <-0
#index <-0
#resultlist <- matrix(0,length(marketsegments),3)
#for( i in 1:length(marketsegments)){
#  temp <- subset(globalstore,globalstore$MarketSegment==marketsegments[i])
#  temp <- aggregate(cbind(Sales,Profit,Quantity)~Order.Date,temp,sum)
#  temp <- temp[1:42,]
#  #CV calcluations
#  temp_CV <- sd(temp$Profit)/mean(temp$Profit)
#  resultlist[i,] <- c(as.character( marketsegments[i]),temp_CV,nrow(temp))
  
#}

#resultlist
#Consistent and most profitable

#Consumer_APAC 0.60363
#Consumer_EU  0.6553

resultlist <- matrix(0,length(marketsegments),6)
for( i in 1:length(marketsegments)){
  temp <- subset(globalstore,globalstore$MarketSegment==marketsegments[i])
  temp <- aggregate(cbind(Sales,Profit,Quantity)~Order.Date,temp,sum)
  temp1 <- temp[1:42,]
  #CV calcluations
  temp_CV <- sd(temp1$Profit)/mean(temp1$Profit)
  resultlist[i,] <- c(as.character( marketsegments[i]),temp_CV,nrow(temp),sum(temp$Quantity),sum(temp$Sales),sum(temp$Profit))
  
}

#EDA Analsis
resultlistdf <- data.frame(resultlist)
names(resultlistdf) <- c("Category","COV","Month","Quantity","Sales","Profit")

plot1<-ggplot(resultlistdf,aes(x=resultlistdf$Category,y=resultlistdf$COV,fill=resultlistdf$Category))
plot1<- plot1+ geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none")
plot1 + xlab("Market Segment")+ggtitle("COV Profit")

resultlist
#Consistent and most profitable

#Consumer_APAC 0.60363
#Consumer_EU  0.6553

####################################
####################################
##Model Building-Consumer_APAC Sales
####################################
####################################

#Consumer_APAC Sales forecasting

consumer.apac <- subset(globalstore,globalstore$MarketSegment=="Consumer_APAC")
consumer.apac <- aggregate(cbind(Sales,Quantity)~Order.Date,consumer.apac,sum)
head(consumer.apac)

consumer.apac$Month <- seq(1,nrow(consumer.apac),1)
#split data
consumer.apac.in <- consumer.apac[1:42,]
consumer.apac.out <- consumer.apac[43:48,]

consumer.in.timeser <- ts(consumer.apac.in$Sales)
plot(consumer.in.timeser)

w <-1
consumer.apac.in.smoothed <- filter(consumer.in.timeser, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)

#Smoothing left end of the time series

diff <- consumer.apac.in.smoothed[w+2] - consumer.apac.in.smoothed[w+1]
for (i in seq(w,1,-1)) {
  consumer.apac.in.smoothed[i] <- consumer.apac.in.smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer.in.timeser)
diff <- consumer.apac.in.smoothed[n-w] - consumer.apac.in.smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer.apac.in.smoothed[i] <- consumer.apac.in.smoothed[i-1] + diff
}

#Plot the smoothed time series
#consumer.apac.in$Month <- seq(1,nrow(consumer.apac.in),1)
timevals_in <- consumer.apac.in$Month
lines(consumer.apac.in.smoothed, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(consumer.apac.in.smoothed)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- consumer.in.timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#So the residual series is a white noise

######################################
######################################
##Model Evaluation-Consumer_APAC Sales
######################################
######################################

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- consumer.apac.out
timevals_out <- as.data.frame(seq(nrow(consumer.apac.in)+1,nrow(consumer.apac.in)+nrow(consumer.apac.out),1))
timevals_out <-consumer.apac.out$Month
timevals_out <- as.data.frame(timevals_out)
names(timevals_out)[1] <- "Month"
global_pred_out <- predict(lmfit,timevals_out)

fcast <- global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(consumer.apac$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(consumer.in.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer.in.timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,consumer.apac.out[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

##################################
##################################
##Model Building-Consumer_EU Sales
##################################
##################################

#Consumer_EU attribute Sales forecasting


consumer.eu <- subset(globalstore,globalstore$MarketSegment=="Consumer_EU")
consumer.eu <- aggregate(cbind(Sales,Quantity)~Order.Date,consumer.eu,sum)
head(consumer.eu)

consumer.eu$Month <- seq(1,nrow(consumer.eu),1)
#split data
consumer.eu.in <- consumer.eu[1:42,]
consumer.eu.out <- consumer.eu[43:48,]

consumer.eu.in.timeser <- ts(consumer.eu.in$Sales)
plot(consumer.eu.in.timeser)

w <-1
consumer.eu.in.smoothed <- filter(consumer.eu.in.timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff <- consumer.eu.in.smoothed[w+2] - consumer.eu.in.smoothed[w+1]
for (i in seq(w,1,-1)) {
  consumer.eu.in.smoothed[i] <- consumer.eu.in.smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer.eu.in.timeser)
diff <- consumer.eu.in.smoothed[n-w] - consumer.eu.in.smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer.eu.in.smoothed[i] <- consumer.eu.in.smoothed[i-1] + diff
}

#Plot the smoothed time series
#consumer.apac.in$Month <- seq(1,nrow(consumer.apac.in),1)
timevals_eu.in <- consumer.eu.in$Month
lines(consumer.eu.in.smoothed, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf.eu <- as.data.frame(cbind(timevals_eu.in, as.vector(consumer.eu.in.smoothed)))
colnames(smootheddf.eu) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit.eu <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
               + Month, data=smootheddf.eu)
global_pred.eu <- predict(lmfit.eu, Month=timevals_eu.in)
summary(global_pred.eu)
lines(timevals_eu.in, global_pred.eu, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred.eu <- consumer.eu.in.timeser-global_pred.eu
plot(local_pred.eu, col='red', type = "l")
acf(local_pred.eu)
acf(local_pred.eu, type="partial")
armafit.eu <- auto.arima(local_pred.eu)

tsdiag(armafit.eu)
armafit.eu

#We'll check if the residual series is white noise

resi.eu <- local_pred-fitted(armafit.eu)

adf.test(resi.eu,alternative = "stationary")
kpss.test(resi.eu)

#So the residual series is a white noise

####################################
####################################
##Model Evaluation-Consumer_EU Sales
####################################
####################################

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- consumer.eu.out

timevals_out <-consumer.eu.out$Month
timevals_out <- as.data.frame(timevals_out)
names(timevals_out)[1] <- "Month"
global_pred_out.eu <- predict(lmfit,timevals_out)

fcast.eu <- global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast.eu,outdata[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(consumer.eu$Sales)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(consumer.eu.in.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer.eu.in.timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,consumer.eu.out[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

############################################################################################
#Quantity Forecast
############################################################################################

#######################################
#######################################
##Model Building-Consumer_APAC Quantity
#######################################
#######################################

#Consumer_APAC Quantity forecasting

# consumer.apac <- subset(globalstore,globalstore$MarketSegment=="Consumer_APAC")
# consumer.apac <- aggregate(cbind(Sales,Quantity)~Order.Date,consumer.apac,sum)
# head(consumer.apac)
# 
# consumer.apac$Month <- seq(1,nrow(consumer.apac),1)
# #split data
# consumer.apac.in <- consumer.apac[1:42,]
# consumer.apac.out <- consumer.apac[43:48,]

consumer.in.timeser <- ts(consumer.apac.in$Quantity)
plot(consumer.in.timeser)

w <-1
consumer.apac.in.smoothed <- filter(consumer.in.timeser, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)

#Smoothing left end of the time series

diff <- consumer.apac.in.smoothed[w+2] - consumer.apac.in.smoothed[w+1]
for (i in seq(w,1,-1)) {
  consumer.apac.in.smoothed[i] <- consumer.apac.in.smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer.in.timeser)
diff <- consumer.apac.in.smoothed[n-w] - consumer.apac.in.smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer.apac.in.smoothed[i] <- consumer.apac.in.smoothed[i-1] + diff
}

#Plot the smoothed time series
#consumer.apac.in$Month <- seq(1,nrow(consumer.apac.in),1)
timevals_in <- consumer.apac.in$Month
lines(consumer.apac.in.smoothed, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(consumer.apac.in.smoothed)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- consumer.in.timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#So the residual series is a white noise

##########################################
##########################################
##Model Evaluation-Consumer_APAC Quantity
##########################################
##########################################

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- consumer.apac.out
timevals_out <- as.data.frame(seq(nrow(consumer.apac.in)+1,nrow(consumer.apac.in)+nrow(consumer.apac.out),1))
timevals_out <-consumer.apac.out$Month
timevals_out <- as.data.frame(timevals_out)
names(timevals_out)[1] <- "Month"
global_pred_out <- predict(lmfit,timevals_out)

fcast <- global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,3])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(consumer.apac$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(consumer.in.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer.in.timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,consumer.apac.out[,3])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")


#######################################
#######################################
##Model Building-Consumer_EU Quantity
#######################################
#######################################

#Consuer_EU attribute Quantity forecasting


consumer.eu.in.timeser <- ts(consumer.eu.in$Quantity)
plot(consumer.eu.in.timeser)

w <-1
consumer.eu.in.smoothed <- filter(consumer.eu.in.timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff <- consumer.eu.in.smoothed[w+2] - consumer.eu.in.smoothed[w+1]
for (i in seq(w,1,-1)) {
  consumer.eu.in.smoothed[i] <- consumer.eu.in.smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(consumer.eu.in.timeser)
diff <- consumer.eu.in.smoothed[n-w] - consumer.eu.in.smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  consumer.eu.in.smoothed[i] <- consumer.eu.in.smoothed[i-1] + diff
}

#Plot the smoothed time series
#consumer.apac.in$Month <- seq(1,nrow(consumer.apac.in),1)
timevals_eu.in <- consumer.eu.in$Month
lines(consumer.eu.in.smoothed, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf.eu <- as.data.frame(cbind(timevals_eu.in, as.vector(consumer.eu.in.smoothed)))
colnames(smootheddf.eu) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit.eu <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
               + Month, data=smootheddf.eu)
global_pred.eu <- predict(lmfit.eu, Month=timevals_eu.in)
summary(global_pred.eu)
lines(timevals_eu.in, global_pred.eu, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred.eu <- consumer.eu.in.timeser-global_pred.eu
plot(local_pred.eu, col='red', type = "l")
acf(local_pred.eu)
acf(local_pred.eu, type="partial")
armafit.eu <- auto.arima(local_pred.eu)

tsdiag(armafit.eu)
armafit.eu

#We'll check if the residual series is white noise

resi.eu <- local_pred-fitted(armafit.eu)

adf.test(resi.eu,alternative = "stationary")
kpss.test(resi.eu)

#So the residual series is a white noise

#######################################
#######################################
##Model Evaluation-Consumer_EU Quantity
#######################################
#######################################

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- consumer.eu.out

timevals_out <-consumer.eu.out$Month
timevals_out <- as.data.frame(timevals_out)
names(timevals_out)[1] <- "Month"
global_pred_out.eu <- predict(lmfit.eu,timevals_out)

fcast.eu <- global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast.eu,outdata[,3])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(consumer.eu$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(consumer.eu.in.timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- consumer.eu.in.timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,consumer.eu.out[,3])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

