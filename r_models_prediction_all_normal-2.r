# changes from r_complete_data2.r = using log price instead of price

library(ggplot2)
library(gridExtra)
library(corrplot)
library(randomForest)
library(pROC)
library(caret)
library(Metrics)
library(forecast)
library(rpart)
library(rpart.plot)
library(rattle)
library(glmnet)
library(xgboost)
library(caretEnsemble)
library(gbm)


#set the working directory
setwd("C:/Users/evanf/Documents/Smurfit/Capstone/August 2018")

# my data is saved as datax
datax <- read.csv("houseprice_complete_data_numeric_county4.csv")
colnames(datax)[1] <- "link"
names(datax)

# Just use the complete columns for now
datax <- subset(datax, select = - c(link, address, agent, facilities, features, description, overview, date, views, first_date, first_price))
datax <- subset(datax, select = - c(newAgent, agent1, agent2, town1, town2, town3, town))

# check that it changed
names(datax)

# check for any missing values in the data
apply(is.na(datax), 2, sum)

# check for any duplicate values
sum (is.na(duplicated(datax)))

# attach the data
attach(datax)
# detach(datax)

####DATA EXPLORATION####
# table1 <- table(datax$county)
# table1
table(datax$dwelling_type)
# table(datax$town)
# table(datax$bedrooms)
# table(datax$bathrooms)
# table(datax$price)
# table(datax$north_south)
table3 <- table(datax$newTown)
table4 <- table(datax$ber)
table3
# table5 <- table(datax$county, datax$price)
# table5

table1 <- tapply(datax$price, datax$newCounty, mean)
table2 <- tapply(datax$price_per_m2, datax$newCounty, mean)

# write.csv(table1, file = "avg_price.csv")
# write.csv(table2, file = "avg_price_m2.csv")
write.csv(table3, file = "town_frequency.csv")
# write.csv(table4, file = "characters4.csv")

table1

# Change columns to Numeric
datax$price = as.numeric(as.character(datax$price))
datax$price_per_m2 = as.numeric(as.character(datax$price_per_m2))
datax$longitude = as.numeric(as.character(datax$longitude))
datax$latitude = as.numeric(as.character(datax$latitude))
datax$bedrooms = as.numeric(as.character(datax$bedrooms))
datax$bathrooms = as.numeric(as.character(datax$bathrooms))
datax$area = as.numeric(as.character(datax$area))
datax$nearst_luas = as.numeric(as.character(datax$nearst_luas))
datax$Dist_to_coast = as.numeric(as.character(datax$Dist_to_coast))
datax$Dist_to_city = as.numeric(as.character(datax$Dist_to_city))
datax$Dist_to_dart = as.numeric(as.character(datax$Dist_to_dart))
datax$Dist_to_busstop = as.numeric(as.character(datax$Dist_to_busstop))
# datax$text1 = as.numeric(as.character(datax$text1))
# datax$text2 = as.numeric(as.character(datax$text2))
# datax$text3 = as.numeric(as.character(datax$text3))
# datax$text4 = as.numeric(as.character(datax$text4))
# datax$text5 = as.numeric(as.character(datax$text5))
# datax$text6 = as.numeric(as.character(datax$text6))
datax$text7 = as.numeric(as.character(datax$text7))
datax$text8 = as.numeric(as.character(datax$text8))
datax$text9 = as.numeric(as.character(datax$text9))
datax$text10 = as.numeric(as.character(datax$text10))
datax$text11 = as.numeric(as.character(datax$text11))
datax$text12 = as.numeric(as.character(datax$text12))
datax$text13 = as.numeric(as.character(datax$text13))
datax$text14 = as.numeric(as.character(datax$text14))
# datax$text15 = as.numeric(as.character(datax$text15))
# datax$text16 = as.numeric(as.character(datax$text16))
datax$text17 = as.numeric(as.character(datax$text17))
# datax$text18 = as.numeric(as.character(datax$text18))
datax$text19 = as.numeric(as.character(datax$text19))
datax$text20 = as.numeric(as.character(datax$text20))
# datax$text21 = as.numeric(as.character(datax$text21))
datax$text22 = as.numeric(as.character(datax$text22))
datax$text23 = as.numeric(as.character(datax$text23))

# Change columns to factor
datax$dwelling_type <- as.factor(datax$dwelling_type)
datax$county <- as.factor(datax$county)
# datax$town <- as.factor(datax$town)
# datax$town1 <- as.factor(datax$town1)
# datax$town2 <- as.factor(datax$town2)
# datax$town3 <- as.factor(datax$town3)
datax$newTown <- as.factor(datax$newTown)
datax$ber <- as.factor(datax$ber)
datax$ber_new <- as.factor(datax$ber_new)
datax$newCounty <- as.factor(datax$newCounty)
datax$Luas_Factors <- as.factor(datax$Luas_Factors)
datax$Dart_Factors <- as.factor(datax$Dart_Factors)
datax$Bus_Factors <- as.factor(datax$Bus_Factors)

# Create new columns for logs of price and price per metre squared
datax$log_price_m2 <- log(datax$price_per_m2)
datax$log_price <- log(datax$price)
datax$bath_sq <- (datax$bathrooms_centre)^2

# round the distances for analysis
datax$coast_round <- round(datax$Dist_to_coast, digits = 0)
datax$city_round <- round(datax$Dist_to_city, digits = 0)
datax$dart_round <- round(datax$Dist_to_dart, digits = 0)
datax$luas_round <- round(datax$nearst_luas, digits = 0)
datax$bus_round <- round(datax$Dist_to_busstop, digits = 1)

datax$coast_round

#### BIT OF ANALYSIS ON ASKING PRICE v LOG PRICE etc ####
# Draw a higtogram to figure out the distribution of Asking Price

options(scipen=10000)
graph1 <- ggplot(datax, aes(x = price, fill = ..count..)) +
  geom_histogram(binwidth = 20000) +
  ggtitle("Histogram of Asking Price") +
  ylab("Count of Houses") +
  xlab("Asking Price") + 
  theme(plot.title = element_text(hjust = 0.5))

# Draw a higtogram to figure out the distribution of Log Asking Price
options(scipen=10000)
graph2 <- ggplot(datax, aes(x = log_price, fill = ..count..)) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Histogram of Log Asking Price") +
  ylab("Count of Houses") +
  xlab("Log Asking Price") + 
  theme(plot.title = element_text(hjust = 0.5))


# Draw a higtogram to figure out the distribution of Price Per Metre Squared
options(scipen=10000)
graph3 <- ggplot(datax, aes(x = price_per_m2, fill = ..count..)) +
  geom_histogram(binwidth = 100) +
  ggtitle("Histogram of Price Per Metre Squared") +
  ylab("Count of Houses") +
  xlab("Price Per Metre Squared") + 
  theme(plot.title = element_text(hjust = 0.5))


# Draw a higtogram to figure out the distribution of Log Price Per Metre Squared
options(scipen=10000)
graph4 <- ggplot(datax, aes(x = log_price_m2, fill = ..count..)) +
  geom_histogram(binwidth = 0.05) +
  ggtitle("Histogram of Log Price Per Metre Squared") +
  ylab("Count of Houses") +
  xlab("Log Price Per Metre Squared") + 
  theme(plot.title = element_text(hjust = 0.5))


graph1
graph2
graph3
graph4
grid1 <- grid.arrange(graph1, graph2, ncol = 2)
grid2 <- grid.arrange(graph3, graph4, ncol = 2)


##################################################

datad <- datax[datax$price <= 1500000,]

# historgram of housing price by Dwelling Type 
ggplot(datad, aes(price)) +
  geom_histogram(aes(fill = dwelling_type), position = position_stack(reverse = TRUE), binwidth = 20000) +
  coord_flip() + ggtitle("Histogram of Asking Price (< ???2.5m) for Dwelling Types") +
  ylab("Count") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5),legend.position=c(0.8,0.75), legend.background = element_rect(fill="grey90",
                                                                                                            size=0.5, linetype="solid", 
                                                                                                            colour ="black"))


# historgram of housing price by Number of Bedrooms
ggplot(datad, aes(x = price,fill = as.factor(bedrooms))) +
  geom_histogram(position = "stack", binwidth = 20000) +
  ggtitle("Histogram of Asking Price by Number of Bedrooms") +
  ylab("Count") +
  xlab("Asking Price") + 
  scale_fill_discrete(name="Bedrooms")+
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.9,0.8), legend.background = element_rect(fill="grey90",
                                                                                                             size=0.5, linetype="solid", 
                                                                                                             colour ="black"))


# historgram of housing price by North_South
ggplot(datad, aes(x = price,fill = as.factor(north_south))) +
  geom_histogram(position = "stack", binwidth = 20000) +
  ggtitle("Histogram of Asking Price by North or South Dublin") +
  ylab("Count") +
  xlab("Asking Price") + 
  scale_fill_discrete(name="Bedrooms")+
  theme(plot.title = element_text(hjust = 0.5), legend.position=c(0.9,0.9), legend.background = element_rect(fill="grey90",
                                                                                                             size=0.5, linetype="solid", 
                                                                                                             colour ="black"))


########################################################
#### Check which dependent variable is best to use #####
########################################################

model2 <- lm(datax$price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber)
summary(model2)

# 
model3 <- lm(datax$log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber)
summary(model3)

# 
model4 <- lm(datax$price_per_m2 ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber)
summary(model4)

# 
model5 <- lm(datax$log_price_m2 ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber)
summary(model5)

 
# price_hat_model1 <- exp(predict(model1,newdata=datax))
price_hat_model2 <- predict(model2,newdata=datax)
price_hat_model3 <- exp(predict(model3,newdata=datax))
price_hat_model4 <- predict(model4,newdata=datax)*area
price_hat_model5 <- exp(predict(model5,newdata=datax))*area
# price_hat_model6 <- exp(predict(model6,newdata=datax))
# 
#output <- (cbind("ID"=datax$longitude,"Orginal Price"=datax$price,"Model1 Predict"=price_hat_model1))
output <- (cbind("ID"=datax$longitude,"Orginal Price"=datax$price,"Model2 Predict"=price_hat_model2,
                 "Model3 Predict"=price_hat_model3, "Model4 Predict"=price_hat_model4,
                 "Model5 Predict"=price_hat_model5))
write.csv(output, file = "models_price_v_predicted_y.csv", row.names=FALSE)
 
#####################################################################
#### Check if new changes to county have made a positive change #####

model21 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + county + newTown + ber)
summary(model21)

# change to newCounty
model22 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + newTown + ber)
summary(model22)

model23 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + county)
summary(model23)

# change to newCounty
model24 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty)
summary(model24)


# price_hat_model1 <- exp(predict(model1,newdata=datax))
price_hat_model21 <- exp(predict(model21,newdata=datax))
price_hat_model22 <- exp(predict(model22,newdata=datax))
price_hat_model23 <- exp(predict(model23,newdata=datax))
price_hat_model24 <- exp(predict(model24,newdata=datax))
 

output <- (cbind("ID"=datax$longitude,"Orginal Price"=datax$price,"Model21 Predict"=price_hat_model21,
                 "Model22 Predict"=price_hat_model22, "Model23 Predict"=price_hat_model23, "Model24 Predict"=price_hat_model24))
write.csv(output, file = "models_price_v_predicted_county.csv", row.names=FALSE)

prediction21 <- predict(model21, datax, type="response")
rmse(datax$log_price, prediction21)
prediction22 <- predict(model22, datax, type="response")
rmse(datax$log_price, prediction22)
prediction23 <- predict(model23, datax, type="response")
rmse(datax$log_price, prediction23)
prediction24 <- predict(model24, datax, type="response")
rmse(datax$log_price, prediction24)


####################################################################################
#### Check if new changes to lat & lon coordinates have made a positive change #####

# without any coordinate parameters
model51 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty)
summary(model51)

# add in normal lat & lon
model52 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + latitude + longitude)
summary(model52)

# use th 50 separate lat & lon values instead
model53 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty
              + Lat.Co..Dublin.North + Lat.Co..Dublin.South + Lat.Co..Dublin.West + Lat.Dublin.1 + Lat.Dublin.10 + Lat.Dublin.11
              + Lat.Dublin.12 + Lat.Dublin.13 + Lat.Dublin.14 + Lat.Dublin.15 + Lat.Dublin.16 + Lat.Dublin.17 + Lat.Dublin.18
              + Lat.Dublin.2 + Lat.Dublin.20 + Lat.Dublin.22 + Lat.Dublin.24 + Lat.Dublin.3 + Lat.Dublin.4 + Lat.Dublin.5
              + Lat.Dublin.6 + Lat.Dublin.6W + Lat.Dublin.7 + Lat.Dublin.8 + Lat.Dublin.9
              + Long.Co..Dublin.North + Long.Co..Dublin.South + Long.Co..Dublin.West + Long.Dublin.1 + Long.Dublin.10 + Long.Dublin.11
              + Long.Dublin.12 + Long.Dublin.13 + Long.Dublin.14 + Long.Dublin.15 + Long.Dublin.16 + Long.Dublin.17 + Long.Dublin.18
              + Long.Dublin.2 + Long.Dublin.20 + Long.Dublin.22 + Long.Dublin.24 + Long.Dublin.3 + Long.Dublin.4 + Long.Dublin.5
              + Long.Dublin.6 + Long.Dublin.6W + Long.Dublin.7 + Long.Dublin.8 + Long.Dublin.9)
summary(model53)

# add town to model 51
model54 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + newTown)
summary(model54)

# add town to model 52
model55 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + newTown + latitude + longitude)
summary(model55)

# add town to model 53
model56 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + newTown
              + Lat.Co..Dublin.North + Lat.Co..Dublin.South + Lat.Co..Dublin.West + Lat.Dublin.1 + Lat.Dublin.10 + Lat.Dublin.11
              + Lat.Dublin.12 + Lat.Dublin.13 + Lat.Dublin.14 + Lat.Dublin.15 + Lat.Dublin.16 + Lat.Dublin.17 + Lat.Dublin.18
              + Lat.Dublin.2 + Lat.Dublin.20 + Lat.Dublin.22 + Lat.Dublin.24 + Lat.Dublin.3 + Lat.Dublin.4 + Lat.Dublin.5
              + Lat.Dublin.6 + Lat.Dublin.6W + Lat.Dublin.7 + Lat.Dublin.8 + Lat.Dublin.9
              + Long.Co..Dublin.North + Long.Co..Dublin.South + Long.Co..Dublin.West + Long.Dublin.1 + Long.Dublin.10 + Long.Dublin.11
              + Long.Dublin.12 + Long.Dublin.13 + Long.Dublin.14 + Long.Dublin.15 + Long.Dublin.16 + Long.Dublin.17 + Long.Dublin.18
              + Long.Dublin.2 + Long.Dublin.20 + Long.Dublin.22 + Long.Dublin.24 + Long.Dublin.3 + Long.Dublin.4 + Long.Dublin.5
              + Long.Dublin.6 + Long.Dublin.6W + Long.Dublin.7 + Long.Dublin.8 + Long.Dublin.9)
summary(model56)


# Check RMSE values for above models
prediction51 <- predict(model51, datax, type="response")
rmse(datax$log_price, prediction51)
# 0.236312
prediction52 <- predict(model52, datax, type="response")
rmse(datax$log_price, prediction52)
# 0.2218311
prediction53 <- predict(model53, datax, type="response")
rmse(datax$log_price, prediction53)
# 0.1984393
prediction54 <- predict(model54, datax, type="response")
rmse(datax$log_price, prediction54)
# 0.1777698
prediction55 <- predict(model55, datax, type="response")
rmse(datax$log_price, prediction55)
# 0.1770397
prediction56 <- predict(model56, datax, type="response")
rmse(datax$log_price, prediction56)
# 0.1703569





#####################################################################
#### Check if new changes to distances have made a positive change #####

# without any distance parameters
model41 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty)
summary(model41)

# add in distance to coast and city
model42 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + Dist_to_coast + Dist_to_city)
summary(model42)

# add in distance to transport by numeric values
model43 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + Dist_to_coast + Dist_to_city
              + nearst_luas + Dist_to_dart + Dist_to_busstop)
summary(model43)

# add in distance to transport by factors
model44 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty + Dist_to_coast + Dist_to_city
              + Luas_Factors + Dart_Factors + Bus_Factors)
summary(model44)

# remove coast and city for below 2 models
model45 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty
              + nearst_luas + Dist_to_dart + Dist_to_busstop)
summary(model45)

# add in distance to transport by factors
model46 <- lm(datax$log_price ~ bedrooms + bathrooms + area + dwelling_type + newCounty
              + Luas_Factors + Dart_Factors + Bus_Factors)
summary(model46)


# Check RMSE values for above models
prediction41 <- predict(model41, datax, type="response")
rmse(datax$log_price, prediction41)
# 0.236312
prediction42 <- predict(model42, datax, type="response")
rmse(datax$log_price, prediction42)
# 0.2193726
prediction43 <- predict(model43, datax, type="response")
rmse(datax$log_price, prediction43)
# 0.2192287
prediction44 <- predict(model44, datax, type="response")
rmse(datax$log_price, prediction44)
# 0.2175744
prediction45 <- predict(model45, datax, type="response")
rmse(datax$log_price, prediction45)
# 0.2221335
prediction46 <- predict(model46, datax, type="response")
rmse(datax$log_price, prediction46)
# 0.233845


# price_hat_model1 <- exp(predict(model1,newdata=datax))
price_hat_model41 <- exp(predict(model41,newdata=datax))
price_hat_model42 <- exp(predict(model42,newdata=datax))


output <- (cbind("ID"=datax$longitude,"Orginal Price"=datax$price,"Model41 Predict"=price_hat_model41,
                 "Model42 Predict"=price_hat_model42))
write.csv(output, file = "models_price_v_predicted_distances.csv", row.names=FALSE)

########################################################
####### Asking Price v Sales Price Analysis ############

datasales <- read.csv("ppr_analysis.csv")
names(datasales)
colnames(datasales)[1] <- "ID"

model31 <- lm(datasales$Sale_Price ~ datasales$Asking_Price)
summary(model31)

## Intercept = 2.515e+0.4 (25,150), Asking Price * 9.651e-01 (0.9651)

#########################################################
 
# ### plot the asking prices vs predicted price ###
# datay <- read.csv("models_price_v_predicted.csv")
# 
# # column 8 is for model 6 which we use here as the predicted price
# names(datay)
# colnames(datay)[1] <- "id"
# colnames(datay)[2] <- "askingprice"
# colnames(datay)[8] <- "predictedprice"
# 
# datay <- subset(datay, select = c(id, askingprice, predictedprice))
# datay <- datay[datay$askingprice < 2500000,]
# # datay1 <- subset(datay, select = c(id, askingprice))
# # datay2 <- subset(datay, select = c(id, predictedprice))
# 
# library(ggplot2)
# ggplot(datay, aes(id, askingprice)) + geom_jitter() +ggtitle("AskingPrice") + theme_light() + theme(plot.title = element_text(hjust = 0.5))
# ggplot(datay, aes(id, predictedprice), color = red) + geom_jitter() +ggtitle("PredictedPrice") + theme_light() + theme(plot.title = element_text(hjust = 0.5))
# 
# ggplot(datay, aes(askingprice, predictedprice), color = 'black') + geom_jitter() + ggtitle("Asking v Predicted Price (Properties < ???2.5m)") + theme_light() + theme(plot.title = element_text(hjust = 0.5)) + geom_abline(slope = 1, intercept = 0, color = 'red') + geom_abline(slope = 1.05, intercept = 0, color = 'blue') + geom_abline(slope = 0.95, intercept = 0, color = 'blue')



#################################################

names(datax)
datay <- datax[datax$price <= 1200000,]

datay <- subset(datay, select = - c(bedsFact, bathsFact, bedrooms, bathrooms, price_per_m2, log_price_m2, north_south, county, latitude,
                                    longitude, Luas_Factors, Dart_Factors, Bus_Factors, bedsFact, bathsFact))
datam <- datay
head(datam)
# datam <- datam.sample(frac = 1)

names(datam)

##### Dashboard #####
#### Properties < ???1.2m #####

datam <- datam[c(1:3, 5:27, 269:270)]
names(datam)
datam <- datam[c(1:7, 13:28)]

mlx <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown
                                + ber + text7 + text8 + text9 + text10 + text11 + text12 + text13 + text14 + text16 + text17
                                + text19 + text20 + text22 + text23, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mlx)
prediction_excel1 <- predict(mlx, datam, type="response")
rmse(datam$log_price, prediction_excel1)
#[1] 0.1528069

# Predictions
prediction_model1_excel <- exp(prediction_excel1)

output_excel1 <- (cbind("Orginal Price"=exp(datam$log_price), "Model1"=prediction_model1_excel))
write.csv(output_excel1, file = "models_price_v_predicted_excel1.csv", row.names=FALSE)



##### Dashboard Inputs #####
##### All properties #####

names(datax)
datay <- datax

datay <- subset(datay, select = - c(bedsFact, bathsFact, bedrooms, bathrooms, price_per_m2, log_price_m2, north_south, county, latitude,
                                    longitude, Luas_Factors, Dart_Factors, Bus_Factors, bedsFact, bathsFact))
datam <- datay


names(datam)
datam <- datam[c(2:3, 5:21, 23:27, 219:221)]
names(datam)
datam <- datam[c(1:6, 12:27)]

mly <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown
         + ber_new + text7 + text8 + text9 + text10 + text11 + text12 + text13 + text14 + text17
         + text19 + text20 + text22 + text23, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mly)
prediction_excel2 <- predict(mly, datam, type="response")
rmse(datam$log_price, prediction_excel2)
#[1] 0.1723541

confint(mly)

# Predictions
prediction_model2_excel <- exp(prediction_excel2)

output_excel <- (cbind("Orginal Price"=exp(datam$log_price), "Model2"=prediction_model2_excel))
write.csv(output_excel, file = "models_price_v_predicted_excel2.csv", row.names=FALSE)

# lincomb = caret::findLinearCombos(datam)
# lapply(lincomb$linearCombos, function(x) colnames(df)[x])


###### End of analysis for Excel ########


##### Effect of Postcode #####
#All properties
datay <- datax
datay <- subset(datay, select = - c(bedsFact, bathsFact, bedrooms, bathrooms, price_per_m2, log_price_m2, north_south, county, latitude,
                                    longitude, Luas_Factors, Dart_Factors, Bus_Factors, bedsFact, bathsFact))
datam <- datay
datam <- datam[c(1:3, 5:26, 200, 209:210)]
names(datam)
datam <- datam[c(1:7, 13:28)]
datam <- datam[c(2:24)]


mlp <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mlp)

confint(mlp)

##### Effect of Town #####
# All properties
mlt <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newTown, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mlt)

##### Effect of BER #####
# All properties
mlb <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + ber, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mlb)

##### Effect of BER New #####
# All properties
mlb2 <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber_new, data = datam)
getOption("max.print")
options(max.print = 2000)
summary(mlb2)
confint(mlb2)

##### Quadratic Effect for Bathrooms#####

mlr4 <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + dwelling_type + newCounty + newTown + ber, data = datam)
summary(mlr4)

mlr5 <-lm(formula = log_price ~ area + bedrooms_centre + bathrooms_centre + bath_sq + dwelling_type + newCounty + newTown + ber, data = datam)
summary(mlr5)

confint(mlr5)

ci1 <- predict(mlr4, datam, interval = "confidence")
write.csv(ci1, file = "confidence_intervals.csv", row.names=FALSE)

##############################################################
##############################################################
##############################################################
##############################################################

#####Spatial Variables Analysis####
###plot price vs dist to coast
plot(Dist_to_coast, datax$log_price, main = "Dist to Coast vs. Log Asking Price", xlab = "Dist to Coast", ylab = "Log Asking Price")
abline(lm(log_price ~ Dist_to_coast), col = "red", lwd = 3)

###plot price vs dist to coast (rounded)
line1 <- ggplot(datax, aes(x=coast_round, y=log_price)) + stat_summary(fun.y="mean", geom="line", color = "red") + xlab("Distance to Coast") + ggtitle("Log Price by Distance to Coast") + theme(plot.title = element_text(hjust = 0.5))
line11 <- ggplot(datax, aes(x=coast_round, y=log_price_m2)) + stat_summary(fun.y="mean", geom="line", color = "red") + xlab("Distance to Coast") + ggtitle("Log Price per Metre Squared by Distance to Coast") + theme(plot.title = element_text(hjust = 0.5))

###plot price vs dist to city
plot(Dist_to_city, datax$log_price, main = "Dist to City vs. Log Asking Price", xlab = "Dist to City", ylab = "Log Asking Price")
abline(lm(log_price ~ Dist_to_city), col = "red", lwd = 3)

###plot price vs dist to city (rounded)
line2 <- ggplot(datax, aes(x=city_round, y=log_price)) + stat_summary(fun.y="mean", geom="line", color = "blue") + xlab("Distance to City") + ggtitle("Log Price by Distance to City") + theme(plot.title = element_text(hjust = 0.5))
line12 <- ggplot(datax, aes(x=city_round, y=log_price_m2)) + stat_summary(fun.y="mean", geom="line", color = "blue") + xlab("Distance to City") + ggtitle("Log Price per Metre Squared by Distance to City") + theme(plot.title = element_text(hjust = 0.5))

###plot price vs dist to Luas
plot(nearst_luas, datax$log_price, main = "Dist to Luas vs. Log Asking Price", xlab = "Dist to Luas", ylab = "Log Asking Price")
abline(lm(log_price ~ nearst_luas), col = "red", lwd = 3)

###plot price vs dist to Luas (rounded)
line3 <- ggplot(datax, aes(x=luas_round, y=log_price)) + stat_summary(fun.y="mean", geom="line", color = "green") + xlab("Distance to Luas") + ggtitle("Log Price by Distance to Luas") + theme(plot.title = element_text(hjust = 0.5))
line13 <- ggplot(datax, aes(x=luas_round, y=log_price_m2)) + stat_summary(fun.y="mean", geom="line", color = "green") + xlab("Distance to Luas") + ggtitle("Log Price Per Metre Squared by Distance to Luas") + theme(plot.title = element_text(hjust = 0.5))

###plot price vs dist to DART
plot(Dist_to_dart, datax$log_price, main = "Dist to DART vs. Log Asking Price", xlab = "Dist to DART", ylab = "Log Asking Price")
abline(lm(log_price ~ Dist_to_dart), col = "red", lwd = 3)

###plot price vs dist to DART (rounded)
line4 <- ggplot(datax, aes(x=dart_round, y=log_price)) + stat_summary(fun.y="mean", geom="line", color = "orange") + xlab("Distance to DART") + ggtitle("Log Price by Distance to DART") + theme(plot.title = element_text(hjust = 0.5))
line14<- ggplot(datax, aes(x=dart_round, y=log_price_m2)) + stat_summary(fun.y="mean", geom="line", color = "orange") + xlab("Distance to DART") + ggtitle("Log Price Per Metre Squared by Distance to DART") + theme(plot.title = element_text(hjust = 0.5))

###plot price vs dist to bus
plot(Dist_to_busstop, datax$log_price, main = "Dist to bus vs. Log Asking Price", xlab = "Dist to Bus", ylab = "Log Asking Price")
abline(lm(log_price ~ Dist_to_busstop), col = "red", lwd = 3)

###plot price vs dist to Bus (rounded)
line5 <- ggplot(datax, aes(x=bus_round, y=log_price)) + stat_summary(fun.y="mean", geom="line", color = "black") + xlab("Distance to Bus") + ggtitle("Log Price by Distance to Bus") + theme(plot.title = element_text(hjust = 0.5))
line15 <- ggplot(datax, aes(x=bus_round, y=log_price_m2)) + stat_summary(fun.y="mean", geom="line", color = "black") + xlab("Distance to Bus") + ggtitle("Log Price per Metre Squared by Distance to Bus") + theme(plot.title = element_text(hjust = 0.5))

library(grid)
library(gridExtra)
grid10 <- grid.arrange(line1, line2, line3, line4, line5, ncol = 2, top = textGrob("Distribution of Log Price By Spatial Variables",gp=gpar(fontsize=20,font=3)))
grid11 <- grid.arrange(line11, line12, line13, line14, line15, ncol = 2, top = textGrob("Distribution of Log Price Per Metre Squared By Spatial Variables",gp=gpar(fontsize=20,font=3)))


##############################################################
##############################################################
##############################################################
##############################################################

datay <- datax

datay <- subset(datay, select = - c(bedsFact, bathsFact, bedrooms, bathrooms, price_per_m2, log_price, north_south, county,
                                    Luas_Factors, Dart_Factors, Bus_Factors, bedsFact, bathsFact))
datam <- datay
datam <- subset(datam, select = - c(Side...North, Side...South))

names(datam)

# Partition the data in to a training set (=70%) and a test set (=30%)
split_data = function(datax, train, test) {
  dataPartition = sample(2, nrow(datax), replace = TRUE, prob = c(train, test))
  new_train <<- datax[dataPartition==1, ]
  validation <<- datax[dataPartition==2, ]
}

split_data(datam, 0.7,0.3)
new_train
validation

names(new_train)


#### NON SPATIAL ####
#### USE THIS FOR REGRESSION ####
new_train1 <- new_train[c(2:3, 5:8, 16:28, 200, 210, 216)]
validation1 <- validation[c(2:3, 5:8, 16:28, 200, 210, 216)]
names(new_train1)


#### USE THIS FOR MACHINE LEARNING & NEURAL NETWORK ####
new_train2 <- subset(new_train, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, longitude,
                                             latitude, nearst_luas, Dist_to_coast, Dist_to_city, Dist_to_dart, Dist_to_busstop, ber_new,
                                             coast_round, city_round, dart_round, luas_round, bus_round, log_price_m2))
validation2 <- subset(validation, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, longitude,
                                               latitude, nearst_luas, Dist_to_coast, Dist_to_city, Dist_to_dart, Dist_to_busstop, ber_new,
                                               coast_round, city_round, dart_round, luas_round, bus_round, log_price_m2))
names(new_train2)

##(a).MLR
#First include all predictor variables to see what will happen
mlr <-lm(formula = log_price ~ bedrooms_centre + bathrooms_centre + bath_sq + area + dwelling_type + newCounty + ber_new , data = new_train1)
getOption("max.print")
options(max.print = 1000)
summary(mlr)
prediction1 <- predict(mlr,validation1, type="response")
rmse(validation1$log_price, prediction1)
#[1]
# calc.relimp(mlr, type = c("lmg"), rela = TRUE, rank = TRUE)

mlx <-lm(formula = log_price ~ . - log_price, data = new_train1)
getOption("max.print")
options(max.print = 1000)
summary(mlx)
prediction2 <- predict(mlx,validation1, type="response")
rmse(validation1$log_price, prediction2)
#[1]
varImp(mlx)

## (3) Stepwise Linear Regression
Fitstart <- lm(formula = new_train1$log_price ~ 1,data = new_train1)
FitAll <- lm(log_price ~ . ,data = new_train1)
formula(FitAll)
step(Fitstart, direction = "both",scope = formula(FitAll))

mlr <- lm(formula = new_train1$log_price ~ area + newTown + dwelling_type + 
            bedrooms_centre + newCounty + text9 + ber_new + bath_sq + 
            bathrooms_centre + text10 + text22 + text14 + text20 + text13 + 
            text19, data = new_train1)
getOption("max.print")
options(max.print = 1000)
summary(mlr)
prediction3 <- predict(mlr,validation1, type="response")
rmse(validation1$log_price, prediction3)
#[1]
# Variable Importance
install.packages("relaimpo")
library(relaimpo)
varImp(mlr)

## (4) Decision Trees
myformula <- log_price ~ .
modfit <- rpart(myformula, method="anova" , data = new_train1)
prediction4 <- predict(modfit,newdata = validation1)
rmse(validation1$log_price,prediction4)
# [1]


## (5) Random Forest
rf <- randomForest(log_price ~ ., data = new_train2, ntree=1000, proximity=TRUE)
varImpPlot(rf)
prediction5 <- predict(rf, newdata = validation2)
rmse(validation2$log_price, prediction5)
#[1]


## (6) Regularized Regression(Lasso)
names(new_train2)
all_predictors <- subset(new_train2,select = -c(log_price))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(new_train2,select=names(num_classes))
#corrplot(cor(num_vars),method="number")
#corrplot(cor(num_vars),method="circle")
#Building model
set.seed(325)

lasso <-cv.glmnet(as.matrix(new_train2[, -197]), new_train2[, 197])
prediction6 <- predict(lasso, newx = as.matrix(validation2[, - 197]), s = "lambda.min")
rmse(validation2$log_price,prediction6)
#[1]


## (7) Gradient Boosting model(GBM)
set.seed(315)
cv.ctrl_gbm <- trainControl(method="repeatedcv",number=5,repeats = 5)
gbm<- train(log_price ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = new_train2,verbose = FALSE)
varImp(gbm)
prediction7 <- predict(gbm,newdata = validation2)
rmse(validation2$log_price,prediction7)
#[1]


## (8) XGBOOST(Extreme Gradient Boosting)
# preparing matrix 
dtrain <- xgb.DMatrix(data = as.matrix(new_train2[,-197]),label = as.matrix(new_train2$log_price))
dtest <- xgb.DMatrix(data = as.matrix(validation2[,-197]),label=as.matrix(validation2$log_price))
#Building model
set.seed(311)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 
prediction8 <- predict(xgb,newdata = dtest)
rmse(validation2$log_price,prediction8)
#[1]

## (x) Simple Average RMSE of Random Forest + GBM + XGBoost (Top3 performing models)
rmse(validation2$log_price, (prediction5 + prediction7 + prediction8)/3)
#[1]

## (x) Weighted Average RMSE of Lasso+GBM+XGBoost
rmse(validation2$log_price, (0.1 *prediction5 + 0.3 *prediction7 + 0.6 *prediction8))
#[1] 

## (9) Ensemble method
my_control <- trainControl(method="boot",number=5,savePredictions="final")
set.seed(321)
model_list <- caretList(
  log_price ~ ., data=new_train2,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                          shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                                  eta =0.01,gamma = 0.0468,subsample=0.769,
                                                                  colsample_bytree =0.283))
  )
)
modelCor(resamples(model_list))




##Simple Blending
set.seed(333455)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
greedy_ensemble
varImp(greedy_ensemble)
summary(greedy_ensemble)
prediction9 <- predict(greedy_ensemble,newdata = validation2)
rmse(validation2$log_price,prediction9)
# [1]

# (10) Using a "meta-model"
set.seed(317)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
                          trControl=trainControl(method="boot",number=5,savePredictions="final"))
prediction10 <- predict(rf_ensemble,newdata = validation2)
rmse(validation2$log_price,prediction10)
# [1]


## (11) Simple Average RMSE of XGBoost+Ensemble+Meta-Model(Top3 performance models)
prediction11 <- (prediction8 + prediction9 + prediction10)/3
rmse(validation2$log_price, prediction11)
#[1]

## (12) Neural Network Non Spatial
datam <- subset(datam, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, longitude,
                                    latitude, nearst_luas, Dist_to_coast, Dist_to_city, Dist_to_dart, Dist_to_busstop, ber_new))

UDF <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}

train_o <- datam
train <- as.data.frame(apply(train_o, 2, UDF))
index <- sample(nrow (train), round(0.70 * nrow(train)))

train.wp <- train[index,]
test.wp <- train[-index,]


##
#procValues <- preProcess(datam, method = c("center", "scale"))
#scaledTraindata <-  predict(procValues, datatrain )
#scaledTestdata <-  predict(procValues, datatest)

library(neuralnet)
allVars <- colnames(train)
predictorVars <- allVars[!allVars%in%"log_price"]
predictorVars <- paste(predictorVars, collapse = "+")
form = as.formula(paste("log_price~", predictorVars, collapse = "+"))

# Prediction Model
nn_model <- neuralnet(formula = form, train.wp, hidden = c(2,1), linear.output = TRUE)

test.wp_1 <- subset(test.wp, select = -c(log_price))

prediction1 <- compute(nn_model, test.wp_1)
str(prediction1)
# UDF: Convert the scaled values to original 
UDF_2 <- function(prediction) {
  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
}

Prediction <-  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Prediction <- exp(Prediction)

Actual <- test.wp$log_price * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Actual <- exp(Actual)

table(Actual, Prediction)

submit.df <- data.frame(Real = Actual, PredictPrice = Prediction)
write.csv(submit.df, file = "Prediction_neural.csv", row.names = FALSE)



# RMSE results
rmse(validation2$log_price, prediction3) # [1] 0.179344382
rmse(validation2$log_price, prediction4) # [1] 0.2773309582
rmse(validation2$log_price, prediction5) # [1] 0.2060180447
rmse(validation2$log_price, prediction6) # [1] 0.1777638014
rmse(validation2$log_price, prediction7) # [1] 0.1771044259
rmse(validation2$log_price, prediction8) # [1] 0.1672389116
rmse(validation2$log_price, prediction9) # [1] 0.1660828578
rmse(validation2$log_price, prediction10) # [1] 0.1732218506
rmse(validation2$log_price, prediction11) # [1] 0.1650871085


# Predictions
prediction_model1 <- exp(prediction3)
prediction_model2 <- exp(prediction4)
prediction_model3 <- exp(prediction5)
prediction_model4 <- exp(prediction6)
prediction_model5 <- exp(prediction7)
prediction_model6 <- exp(prediction8)
prediction_model7 <- exp(prediction9)
prediction_model8 <- exp(prediction10)
prediction_model9 <- exp(prediction11)

output <- (cbind("ID"=validation$longitude,"Orginal Price"=exp(validation$log_price),"Model1"=prediction_model1,
                 "Model2"=prediction_model2, "Model3"=prediction_model3, "Model4"=prediction_model4, "Model5"=prediction_model5,
                 "Model6"=prediction_model6, "Model7"=prediction_model7, "Model8"=prediction_model8,
                 "Model9"=prediction_model9))
write.csv(output, file = "models_price_v_predicted_logp_normal.csv", row.names=FALSE)



#########################################
########## SPATIAL - Log Price ##############
#### USE THIS FOR REGRESSION ####
names(new_train)
new_train$log_price <- log(new_train$price)
validation$log_price <- log(validation$price)

train11 <- new_train[c(2:3, 5:28, 200, 210, 216)]
test11 <- validation[c(2:3, 5:28, 200, 210, 216)]
names(train11)


#### USE THIS FOR MACHINE LEARNING & NEURAL NETWORK ####
# datam <- subset(datay, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new))
train12 <- subset(new_train, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, log_price_m2, coast_round, city_round, dart_round, luas_round, bus_round))
test12 <- subset(validation, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, log_price_m2, coast_round, city_round, dart_round, luas_round, bus_round))
names(train12)

##(a).MLR
#First include all predictor variables to see what will happen
mlx <-lm(formula = log_price ~ . - newCounty, data = train11)
getOption("max.print")
options(max.print = 1000)
summary(mlx)
prediction2 <- predict(mlx,test11, type="response")
rmse(test11$log_price, prediction2)
#[1]
# calc.relimp(mlx, type = c("lmg"), rela = TRUE, rank = TRUE)

## (3) Stepwise Linear Regression
Fitstart <- lm(formula = train11$log_price ~ 1,data = train11)
FitAll <- lm(log_price ~ . ,data = train11)
formula(FitAll)
step(Fitstart, direction = "both",scope = formula(FitAll))

mlr <- lm(formula = train11$log_price ~ area + newTown + dwelling_type + 
            bedrooms_centre + newCounty + text9 + ber_new + bath_sq + 
            bathrooms_centre + text10 + text22 + text14 + text20 + Dist_to_city + 
            text13 + latitude + text19, data = train11)
getOption("max.print")
options(max.print = 1000)
summary(mlr)
prediction13 <- predict(mlr,test11, type="response")
rmse(test11$log_price, prediction13)
#[1]

## (4) Decision Trees
myformula <- log_price ~ .
modfit <- rpart(myformula, method="anova" , data = train11)
prediction14 <- predict(modfit,newdata = test11)
rmse(test11$log_price,prediction14)
# [1]


## (5) Random Forest
rf <- randomForest(log_price ~ ., data = train12, ntree=1000, proximity=TRUE)
varImpPlot(rf)
prediction15 <- predict(rf, newdata = test12)
rmse(test12$log_price, prediction15)
#[1]


## (6) Regularized Regression(Lasso)
names(train12)
all_predictors <- subset(train12,select = -c(log_price))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(train12,select=names(num_classes))
#corrplot(cor(num_vars),method="number")
#corrplot(cor(num_vars),method="circle")
#Building model
set.seed(425)

lasso <-cv.glmnet(as.matrix(train12[, -204]), train12[, 204])
prediction16 <- predict(lasso, newx = as.matrix(test12[, - 204]), s = "lambda.min")
rmse(test12$log_price,prediction16)
#[1]


## (7) Gradient Boosting model(GBM)
set.seed(415)
cv.ctrl_gbm <- trainControl(method="repeatedcv",number=5,repeats = 5)
gbm<- train(log_price ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = train12,verbose = FALSE)
varImp(gbm)
prediction17 <- predict(gbm,newdata = test12)
rmse(test12$log_price,prediction17)
#[1]


## (8) XGBOOST(Extreme Gradient Boosting)
# preparing matrix 
dtrain <- xgb.DMatrix(data = as.matrix(train12[,-204]),label = as.matrix(train12$log_price))
dtest <- xgb.DMatrix(data = as.matrix(test12[,-204]),label=as.matrix(test12$log_price))
#Building model
set.seed(411)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 
prediction18 <- predict(xgb,newdata = dtest)
rmse(test12$log_price,prediction18)
#[1]

## (x) Simple Average RMSE of Random Forest + GBM + XGBoost (Top3 performing models)
rmse(test12$log_price, (prediction15 + prediction17 + prediction18)/3)
#[1]

## (x) Weighted Average RMSE of Lasso+GBM+XGBoost
rmse(test12$log_price, (0.1 *prediction15 + 0.3 *prediction17 + 0.6 *prediction18))
#[1] 

## (9) Ensemble method
my_control <- trainControl(method="boot",number=5,savePredictions="final")
set.seed(421)
model_list <- caretList(
  log_price ~ ., data=train12,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                          shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                                  eta =0.01,gamma = 0.0468,subsample=0.769,
                                                                  colsample_bytree =0.283))
  )
)
modelCor(resamples(model_list))




##Simple Blending
set.seed(433455)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
greedy_ensemble
varImp(greedy_ensemble)
summary(greedy_ensemble)
prediction19 <- predict(greedy_ensemble,newdata = test12)
rmse(test12$log_price,prediction19)
# [1]

# (10) Using a "meta-model"
set.seed(417)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
                          trControl=trainControl(method="boot",number=5,savePredictions="final"))
prediction20 <- predict(rf_ensemble,newdata = test12)
rmse(test12$log_price,prediction20)
# [1]


## (11) Simple Average RMSE of XGBoost+Ensemble+Meta-Model(Top3 performance models)
prediction21 <- (prediction18 + prediction19 + prediction20)/3
rmse(test12$log_price, prediction21)
#[1]

## (12) Neural Network

UDF <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}

train_o <- datam
train <- as.data.frame(apply(train_o, 2, UDF))
index <- sample(nrow (train), round(0.70 * nrow(train)))

train.wp <- train[index,]
test.wp <- train[-index,]


##
#procValues <- preProcess(datam, method = c("center", "scale"))
#scaledTraindata <-  predict(procValues, datatrain )
#scaledTestdata <-  predict(procValues, datatest)

library(neuralnet)
allVars <- colnames(train)
predictorVars <- allVars[!allVars%in%"log_price"]
predictorVars <- paste(predictorVars, collapse = "+")
form = as.formula(paste("log_price~", predictorVars, collapse = "+"))

# Prediction Model
nn_model <- neuralnet(formula = form, train.wp, hidden = c(2,1), linear.output = TRUE)

test.wp_1 <- subset(test.wp, select = -c(log_price))

prediction1 <- compute(nn_model, test.wp_1)
str(prediction1)
# UDF: Convert the scaled values to original 
UDF_2 <- function(prediction) {
  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
}

Prediction <-  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Prediction <- exp(Prediction)

Actual <- test.wp$log_price * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Actual <- exp(Actual)

table(Actual, Prediction)

submit.df <- data.frame(Real = Actual, PredictPrice = Prediction)
write.csv(submit.df, file = "Prediction_neural_spatial.csv", row.names = FALSE)







# RMSE results
rmse(test12$log_price, prediction13) # [1] 0.178790619
rmse(test12$log_price, prediction14) # [1] 0.2807840408
rmse(test12$log_price, prediction15) # [1] 0.1736463843
rmse(test12$log_price, prediction16) # [1] 0.178019933
rmse(test12$log_price, prediction17) # [1] 0.1605449246
rmse(test12$log_price, prediction18) # [1] 0.1532523828
rmse(test12$log_price, prediction19) # [1] 0.1557409292
rmse(test12$log_price, prediction20) # [1] 0.1640881857
rmse(test12$log_price, prediction21) # [1] 0.1538401409
# rmse(test12$log_price, Prediction22) # [1] 

# Predictions
prediction_model11 <- exp(prediction13)
prediction_model12 <- exp(prediction14)
prediction_model13 <- exp(prediction15)
prediction_model14 <- exp(prediction16)
prediction_model15 <- exp(prediction17)
prediction_model16 <- exp(prediction18)
prediction_model17 <- exp(prediction19)
prediction_model18 <- exp(prediction20)
prediction_model19 <- exp(prediction21)
# Prediction22 <- exp(Prediction22)

output <- (cbind("ID"=validation$longitude,"Orginal Price"=exp(validation$log_price),"Model1"=prediction_model11,
                 "Model2"=prediction_model12, "Model3"=prediction_model13, "Model4"=prediction_model14, "Model5"=prediction_model15,
                 "Model6"=prediction_model16, "Model7"=prediction_model17, "Model8"=prediction_model18,
                 "Model9"=prediction_model19))
write.csv(output, file = "models_price_v_predicted_logp_spatial.csv", row.names=FALSE)


#########################################
########## SPATIAL ##############
#### USING LOG PRICE PER METRE SQUARED ####
names(new_train)

train1 <- new_train[c(2:3, 5:28, 200, 209:210)]
test11 <- validation[c(2:3, 5:28, 200, 209:210)]
names(train1)


#### USE THIS FOR MACHINE LEARNING & NEURAL NETWORK ####
datam <- subset(datay, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, coast_round, city_round, dart_round, luas_round, bus_round))
train2 <- subset(new_train, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, coast_round, city_round, dart_round, luas_round, bus_round))
test2 <- subset(validation, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, coast_round, city_round, dart_round, luas_round, bus_round))
names(train2)

##(a).MLR
#First include all predictor variables to see what will happen
mlx <-lm(formula = log_price_m2 ~ . - log_price_m2, data = train1)
getOption("max.print")
options(max.print = 1000)
summary(mlx)
prediction2 <- predict(mlx,test11, type="response")
rmse(test11$log_price_m2, prediction2)
#[1]

## (3) Stepwise Linear Regression
Fitstart <- lm(formula = train1$log_price_m2 ~ 1,data = train1)
FitAll <- lm(log_price_m2 ~ . ,data = train1)
formula(FitAll)
step(Fitstart, direction = "both",scope = formula(FitAll))

mlr <- lm(formula = train1$log_price_m2 ~ newTown + dwelling_type + 
            bedrooms_centre + Dist_to_dart + newCounty + text9 + text19 + 
            text13 + bathrooms_centre + bath_sq + text22 + text10 + area + 
            text17 + text23 + text7 + Dist_to_city + latitude + text14, 
          data = train1)
getOption("max.print")
options(max.print = 1000)
summary(mlr)
prediction33 <- predict(mlr,test11, type="response")
rmse(test11$log_price_m2, prediction33)
#[1]

## (4) Decision Trees
myformula <- log_price_m2 ~ .
modfit <- rpart(myformula, method="anova" , data = train1)
prediction34 <- predict(modfit,newdata = test11)
rmse(test11$log_price_m2,prediction34)
# [1]


## (5) Random Forest
rf <- randomForest(log_price_m2 ~ ., data = train2, ntree=1000, proximity=TRUE)
varImpPlot(rf)
prediction35 <- predict(rf, newdata = test2)
rmse(test2$log_price_m2, prediction35)
#[1]


## (6) Regularized Regression(Lasso)
names(train2)
all_predictors <- subset(train2,select = -c(log_price_m2))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(train2,select=names(num_classes))
#corrplot(cor(num_vars),method="number")
#corrplot(cor(num_vars),method="circle")
#Building model
set.seed(525)

lasso <-cv.glmnet(as.matrix(train2[, -203]), train2[, 203])
prediction36 <- predict(lasso, newx = as.matrix(test2[, - 203]), s = "lambda.min")
rmse(test2$log_price_m2,prediction36)
#[1]


## (7) Gradient Boosting model(GBM)
set.seed(515)
cv.ctrl_gbm <- trainControl(method="repeatedcv",number=5,repeats = 5)
gbm<- train(log_price_m2 ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = train2,verbose = FALSE)
varImp(gbm)
prediction37 <- predict(gbm,newdata = test2)
rmse(test2$log_price_m2,prediction37)
#[1]


## (8) XGBOOST(Extreme Gradient Boosting)
# preparing matrix 
dtrain <- xgb.DMatrix(data = as.matrix(train2[,-203]),label = as.matrix(train2$log_price_m2))
dtest <- xgb.DMatrix(data = as.matrix(test2[,-203]),label=as.matrix(test2$log_price_m2))
#Building model
set.seed(511)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 
prediction38 <- predict(xgb,newdata = dtest)
rmse(test2$log_price_m2,prediction38)
#[1]

## (x) Simple Average RMSE of Random Forest + GBM + XGBoost (Top3 performing models)
rmse(test2$log_price_m2, (prediction35 + prediction37 + prediction38)/3)
#[1]

## (x) Weighted Average RMSE of Lasso+GBM+XGBoost
rmse(test2$log_price_m2, (0.1 *prediction35 + 0.3 *prediction37 + 0.6 *prediction38))
#[1] 

## (9) Ensemble method
my_control <- trainControl(method="boot",number=5,savePredictions="final")
set.seed(521)
model_list <- caretList(
  log_price_m2 ~ ., data=train2,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                          shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                                  eta =0.01,gamma = 0.0468,subsample=0.769,
                                                                  colsample_bytree =0.283))
  )
)
modelCor(resamples(model_list))




##Simple Blending
set.seed(533455)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
greedy_ensemble
varImp(greedy_ensemble)
summary(greedy_ensemble)
prediction39 <- predict(greedy_ensemble,newdata = test2)
rmse(test2$log_price_m2,prediction39)
# [1]

# (10) Using a "meta-model"
set.seed(517)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
                          trControl=trainControl(method="boot",number=5,savePredictions="final"))
prediction40 <- predict(rf_ensemble,newdata = test2)
rmse(test2$log_price_m2,prediction40)
# [1]


## (11) Simple Average RMSE of XGBoost+Ensemble+Meta-Model(Top3 performance models)
prediction41 <- (prediction38 + prediction39 + prediction40)/3
rmse(test2$log_price_m2, prediction41)
#[1]

## (12) Neural Network

UDF <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}

train_o <- datam
train <- as.data.frame(apply(train_o, 2, UDF))
index <- sample(nrow (train), round(0.70 * nrow(train)))

train.wp <- train[index,]
test.wp <- train[-index,]


##
#procValues <- preProcess(datam, method = c("center", "scale"))
#scaledTraindata <-  predict(procValues, datatrain )
#scaledTestdata <-  predict(procValues, datatest)

library(neuralnet)
allVars <- colnames(train)
predictorVars <- allVars[!allVars%in%"log_price_m2"]
predictorVars <- paste(predictorVars, collapse = "+")
form = as.formula(paste("log_price_m2~", predictorVars, collapse = "+"))

# Prediction Model
nn_model <- neuralnet(formula = form, train.wp, hidden = c(2,1), linear.output = TRUE)

test.wp_1 <- subset(test.wp, select = -c(log_price_m2))

prediction1 <- compute(nn_model, test.wp_1)
str(prediction1)
# UDF: Convert the scaled values to original 
UDF_2 <- function(prediction) {
  prediction1$net.result * (max(train_o$log_price_m2)-min(train_o$log_price_m2)) + min(train_o$log_price_m2)
}

Prediction <-  prediction1$net.result * (max(train_o$log_price_m2)-min(train_o$log_price_m2)) + min(train_o$log_price_m2)
Prediction <- exp(Prediction)*test.wp$area

Actual <- test.wp$log_price_m2 * (max(train_o$log_price_m2)-min(train_o$log_price_m2)) + min(train_o$log_price_m2)
Actual <- exp(Actual)*test.wp$area

table(Actual, Prediction)

submit.df <- data.frame(Real = Actual, PredictPrice = Prediction)
write.csv(submit.df, file = "Prediction_neural_spatial_m2.csv", row.names = FALSE)







# RMSE results
rmse(test2$log_price_m2, prediction33) # [1] 0.1797238
rmse(test2$log_price_m2, prediction34) # [1] 0.204188
rmse(test2$log_price_m2, prediction35) # [1] 0.1649854
rmse(test2$log_price_m2, prediction36) # [1] 0.1790057
rmse(test2$log_price_m2, prediction37) # [1] 0.1581145
rmse(test2$log_price_m2, prediction38) # [1] 0.1534901
rmse(test2$log_price_m2, prediction39) # [1] 0.1513939
rmse(test2$log_price_m2, prediction40) # [1] 0.1586336
rmse(test2$log_price_m2, prediction41) # [1] 0.1524193


# Predictions
prediction_model31 <- exp(prediction33)*validation$area
prediction_model32 <- exp(prediction34)*validation$area
prediction_model33 <- exp(prediction35)*validation$area
prediction_model34 <- exp(prediction36)*validation$area
prediction_model35 <- exp(prediction37)*validation$area
prediction_model36 <- exp(prediction38)*validation$area
prediction_model37 <- exp(prediction39)*validation$area
prediction_model38 <- exp(prediction40)*validation$area
prediction_model39 <- exp(prediction41)*validation$area
# Prediction22 <- exp(Prediction22)

output <- (cbind("ID"=validation$longitude,"Orginal Price"=exp(validation$log_price_m2)*validation$area,"Model1"=prediction_model31,
                 "Model2"=prediction_model32, "Model3"=prediction_model33, "Model4"=prediction_model34, "Model5"=prediction_model35,
                 "Model6"=prediction_model36, "Model7"=prediction_model37, "Model8"=prediction_model38,
                 "Model9"=prediction_model39))
write.csv(output, file = "models_price_v_predicted_logpm2_spatial.csv", row.names=FALSE)


#########################################
########## SPATIAL - Log Price ##############
###### Properties <= ???1.2m ##########
#### USE THIS FOR REGRESSION ####

names(new_train)
new_train$log_price <- log(new_train$price)
validation$log_price <- log(validation$price)

new_train3 <- new_train[new_train$price <= 1200000,]
validation3 <- validation[validation$price <= 1200000,]

train31 <- new_train3[c(2:3, 5:28, 200, 210, 216)]
test31 <- validation3[c(2:3, 5:28, 200, 210, 216)]
names(train31)



#### USE THIS FOR MACHINE LEARNING & NEURAL NETWORK ####
# datam <- subset(datay, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new))
train32 <- subset(new_train3, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, log_price_m2, coast_round, city_round, dart_round, luas_round, bus_round))
test32 <- subset(validation3, select = - c(price, newTown, ber, dwelling_type, newCounty, Side...North, Side...South, ber_new, log_price_m2, coast_round, city_round, dart_round, luas_round, bus_round))
names(train32)

##(a).MLR
#First include all predictor variables to see what will happen
mlx <-lm(formula = log_price ~ . - newCounty, data = train31)
getOption("max.print")
options(max.print = 1000)
summary(mlx)
prediction2 <- predict(mlx,test31, type="response")
rmse(test31$log_price, prediction2)
#[1]
# calc.relimp(mlx, type = c("lmg"), rela = TRUE, rank = TRUE)

## (3) Stepwise Linear Regression
Fitstart <- lm(formula = train31$log_price ~ 1,data = train31)
FitAll <- lm(log_price ~ . ,data = train31)
formula(FitAll)
step(Fitstart, direction = "both",scope = formula(FitAll))

mlr <- lm(formula = train31$log_price ~ area + newTown + dwelling_type + 
            bedrooms_centre + newCounty + bath_sq + ber_new + text9 + 
            text20 + text14 + bathrooms_centre + text19 + text22 + Dist_to_city + 
            latitude + text17 + nearst_luas + text13 + text10 + text11, 
          data = train31)
getOption("max.print")
options(max.print = 1000)
summary(mlr)
prediction43 <- predict(mlr,test31, type="response")
rmse(test31$log_price, prediction43)
#[1]

## (4) Decision Trees
myformula <- log_price ~ .
modfit <- rpart(myformula, method="anova" , data = train31)
prediction44 <- predict(modfit,newdata = test31)
rmse(test31$log_price,prediction44)
# [1]


## (5) Random Forest
rf <- randomForest(log_price ~ ., data = train32, ntree=1000, proximity=TRUE)
varImpPlot(rf)
prediction45 <- predict(rf, newdata = test32)
rmse(test32$log_price, prediction45)
#[1]


## (6) Regularized Regression(Lasso)
names(train32)
all_predictors <- subset(train32,select = -c(log_price))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(train32,select=names(num_classes))
#corrplot(cor(num_vars),method="number")
#corrplot(cor(num_vars),method="circle")
#Building model
set.seed(625)

lasso <-cv.glmnet(as.matrix(train32[, -204]), train32[, 204])
prediction46 <- predict(lasso, newx = as.matrix(test32[, - 204]), s = "lambda.min")
rmse(test32$log_price,prediction46)
#[1]


## (7) Gradient Boosting model(GBM)
set.seed(615)
cv.ctrl_gbm <- trainControl(method="repeatedcv",number=5,repeats = 5)
gbm<- train(log_price ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = train32,verbose = FALSE)
varImp(gbm)
prediction47 <- predict(gbm,newdata = test32)
rmse(test32$log_price,prediction47)
#[1]


## (8) XGBOOST(Extreme Gradient Boosting)
# preparing matrix 
dtrain <- xgb.DMatrix(data = as.matrix(train32[,-204]),label = as.matrix(train32$log_price))
dtest <- xgb.DMatrix(data = as.matrix(test32[,-204]),label=as.matrix(test32$log_price))
#Building model
set.seed(611)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 
prediction48 <- predict(xgb,newdata = dtest)
rmse(test32$log_price,prediction48)
#[1]

## (x) Simple Average RMSE of Random Forest + GBM + XGBoost (Top3 performing models)
rmse(test32$log_price, (prediction45 + prediction47 + prediction48)/3)
#[1]

## (x) Weighted Average RMSE of Lasso+GBM+XGBoost
rmse(test32$log_price, (0.1 *prediction45 + 0.3 *prediction47 + 0.6 *prediction48))
#[1] 

## (9) Ensemble method
my_control <- trainControl(method="boot",number=5,savePredictions="final")
set.seed(621)
model_list <- caretList(
  log_price ~ ., data=train32,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                          shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                                  eta =0.01,gamma = 0.0468,subsample=0.769,
                                                                  colsample_bytree =0.283))
  )
)
modelCor(resamples(model_list))




##Simple Blending
set.seed(633455)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
greedy_ensemble
varImp(greedy_ensemble)
summary(greedy_ensemble)
prediction49 <- predict(greedy_ensemble,newdata = test32)
rmse(test32$log_price,prediction49)
# [1]

# (10) Using a "meta-model"
set.seed(617)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
                          trControl=trainControl(method="boot",number=5,savePredictions="final"))
prediction50 <- predict(rf_ensemble,newdata = test32)
rmse(test32$log_price,prediction50)
# [1]


## (11) Simple Average RMSE of XGBoost+Ensemble+Meta-Model(Top3 performance models)
prediction51 <- (prediction48 + prediction49 + prediction50)/3
rmse(test32$log_price, prediction51)
#[1]

## (12) Neural Network

UDF <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}

train_o <- datam
train <- as.data.frame(apply(train_o, 2, UDF))
index <- sample(nrow (train), round(0.70 * nrow(train)))

train.wp <- train[index,]
test.wp <- train[-index,]


##
#procValues <- preProcess(datam, method = c("center", "scale"))
#scaledTraindata <-  predict(procValues, datatrain )
#scaledTestdata <-  predict(procValues, datatest)

library(neuralnet)
allVars <- colnames(train)
predictorVars <- allVars[!allVars%in%"log_price"]
predictorVars <- paste(predictorVars, collapse = "+")
form = as.formula(paste("log_price~", predictorVars, collapse = "+"))

# Prediction Model
nn_model <- neuralnet(formula = form, train.wp, hidden = c(2,1), linear.output = TRUE)

test.wp_1 <- subset(test.wp, select = -c(log_price))

prediction1 <- compute(nn_model, test.wp_1)
str(prediction1)
# UDF: Convert the scaled values to original 
UDF_2 <- function(prediction) {
  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
}

Prediction <-  prediction1$net.result * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Prediction <- exp(Prediction)

Actual <- test.wp$log_price * (max(train_o$log_price)-min(train_o$log_price)) + min(train_o$log_price)
Actual <- exp(Actual)

table(Actual, Prediction)

submit.df <- data.frame(Real = Actual, PredictPrice = Prediction)
write.csv(submit.df, file = "Prediction_neural_spatial.csv", row.names = FALSE)







# RMSE results
rmse(test32$log_price, prediction43) # [1] 0.1675403159
rmse(test32$log_price, prediction44) # [1] 0.2296230449
rmse(test32$log_price, prediction45) # [1] 0.1580566661
rmse(test32$log_price, prediction46) # [1] 0.1673148988
rmse(test32$log_price, prediction47) # [1] 0.1437361712
rmse(test32$log_price, prediction48) # [1] 0.1402385366
rmse(test32$log_price, prediction49) # [1] 0.1388763678
rmse(test32$log_price, prediction50) # [1] 0.144111393
rmse(test32$log_price, prediction51) # [1] 0.1384158531
# rmse(test32$log_price, Prediction22) # [1] 

# Predictions
prediction_model41 <- exp(prediction43)
prediction_model42 <- exp(prediction44)
prediction_model43 <- exp(prediction45)
prediction_model44 <- exp(prediction46)
prediction_model45 <- exp(prediction47)
prediction_model46 <- exp(prediction48)
prediction_model47 <- exp(prediction49)
prediction_model48 <- exp(prediction50)
prediction_model49 <- exp(prediction51)
# Prediction22 <- exp(Prediction22)

output <- (cbind("ID"=validation3$longitude,"Orginal Price"=exp(validation3$log_price),"Model1"=prediction_model41,
                 "Model2"=prediction_model42, "Model3"=prediction_model43, "Model4"=prediction_model44, "Model5"=prediction_model45,
                 "Model6"=prediction_model46, "Model7"=prediction_model47, "Model8"=prediction_model48,
                 "Model9"=prediction_model49))
write.csv(output, file = "models_price_v_predicted_logp_spatial_reduced.csv", row.names=FALSE)




plot(prediction_model46, exp(validation3$log_price), main = "Predicted vs. Asking Price - Reduced Dataset - XGB Model", xlab = "Predicted Price", ylab = "Asking Price")
abline(0, 1, col = "green", lwd = 2)
abline(0, 0.95, col = "blue", lwd = 2, lty = 2)
abline(0, 1.05, col = "blue", lwd = 2, lty = 2)
abline(0, 0.9, col = "red", lwd = 2, lty = 2)
abline(0, 1.1, col = "red", lwd = 2, lty = 2)
legend(100000, 1200000, legend = c("Predicted = Asking", "5% prediction error", "10% prediction error"), col = c("green", "blue", "red"), lty = c(1,2,2), lwd = c(2,2,2))


plot(prediction_model16, exp(validation$log_price), main = "Predicted vs. Asking Price - XGB Spatial Model", xlab = "Predicted Price", ylab = "Asking Price")
abline(0, 1, col = "green", lwd = 2)
abline(0, 0.95, col = "blue", lwd = 2, lty = 2)
abline(0, 1.05, col = "blue", lwd = 2, lty = 2)
abline(0, 0.9, col = "red", lwd = 2, lty = 2)
abline(0, 1.1, col = "red", lwd = 2, lty = 2)
legend(100000, 7000000, legend = c("Predicted = Asking", "5% prediction error", "10% prediction error"), col = c("green", "blue", "red"), lty = c(1,2,2), lwd = c(2,2,2))


# plot(exp(datam$log_price), datam$area, main = "Log Predicted vs. Log Asking Price", xlab = "Log Predicted Price", ylab = "Log Asking Price")
# # plot(rfpredict, datay$log_price, main = "Log Predicted vs. Asking Price")
# abline(0,1, col = "red", lwd = 3)
# 
# 
# datay <- subset(datam, select = c(log_price, area))
# datay$price <- exp(datay$log_price)
# graph5 <- ggplot(datay, aes(x = price, y = area)) + geom_point() +
#   ggtitle("Price vs Area") +
#   ylab("Area") +
#   xlab("Price")
# graph5
# 
# datay <- datay[datay$price < 2000000,]
# graph6 <- ggplot(datay, aes(x = price, y = area)) + geom_point() +
#   ggtitle("Price (Less than ???2m) vs Area") +
#   ylab("Area") +
#   xlab("Price")
# graph6
# 
# grid1 <- grid.arrange(graph5, graph6, ncol = 2)


