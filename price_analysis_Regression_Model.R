#Import the merged data (done in python)
merged_df <- read.csv("C:/Users/family/Desktop/Assignments/ALY 6040/Week2/Data set/uber&lyft/merged_df.csv")
#View(merged_df)
head(merged_df,6)


#get date from date time table and then put that in wday function.

library(lubridate)
#label=FALSE enable to store weekday as a number.
merged_df$day<- wday(merged_df$date_time, label = FALSE)





#Making a data frame with sensible columns
#removing all the unwanted columns
cab <- data.frame(merged_df$price,merged_df$surge_multiplier,merged_df$distance, merged_df$day,
                  merged_df$source,merged_df$destination,merged_df$product_id,merged_df$temp,
                  merged_df$clouds,merged_df$pressure,merged_df$rain,merged_df$humidity,
                  merged_df$wind)
#Renaming the columns of Cab data.frame
names(cab)<- c("Price", "Multiplier", "Distance", "Day","Source", "Destination", "Product_id",
               "Temp","Clouds","Pressure", "Rain", "Humidity", "Wind")
#NA handling
summary(cab) # Provide the column names which have NA

#Replace the NA with mean value of the column
install.packages("zoo")
library(zoo)
cab$Price <- na.aggregate(cab$Price)
cab$Multiplier <- na.aggregate(cab$Multiplier)
cab$Distance <- na.aggregate(cab$Distance)
cab$Temp<- na.aggregate(cab$Temp)
cab$Clouds <- na.aggregate(cab$Clouds)
cab$Pressure<- na.aggregate(cab$Pressure)
cab$Rain <- na.aggregate(cab$Rain)
cab$Humidity<- na.aggregate(cab$Humidity)
cab$Wind<- na.aggregate(cab$Wind)


#Deviding the data into training and test dataset
cab_index <- sample(x=1:nrow(cab), size = 0.75*nrow(cab))
cab_training <- cab[cab_index,]
cab_testing <- cab[-cab_index,]

#Model with all variables
price_model <- lm(Price~., data = cab_training)
summary(price_model)

#predicting the value and checking the accuracy
pred_price <- predict(price_model, newdata = cab_testing)
head(observed_price <- cab_testing$Price)
head(pred_price)
#R square
SSE_model <- sum((observed_price - pred_price)^2)
SST_model <- sum((observed_price-mean(observed_price))^2)
r2_model <- 1- SSE_model/SST_model
r2_model
#MSE
MSE_model <- mean((observed_price - pred_price)^2)
MSE_model


#Remodeloing with most important varibales and w/o destination 
price_model1 <- lm(Price ~ Multiplier + Distance + Day + Temp + 
                     Clouds + Pressure + Rain + Humidity + Wind + 
                     Product_id+ Source, 
                   data = cab_training)
summary(price_model1)
#predicting the value and checking the accuracy
#In order to avoid following obtained error in above model we will remove destination varibale.
#"Coefficients: (1 not defined because of singularities)"
pred_price1 <- predict(price_model1, newdata = cab_testing)
head(observed_price1 <- cab_testing$Price)
head(pred_price1)
#R square
SSE_model1 <- sum((observed_price1 - pred_price1)^2)
SST_model1 <- sum((observed_price1-mean(observed_price1))^2)
r2_model1 <- 1- SSE_model1/SST_model1
r2_model1
#MSE
MSE_model1 <- mean((observed_price1 - pred_price1)^2)
MSE_model1
AIC(price_model1)

#Optimization
#Forward and Backwrad
stepModel_forward<- step(price_model1,direction = "forward")
price_model_forward<-lm(Price ~ Multiplier + Distance + Day + Source + 
                          Product_id + Temp + Clouds + Pressure + Rain + Humidity + 
                          Wind, data = cab_training)
summary(price_model_forward)

pred_price2 <- predict(price_model_forward, newdata = cab_testing)
head(observed_price2 <- cab_testing$Price)
head(pred_price2)
#R square
SSE_model2 <- sum((observed_price2 - pred_price2)^2)
SST_model2 <- sum((observed_price2-mean(observed_price2))^2)
r2_model2 <- 1- SSE_model2/SST_model2
r2_model1
#MSE
MSE_model2 <- mean((observed_price2 - pred_price2)^2)
MSE_model2
AIC(price_model_forward)




stepModel_backward<- step(price_model1,direction = "backward")
#This optimization method provide the AIC along with 
#the same independent variable.
price_model_backward<-lm(Price ~ Multiplier + Distance + Day + Source + 
                           Product_id + Temp + Clouds + Pressure + Rain + Humidity + 
                           Wind, data = cab_training)
summary(price_model_backward)

pred_price3 <- predict(price_model_backward, newdata = cab_testing)
head(observed_price3 <- cab_testing$Price)
head(pred_price3)
#R square
SSE_model3 <- sum((observed_price3 - pred_price3)^2)
SST_model3 <- sum((observed_price3-mean(observed_price3))^2)
r2_model3 <- 1- SSE_model3/SST_model3
r2_model1
#MSE
MSE_model3 <- mean((observed_price3 - pred_price3)^2)
MSE_model3
AIC(price_model_backward)







