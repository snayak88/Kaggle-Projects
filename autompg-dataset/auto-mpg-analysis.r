setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\autompg-dataset")
auto_data <- read.csv("auto-mpg.csv")
str(auto_data)
auto_data[!complete.cases(auto_data),]

square <- function(x)
{
  return(x*x)
}
################ Preprocess the dataset #########################
summary(auto_data$cylinders)
auto_data$cylinders <- as.factor(auto_data$cylinders)
summary(auto_data$model.year)
auto_data$model.year <- as.factor(auto_data$model.year)
levels(auto_data$model.year)
summary(auto_data$displacement)
summary(auto_data$horsepower) ## Do we need to change to int ?
auto_data$horsepower <- as.integer(auto_data$horsepower)
auto_data$origin <- as.factor(auto_data$origin)
auto_data <- auto_data[-9] ##Remove the car names as they may not be useful predictors.
auto_data$displacement2 <- sapply(auto_data$displacement,square)
auto_data$weight2 <- sapply(auto_data$weight,square)
pairs(train_data[c("cylinders", "displacement", "horsepower", "weight", "acceleration")])
############### Analyse the features ###########################
library(ggplot2)
ggplot(data = auto_data, aes(x=cylinders, y=mpg)) + geom_boxplot() + geom_jitter() #Most cars are 4 cylinder. 4 and 5 cylinder engines have the best performance.
																					#Post that performance reduces with increase in number of cylinders.

ggplot(data = auto_data, aes(x=displacement, y=mpg)) + geom_point() # MPG decreases quadratically with increase in displacement

ggplot(data=auto_data, aes(x=horsepower, y=mpg)) + geom_point() # Vehicles with horsepower > 50 have higher mpg. Check for ouliers in 
                                                                #horsepower < 50

ggplot(data=auto_data, aes(x=weight, y=mpg)) + geom_point() #MPG decreases quadratically with increase in weight.

ggplot(data=auto_data, aes(x=acceleration, y=mpg)) + geom_point() #No clear trend but higher acceleration generally means higher 
                                                                  #MPG

ggplot(data=auto_data, aes(x=model.year, y=mpg)) + geom_boxplot() # Steady increase in mileage since 1975.

ggplot(data=auto_data, aes(x=origin, y=mpg)) + geom_boxplot() + geom_jitter() # Average mpg increases with increase in origin from 1-3

############# Fit the linear regression model ####################
library(dummies)
dummy_auto_data <- dummy.data.frame(auto_data)
str(dummy_auto_data)
samples <- sample(398,300)
train_data <- auto_data[samples,]
test_data <-  auto_data[-samples,]

library(stats)
model <- lm(mpg ~ ., data = train_data)
test_data$predictions <- predict(model, test_data)
ggplot(data = test_data, aes(x=mpg, y=predictions)) + geom_point() + geom_smooth(method = "lm")
cor( test_data$mpg, test_data$predictions)

################# Analysis using regression tree ##################
MAE <- function(x,y) {
  return(mean(abs(x-y)))
}

library(rpart)
model <- rpart(mpg ~ ., data = train_data)
test_data$predictions <- predict(model, test_data)
ggplot(data = test_data, aes(x=mpg, y=predictions)) + geom_point() + geom_smooth(method = "lm")
library(rpart.plot)
rpart.plot(model)
summary(test_data$predictions)
cor(test_data$mpg, test_data$predictions)
mae <- MAE(test_data$mpg, test_data$predictions)
mae

################## Analysis using model trees #####################
library(RWeka)
model <- M5P(mpg ~ ., data = train_data)
test_data$predictions <- predict(model, test_data)
ggplot(data = test_data, aes(x=mpg, y=predictions)) + geom_point() + geom_smooth(method = "lm")
cor(test_data$mpg, test_data$predictions)
mae <- MAE(test_data$mpg, test_data$predictions)
mae
