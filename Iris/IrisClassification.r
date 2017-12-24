setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\Iris")

iris_data <- read.csv("Iris.csv")

###################################################Analyse Data##########################################

library(ggplot2)
ggplot(data=iris_data, aes(x=SepalLengthCm, y=SepalWidthCm, colour=Species)) + geom_point()
ggplot(data=iris_data, aes(x=PetalLengthCm, y=PetalWidthCm, colour=Species)) + geom_point()

###################################################Prepare Data##########################################

samples <- sample(150,100)
train_data <- iris_data[samples,]
test_data <- iris_data[-samples,]

###############################################Decision Tree###############################################

library(C50)
model <- C5.0(train_data[c(-1,-6)],train_data$Species)
predictions <- predict(model, test_data)
table(predictions, test_data$Species)

##############################################Logistic Regression##########################################

library(rpart)
model <- rpart(Species~., data = train_data[-1])
predictions <- predict(model,test_data$Species)
table(predictions, test_data$Species)
