###########Read Data########################

setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\mushroom-classification")
mushroom_data <- read.csv("mushrooms.csv")

###############Analyse Data###############

str(mushroom_data)
mushroom_data[!complete.cases(mushroom_data),]
samples <- sample(8124, 6000)
train_data <- mushroom_data[samples,]
test_data <- mushroom_data[-samples,]

#############Decision Tree################

library(C50)
model <- C5.0(train_data[c(-1,-17)],train_data$class)
predictions <- predict(model,test_data)
table(predictions, test_data$class)

############Random Forest#################

library(randomForest)
model.rf = randomForest(class~.,data=train_data, mtry=6,importance=TRUE)
predictions <- predict(model.rf,test_data)
table(predictions, test_data$class)




