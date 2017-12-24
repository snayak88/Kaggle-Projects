#######################Glass data analysis with Neural net#################################
setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\glass-classification")
glass_data <- read.csv("glass.csv")
glass_data$Type <- as.factor(glass_data$Type)

#################Normalize function for kNN algorithm###################
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}
#################Normalize data for kNN algorithm#######################
glass_data_n <- as.data.frame(lapply(glass_data_n[1:9], normalize))
glass_data_n$Type <- glass_data$Type
samples <- sample(214,150)
glass_data_train <- glass_data_n[samples,]
glass_data_test <- glass_data_n[-samples,]
glass_data_train_labels <- glass_data[samples,10]
glass_data_test_labels <- glass_data[-samples,10]
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "1")
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "2")
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "3")
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "5")
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "6")
glass_data_train <- cbind(glass_data_train, glass_data_train$Type == "7")

glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "1")
glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "2")
glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "3")
glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "5")
glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "6")
glass_data_test <- cbind(glass_data_test, glass_data_test$Type == "7")

names(glass_data_train)[11:16] <- c("type1", "type2", "type3", "type5", "type6", "type7")
library(neuralnet)
model <- neuralnet(type1 + type2 + type3 + type5 + type6 + type7  ~ RI+Na+Mg+Al+Si+K+Ca+Ba+Fe, data = glass_data_train)
model_result <- compute(model, glass_data_test[1:9])
