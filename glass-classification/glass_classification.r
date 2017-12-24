setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\glass-classification")

###################Read and Analyse Data#############################
library(ggplot2)
glass_data <- read.csv("glass.csv")
str(glass_data)
glass_data$Type <- as.factor(glass_data$Type)
glass_data[!complete.cases(glass_data),]
ggplot(data = glass_data, aes(x=RI, y = Type))+geom_point()
table(glass_data$Type)

#################Normalize function for kNN algorithm###################
normalize <- function(x) {
	return ((x-min(x)) / (max(x)-min(x)))
}
#################Normalize data for kNN algorithm#######################
glass_data_n <- as.data.frame(lapply(glass_data[1:9], normalize))
str(glass_data_n)

#################Create training and test datasets for analysis#########
samples <- sample(214,150)
glass_data_train <- glass_data_n[samples,]
glass_data_test <- glass_data_n[-samples,]
glass_data_train_labels <- glass_data[samples,10]
glass_data_test_labels <- glass_data[-samples,10]
prop.table(table(glass_data_train_labels))
prop.table(table(glass_data_test_labels))
library(class)
glass_data_pred <- knn(train = glass_data_train, test = glass_data_test, cl = glass_data_train_labels, k = 14)

##################Analyse results#####################
library(gmodels)
CrossTable(x = glass_data_test_labels, y = glass_data_pred, prop.chisq = FALSE)

##################Improve results using Z score standardization###############
glass_data_z <- as.data.frame(scale(glass_data[1:9]))
summary(glass_data_z)
samples <- sample(214,150)
glass_data_train <- glass_data_z[samples,]
glass_data_test <- glass_data_z[-samples,]
glass_data_train_labels <- glass_data[samples,10]
glass_data_test_labels <- glass_data[-samples,10]
prop.table(table(glass_data_train_labels))
prop.table(table(glass_data_test_labels))
glass_data_pred <- knn(train = glass_data_train, test = glass_data_test, cl = glass_data_train_labels, k = 10)
CrossTable(x = glass_data_test_labels, y = glass_data_pred, prop.chisq = FALSE)

##################Train data using Caret package#########################
library(caret)
glass_data_train$Type <- glass_data_train_labels
m <- train(Type ~ ., data = glass_data_train, method = "knn")