setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\adult-census-income")

data <- read.csv("adult.csv")
str(data)
#=========================================================================================================
library(ggplot2)
ggplot(data = data, aes(x=income, y = age)) + geom_boxplot()
#Does not make that much of a significant impact to the predictions
#=========================================================================================================
ggplot(data = data, aes(x=income, y = hours.per.week)) + geom_boxplot()
#Does not make that much of a significant difference although people working > 40 hrs are likelier to make >50K
#=========================================================================================================
ggplot(data = data, aes(x=income, y = fnlwgt)) + geom_boxplot()
#Does not have any effect. Remove
#=========================================================================================================
table(data$education, data$income)
#Education has a significant impact on income. People who have gone to college are more likely to earn more than 50,000.
#=========================================================================================================
ggplot(data = data, aes(x=income, y = education.num)) + geom_boxplot()
#Education.num > 10 has a very high chance of earning > 50000
#===========================================================================================================
table(data$marital.status, data$income)
#Not being married significantly increases chance of income <=50k
#============================================================================================================
table(data$occupation, data$income)
#Classification may not be that clearcut
#=============================================================================================================
table(data$relationship, data$income)
#Correlated with marital status
#=============================================================================================================
prop.table(table(data$sex, data$income))
#Men are likelier to earn more than 50k per annum
#=============================================================================================================
ggplot(data = data, aes(x = income, y = capital.gain)) + geom_boxplot()
#Some outliers may skewer the data. Remove.
#=============================================================================================================
ggplot(data = data, aes(x = income, y = capital.loss)) + geom_boxplot()
#Capital loss > 1500 means  income > 50000 except for 1 outlier.
#==============================================================================================================
table(data$native.country, data$income)
#May not be that useful. Remove.
#================================================================================================================
library(randomForest)
samples <- sample(32561,25000)
train_data <- data[samples,]s
test_data <- data[-samples,]
model <- randomForest(income ~ ., data = train_data)

####################### Use selective features ############################################
#model <- randomForest(income ~ age+hours.per.week+education.num+marital.status+sex+capital.loss, data = train_data)
model <- randomForest(income ~ age+hours.per.week+education.num+marital.status+sex+capital.loss+workclass+occupation+sex+capital.gain, data = train_data)
model
predictions <- predict(model, test_data)
table(predictions, test_data$income)