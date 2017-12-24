setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\default-of-credit-card-clients-dataset")
###############################Read and analyse data##################################
credit_card_data <- read.csv("UCI_Credit_Card.csv")
str(credit_card_data)
credit_card_data <- credit_card_data[-1] ####Drop ID
credit_card_data$default <- credit_card_data$default.payment.next.month

####Function to Convert categorical variables to factors from int###
toFactor <- function(x) {
  return(as.factor(x))
}

###########Convert numeric to factor##################

credit_card_data[c(2,3,4,6,7,8,9,10,11,24,25)] = lapply(credit_card_data[c(2,3,4,6,7,8,9,10,11,24)],toFactor)

##################Analyse data with ggplot##############

library(ggplot2)
ggplot(data = credit_card_data, aes(x = PAY_0, y = BILL_AMT1)) + geom_boxplot() + geom_jitter() 

pl <- ggplot(data=credit_card_data,aes(x=AGE))
pl+geom_histogram(binwidth = 10,aes(fill=default.payment.next.month),colour="Black")+
  facet_grid(MARRIAGE~.)

################Build Random Forest Model###############
train_data <- credit_card_data[1:24000,]
test_data <- credit_card_data[24001:30000,]
library(randomForest)
model <- randomForest(default ~ ., data = train_data)
predictions <- predict(model, test_data)
table(predictions, test_data$default)