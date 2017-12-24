# German credit Scoring

# INTRODUCTION
# ============

# Le projet est découpé en 3 parties
# Exploratoty data analysis (EDA)
# Feature engineering
# Model 
# Prediction 


# Load Packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library(gtools) # for discretisation
library(corrplot)
library(Hmisc)
library(devtools)
library(PerformanceAnalytics)
library(FactoMineR)



# Importing Data
setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\default-of-credit-card-clients-dataset")
data <- read.csv("UCI_Credit_Card.csv")

head(data)
summary(data)

# In R, Catégorical variables are called Factors

# Make variables factors into factors
factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')


data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))


# Boxplots amount of credit lim by education 
# observations (points) are overlayed and jittered
qplot(EDUCATION, LIMIT_BAL, data=data, geom=c("boxplot", "jitter"), 
      fill=EDUCATION, main="LIMIT_AMT by Education cat",
      xlab="", ylab="amount of given credit")




# FEATURE ENGINEERING

# We will looke at all our 25 variables, distributions, level(for factors)



# Lets look at Education variable
plot(data$EDUCATION)

# we remark that that levels (0 , 4 , 5 and 6 reprents a tini amount of the data)
x = data$EDUCATION[data$EDUCATION == c(0,4,5,6)]
data$EDUCATION[data$EDUCATION== 4]         <- 0 
data$EDUCATION[data$EDUCATION== 5]  <- 0 
data$EDUCATION[data$EDUCATION== 6]  <- 0 

plot(data$EDUCATION)

ggplot(data, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Education') +
  theme_excel()
A = data
# From now we are using A instead of data

# Barplot
bp<- ggplot(A, aes(x=EDUCATION, fill = default.payment.next.month))+
  geom_bar(width = 1)+
  coord_polar()
qplot(AGE, data = A, geom = "density", fill = default.payment.next.month)
bp
# Thats better 


# LETS LOOK AT LIMIT_AMOUNT
# We plot a histogram for limit amount (include individuals and family credits)

plot(A$LIMIT_BAL)
# Thats better!

mean(A$LIMIT_BAL)

# 167484.3 $ 
boxplot(x = data[,2],col = "red")


# let's get a look to age variable

hist(A$AGE,col = "lightblue",freq  = 2,main = "histogram & distrubution of client's age",breaks = 10,xlab = "Age of the client", ylab = "Frequency")

ggplot(A, aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Age') +
  theme_excel()


# exploring limit_bal payment
  
hist(data$LIMIT_BAL, breaks = 20) # 10K to 1G , mean = 167484 

# RELATION BTWN DEFAULT AND EDUCATION

ggplot(data, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Education') +
  theme_excel()


# RELATION BTWN DEFAULT AND SEX

ggplot(data, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Sex') +
  theme_economist_white()


# RELATION BTWN DEFAULT AND MARIAGE

ggplot(data, aes(x = MARRIAGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Mariage') +
  theme_excel()



# RELATION BTWN LIMIT_BAL AND EDUCATION
ggplot(data, aes(x = EDUCATION, fill = LIMIT_BAL)) +
  geom_bar() +
  labs(x = 'Education') +
  labs(y = 'Limit crédit') +
  theme_few()

# relation btwn age and default

ggplot(data, aes(AGE, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 6) + 
  # I include education since we know (a priori) it's a significant predictor
  facet_grid(.~EDUCATION) + 
  theme_fivethirtyeight()

# we see clearly an important rate of default in the university category

# lets explore PAY_0 

ggplot(data, aes(PAY_0, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) + 
  # I include education since we know (a priori) it's a significant predictor
  # facet_grid(.~EDUCATION) + 
  theme_excel()

# we see that if pay_0 is btwn 1 and 6, the probability of default is a lot highter


# now let's see pay_2
ggplot(data, aes(PAY_2, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) + 
  # I include education since we know (a priori) it's a significant predictor
  # facet_grid(.~EDUCATION) + 
  theme_fivethirtyeight()
  
ggplot(data, aes(PAY_3, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) + 
  # I include education since we know (a priori) it's a significant predictor
  # facet_grid(.~EDUCATION) + 
  theme_fivethirtyeight()
  
ggplot(data, aes(PAY_4, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) + 
  # I include education since we know (a priori) it's a significant predictor
  facet_grid(.~EDUCATION) + 
  theme_fivethirtyeight()
  
 
  
  
  

  
#DIMENTIONZLITY REDUCTION

# Principal component analysis (PCA) is used to summarize the information in a data set described by multiple variables.
# PCA reduces the dimensionality of data containing a large set of variables. This is achieved by transforming the initial variables into a new small set of variables without loosing the most important information in the original data set.
  
# pca applies only to quantitative features
# we are going to explore the correlations between our Bill_amt 1 to6 , pay_amt 1 to 6 and limit_bal

df <- A[,c(2,13,14,15,16,17,18,19,20,21,22,23,24)]
head(df)


res.pca <- PCA(df, scale.unit = TRUE, graph = TRUE)



# visualisation of the correlation matrix


chart.Correlation(df, histogram=TRUE, pch=19)


print(res.pca)


head(res.pca$var$coord)

# Cos2 : quality of the representation for variables on the factor map
head(res.pca$var$cos2)


# Contributions of the variables to the principal components

# The contributions of variables in accounting for the variability in a given principal component are (in percentage) : (variable.cos2 * 100) / (total cos2 of the component)


head(res.pca$var$contrib)

# The larger the value of the contribution, the more the variable contributes to the component.


# The function dimdesc()[in FactoMineR] can be used to identify the most correlated variables with a given principal component.


dimdesc(res.pca, axes = 1:3, proba = 0.05)

res.desc <- dimdesc(res.pca, axes = c(1,2))

res.desc

res.desc$Dim.1

