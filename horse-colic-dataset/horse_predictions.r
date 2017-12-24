s#R program for predicting the condition of horses(alive/dead)

setwd("C:\\Users\\Sandesh Nayak\\Desktop\\Study\\Machine Learning Resources\\Kaggle Projects\\horse-colic-dataset")
horse_data <- read.csv("horse.csv")
############################################Data Preparation#######################################################
horse_data[!complete.cases(horse_data$rectal_temp),]$rectal_temp = mean(horse_data$rectal_temp,na.rm=T)
horse_data[!complete.cases(horse_data$pulse),]$pulse = mean(horse_data$pulse,na.rm=T)
horse_data[!complete.cases(horse_data$respiratory_rate),]$respiratory_rate = mean(horse_data$respiratory_rate,na.rm=T)
horse_data[!complete.cases(horse_data$temp_of_extremities),]$temp_of_extremities = "cool"
horse_data[!complete.cases(horse_data$peripheral_pulse),][1:35,]$peripheral_pulse = "normal"
horse_data[!complete.cases(horse_data$peripheral_pulse),][1:34,]$peripheral_pulse = "reduced"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:15,]$mucous_membrane = "normal_pink"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:10,]$mucous_membrane = "pale_pink"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:8,]$mucous_membrane = "pale_cyanotic"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:5,]$mucous_membrane = "bright_pink"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:5,]$mucous_membrane = "bright_red"
horse_data[!complete.cases(horse_data$mucous_membrane),][1:4,]$mucous_membrane = "dark_cyanotic"
horse_data[!complete.cases(horse_data$capillary_refill_time),][1:22,]$capillary_refill_time = "less_3_sec"
horse_data[!complete.cases(horse_data$capillary_refill_time),][1:10,]$capillary_refill_time = "more_3_sec"
horse_data[!complete.cases(horse_data$peristalsis),][1:20,]$peristalsis = "hypomotile"
horse_data[!complete.cases(horse_data$peristalsis),][1:12,]$peristalsis = "absent"
horse_data[!complete.cases(horse_data$peristalsis),][1:8,]$peristalsis = "hypermotile"
horse_data[!complete.cases(horse_data$peristalsis),][1:4,]$peristalsis = "normal"
horse_data[!complete.cases(horse_data$pain),][1:18,]$pain = "mild_pain"
horse_data[!complete.cases(horse_data$pain),][1:15,]$pain = "depressed"
horse_data[!complete.cases(horse_data$pain),][1:10,]$pain = "extreme_pain"
horse_data[!complete.cases(horse_data$pain),][1:6,]$pain = "alert"
horse_data[!complete.cases(horse_data$pain),][1:6,]$pain = "severe_pain"
horse_data[!complete.cases(horse_data$nasogastric_reflux),][1:80,]$nasogastric_reflux = "none"
horse_data[!complete.cases(horse_data$nasogastric_reflux),][1:13,]$nasogastric_reflux = "more_1_liter"
horse_data[!complete.cases(horse_data$nasogastric_reflux),][1:13,]$nasogastric_reflux = "less_1_liter"
horse_data[!complete.cases(horse_data$nasogastric_tube),][1:55,]$nasogastric_tube = "slight"
horse_data[!complete.cases(horse_data$nasogastric_tube),][1:35,]$nasogastric_tube = "none"
horse_data[!complete.cases(horse_data$nasogastric_tube),][1:14,]$nasogastric_tube = "significant"
horse_data[!complete.cases(horse_data$nasogastric_reflux_ph),]$nasogastric_reflux_ph = 5
horse_data[!complete.cases(horse_data$rectal_exam_feces),][1:40,]$rectal_exam_feces = "absent"
horse_data[!complete.cases(horse_data$rectal_exam_feces),][1:30,]$rectal_exam_feces = "normal"
horse_data[!complete.cases(horse_data$rectal_exam_feces),][1:27,]$rectal_exam_feces = "decreased"
horse_data[!complete.cases(horse_data$rectal_exam_feces),][1:5,]$rectal_exam_feces = "increased"
horse_data[!complete.cases(horse_data$abdomen),][1:45,]$abdomen = "distend_large"
horse_data[!complete.cases(horse_data$abdomen),][1:30,]$abdomen = "distend_small"
horse_data[!complete.cases(horse_data$abdomen),][1:20,]$abdomen = "normal"
horse_data[!complete.cases(horse_data$abdomen),][1:14,]$abdomen = "other"
horse_data[!complete.cases(horse_data$abdomen),][1:9,]$abdomen = "firm"
horse_data[!complete.cases(horse_data$packed_cell_volume),]$packed_cell_volume = 45
horse_data[!complete.cases(horse_data$total_protein),]$total_protein = 7.5
horse_data[!complete.cases(horse_data$abdomo_appearance),][1:55,]$abdomo_appearance = "clear"
horse_data[!complete.cases(horse_data$abdomo_appearance),][1:55,]$abdomo_appearance = "cloudy"
horse_data[!complete.cases(horse_data$abdomo_appearance),][1:55,]$abdomo_appearance = "serosanguious"
horse_data[!complete.cases(horse_data$abdomo_protein),]$abdomo_protein = 2.30
horse_data$hospital_number <- as.factor(horse_data$hospital_number)


##########################################Decision Tree##########################################################

library(C50)
set.seed(123)
train <- sample(299,200)
training_data <- horse_data[train,]
test_data <- horse_data[-train,]
horse_model <- C5.0(training_data[c(-3,-23)],training_data$outcome)
horse_pred <- predict(horse_model,test_data)
table(horse_pred,test_data$outcome)

########################################Random Forest###########################################################

library(randomForest)
set.seed(300)
train <- sample(299,200)
training_data <- horse_data[train,]
test_data <- horse_data[-train,]
horse_model <- randomForest(outcome ~ .,data = training_data[-3], na.action = na.exclude, mtry = 16)

######################################Boost Random Forest using Caret##########################################

library(caret)
set.seed(300)
train <- sample(299,200)
training_data <- horse_data[train,]
test_data <- horse_data[-train,]
ctrl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
model_rf <- train(outcome ~ ., data = training_data[-3], method = "rf", metric = "Kappa", trControl = ctrl, tuneGrid = grid_rf, na.action = na.exclude)



