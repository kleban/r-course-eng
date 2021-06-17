library(caret)
my_data <- read.csv("data/scoring.csv", stringsAsFactors = T)
my_data <- my_data %>% mutate(Status = ifelse(Status == "bad", 1 , 0))
target_variable = "Status" 

trainIndex <- createDataPartition(my_data[, target_variable], p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
train_set <- list()
test_set <- list()
train_set$data <- my_data[trainIndex, ]
test_set$data <- my_data[-trainIndex, ]

trainctrl <- trainControl(verboseIter = TRUE) # progress

glmFit<- train(as.formula(paste(target_variable, "~ .")), 
               data = train_set$data, method = "glm", 
               trControl = trainctrl)
glmFit

imp <- varImp(glmFit)
plot(imp)
test_set$prediction_glm <- predict(glmFit, newdata = test_set$data)

cm <- caret::confusionMatrix(factor(test_set$data$Status),  factor(ifelse(test_set$prediction_glm > 0.5, 1, 0)), positive = "1")
cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]

library(InformationValue)
InformationValue::plotROC(test_set$data$Status, test_set$prediction_glm)

nnFit<- train(as.formula(paste(target_variable, "~ .")), 
              data = train_set$data, 
              method = "nnet", 
              trControl = trainctrl)
nnFit


imp <- varImp(nnFit)
plot(imp)
test_set$prediction_nn <- predict(nnFit, newdata = test_set$data)

cm <- caret::confusionMatrix(factor(test_set$data$Status),  factor(ifelse(test_set$prediction_nn > 0.5, 1, 0)), positive = "1")
cm$table
cm$overall["Accuracy"]
cm$byClass["Balanced Accuracy"]

library(InformationValue)
InformationValue::plotROC(test_set$data$Status, test_set$prediction_nn)

