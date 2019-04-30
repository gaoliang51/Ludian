library("C50")
library("nnet")
library("caret")
library("neuralnet")
library("pROC")

# read and pre-process data
data <- read.csv("C:\\Temporary data\\LudianData\\R_Data\\Data_8.csv")
data$FID <- NULL
data$Y <- factor(data$Y)
data$Lithology <- factor(data_train$Lithology)
data$The.distance.to.roads <- factor(data$The.distance.to.roads)
data$Soil.type <- factor(data$Soil.type)
data$The.distance.to.faults <- factor(data$The.distance.to.faults)
data$The.distance.to.rivers <- factor(data$The.distance.to.river)
data$TWI <- factor(data$TWI)

# generate the train and test data
ind<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
data_train<- data [ind==1,]
data_test<- data [ind==2,]

# model generation and evaluation
data_model_c50 <- C5.0(Y ~ ., data = data_train, trials = 20, model = rules, winnow = FALSE)
data_pred_c50 <- predict(data_model_c50, data_test, type = "prob")
pred_value_c50 <- data_pred_c50[, 2]
modelroc <- roc(as.integer(data_test$Y),pred_value_c50)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

data_model_mlgSet2 <- glm(Y~Anger + Lithology + Fluctuation + Aspect + DEM + NDVI + PGA 
                          + The.distance.to.roads + Soil.type + The.distance.to.faults 
                          + The.distance.to.rivers + TWI,family=binomial(link='logit'),data=trainSet2)
data_pred_neuralnet <- compute(data_model_neuralnet, data_test)
pred_value_neu <- data_pred_neuralnet$net.result
modelroc <- roc(as.integer(data_test$Y),pred_value_neu)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

data_model_nnet <- nnet(Y~., data=trainSet, size =model.jc.nnt$bestTune[1,1],
                        decay = model.jc.nnt$bestTune[1,2])
data_pred_nnet <-predict(data_model_nnet,data_test, type = "class")
pred_value_nnet <- data_pred_nnet[, 1]
modelroc <- roc(as.integer(data_test$Y),pred_value_nnet)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)