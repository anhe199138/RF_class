# import required packages
library(readxl)
library(party)
library(randomForest)
library(pROC)
require (MASS)


# import training and testing data
training <- read_excel("Prediction_RandomForest/training_n.xlsx")
testing  <- read_excel("Prediction_RandomForest/testing.xlsx")


# features (x) and corresponding labels (y) in training set
# method 1 to read features
x_train <- training[, c("mean0","sd0","sd2","sd3","sd4","sd5","sd6",
                        "entropy0","entropy2","entropy3","entropy4","entropy5","entropy6",
                        "mpp0","mpp2","mpp3","mpp4","mpp5","mpp6","age")]
# method 2 to read features
# x_test <- cbind(array(data = testing[["mean0"]], dim = c(62,1)),
#                 array(data = testing[["sd0"]], dim = c(62,1)),
#                 array(data = testing[["entropy0"]], dim = c(62,1)),
#                 array(data = testing[["mpp0"]], dim = c(62,1)))

# method 1 to read label
y_train_PR <- factor(training[["PR"]],labels = c(0,1))
y_train_ER <- factor(training[["ER"]],labels = c(0,1))
y_train_WT1 <- factor(training[["WT1"]],labels = c(0,1))
y_train_histype <- training[["histype"]]
# method 2 to read label
## "training[, c("PR")]" is a 1D list. Only vector can be used as the first parameter in function factor.
## Convert 1D list into 1D vector: " vector = as.vector(unlist(list)) "
# y_train_PR <- factor(as.vector(unlist(training[, c("PR")])),labels = c(0,1))
# y_train_histype <- as.vector(unlist(training[, c("histype")]))


# features (x) and corresponding labels (y) in testing set
x_test <- testing[, c("mean0","sd0","sd2","sd3","sd4","sd5","sd6",
                      "entropy0","entropy2","entropy3","entropy4","entropy5","entropy6",
                      "mpp0","mpp2","mpp3","mpp4","mpp5","mpp6","age")]

y_test_PR <- factor(testing[["PR"]],labels = c(0,1))
y_test_ER <- factor(testing[["ER"]],labels = c(0,1))
y_test_WT1 <- factor(testing[["WT1"]],labels = c(0,1))
y_test_histype <- testing[["histype"]]


set.seed(42)


# Create the forest for PR 
# Method 1
RF_model_PR <- randomForest(as.factor(PR) ~ 
                                #mean0+mean2+mean3+mean4+mean5+mean6+
                                #sd0+sd2+sd3+sd4+sd5+sd6+
                                #entropy0+entropy2+entropy3+entropy4+entropy5+entropy6+
                                #mpp0+mpp2+mpp3+mpp4+mpp5+mpp6+
                                #kurtosis0+kurtosis2+kurtosis3+kurtosis4+kurtosis5+kurtosis6+
                                #skewness0+skewness2+skewness3+skewness4+skewness5+skewness6+
                                #age,
                                mean0+sd0+sd2+sd3+sd4+sd5+sd6+
                                entropy0+entropy2+entropy3+entropy4+entropy5+entropy6+
                                mpp0+mpp2+mpp3+mpp4+mpp5+mpp6+age,
                                data = training, 
                                importance=TRUE, ntree = 1000)
# Method 2
# RF_model_PR <- randomForest(x=x_train, y=y_train, importance=TRUE, ntree = 1000)

# Create the forest for ER 
RF_model_ER <- randomForest(as.factor(ER) ~ 
                              mean0+sd0+sd2+sd3+sd4+sd5+sd6+
                              entropy0+entropy2+entropy3+entropy4+entropy5+entropy6+
                              mpp0+mpp2+mpp3+mpp4+mpp5+mpp6+age,
                            data = training, 
                            importance=TRUE, ntree = 1000)

# Create the forest for WT1
RF_model_WT1 <- randomForest(as.factor(WT1) ~ 
                              mean0+sd0+sd2+sd3+sd4+sd5+sd6+
                              entropy0+entropy2+entropy3+entropy4+entropy5+entropy6+
                              mpp0+mpp2+mpp3+mpp4+mpp5+mpp6+age,
                            data = training, 
                            importance=TRUE, ntree = 1000)

# Creat the forest for histype
RF_model_histype <- randomForest(as.factor(histype) ~ 
                              mean0+sd0+sd2+sd3+sd4+sd5+sd6+
                              entropy0+entropy2+entropy3+entropy4+entropy5+entropy6+
                              mpp0+mpp2+mpp3+mpp4+mpp5+mpp6+age,
                            data = training, 
                            importance=TRUE, ntree = 1000)

# View the forest results.
# print(output.forest) 

# Importance of each predictor
# print(importance(output.forest,type = 2)) 

# Input OOB values
# output.predict = predict(output.forest,newdata = testing,interval="predict")
# table(output.predict,testing$PR)

# Plot decrease gini
# varImpPlot(output.forest,type=2)


# Get confusion matrix from the outcome of model
# Each Raw represents the numbers of right and wrong predictions respectively
# ConfusionMatrix <- output.forest[["confusion"]]
# View(ConfusionMatrix)
# Calculate TP, FP, FN, TN, TPR, FPR
# TP <- ConfusionMatrix[1,1]
# FP <- ConfusionMatrix[1,2]
# FN <- ConfusionMatrix[2,1]
# TN <- ConfusionMatrix[2,2]
# TPR <- TP/(TP+FN)
# FPR <- FP/(FP+TN)


# Performance of model in testing dataset
## type: "response", "prob" or "votes"
## indicating the type of output: predicted values, matrix of class probabilities, or matrix of vote counts. 
## Class is allowed, but automatically converted to "response", for backward compatibility.
## Here, type = "prob" or "vote", same outcome
pred_RF_PR <- predict(RF_model_PR, newdata = x_test, type = "prob")
pred_RF_ER <- predict(RF_model_ER, newdata = x_test, type = "prob")
pred_RF_WT1 <- predict(RF_model_WT1, newdata = x_test, type = "prob")
pred_RF_histype <- predict(RF_model_histype, newdata = x_test, type = "prob")


# method 1 and 2 to calculate auc
# Remember: 1 and 2 all based on sensitivity(TPR) and specificity(1-FPR)
# Calculate ROC, AUC for PR
model_roc_PR <- roc(y_test_PR ~ pred_RF_PR[,c("+")])
model_auc_PR <- model_roc_PR$auc        # method 1 to calculate auc defined in trained model "RF_model_PR"
View(model_auc_PR)

# Calculate ROC, AUC for ER
model_roc_ER <- roc(y_test_ER ~ pred_RF_ER[,c("+")])
model_auc_ER <- auc(model_roc_ER)       # method 2 to calculate auc defined in trained model "RF_model_ER"
View(model_auc_ER)

# Calculate ROC, AUC for WT1
model_roc_WT1 <- roc(y_test_WT1 ~ pred_RF_WT1[,c("+")])
model_auc_WT1 <- model_roc_WT1$auc
View(model_auc_WT1)

# Calculate ROC, AUC for histype
model_roc_histype <- roc(y_test_histype ~ pred_RF_histype[,c(1)])
model_auc_histype <- model_roc_histype$auc
View(model_auc_histype)

# method 3 to calculate auc, using defined algorithm based on TPR and FPR
TPR_PR <- model_roc_PR[["sensitivities"]]
Specificity <- model_roc_PR[["specificities"]]
FPR_PR <- 1 - Specificity  
m3_auc <- 0
for(i in c(1:60)){
  m3_auc <- m3_auc + TPR_PR[i] * (FPR_PR[i] - FPR_PR[i+1])
}

# Plot AUC figure (unfinished!!!)
## In pROC package, roc function returns Sensitivities and Specificities.
## Sensitivity = TPR, Specificity = 1-FPR
## So diagram remain unchanged when TPR and FPR replace Sensitivity and Specificty.
## But titles and scales of axises need to be modified. 
roc_obj <- plot.roc(model_roc_PR)
plot(model_roc)


# View(model_roc)
# model_roc[["specificities"]]
# model_roc[["thresholds"]]
# roc_PR <- roc(training[["histype"]], RF_model_PR[["votes"]][,1])
# a <- auc(roc_PR)
# mode(a)
# View(a)
# identical(a, b)
# numeric(vector?) = as.factor(character) 
