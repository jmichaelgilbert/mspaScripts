# Load packages
library(caret)

# Load dataset
iris = datasets::iris

# Check structure
str(iris)

# Specify fit parameters
iris.svm.fc = trainControl(method = "cv",
                           number = 5,
                           classProbs = T)

#--------------------------------------
# Model 1
#--------------------------------------
# Model notes:
#   warnings() == F
#   sigma == constant
#   C == varied, 9 times, each = 1
#   Weight == NULL

# Build model
ptm = proc.time()
set.seed(123)
iris.svm.m1 = train(x = iris[, -5],
                    y = iris[, 5],
                    method = "svmRadial",
                    preProcess = c("center", "scale"),
                    trControl = iris.svm.fc,
                    tuneLength = 9)
proc.time() - ptm; rm(ptm)

# In-sample summary
iris.svm.m1$finalModel
iris.svm.m1$results

# Plots
plot(iris.svm.m1, main = "Accuracy: iris.svm.m1")
plot(varImp(iris.svm.m1), main = "Var Imp: iris.svm.m1")

# In-sample fit
iris.svm.m1.trn.pred = predict(iris.svm.m1, newdata = iris[, -5])
iris.svm.m1.trn.cm = confusionMatrix(iris.svm.m1.trn.pred, iris$Species)
iris.svm.m1.trn.cm$table
iris.svm.m1.trn.cm$overall[1:2]

#--------------------------------------
# Model 2
#--------------------------------------
# Model notes:
#   warnings() == F
#   sigma == NULL
#   C == varied, 9 times, each = 1
#   Weight == NULL

# Build model
ptm = proc.time()
set.seed(123)
iris.svm.m2 = train(x = iris[, -5],
                    y = iris[, 5],
                    method = "svmRadialCost",
                    preProcess = c("center", "scale"),
                    trControl = iris.svm.fc,
                    tuneLength = 9)
proc.time() - ptm; rm(ptm)

# In-sample summary
iris.svm.m2$finalModel
iris.svm.m2$results

# Plots
plot(iris.svm.m2, main = "Accuracy: iris.svm.m2")
plot(varImp(iris.svm.m2), main = "Var Imp: iris.svm.m2")

# In-sample fit
iris.svm.m2.trn.pred = predict(iris.svm.m2, newdata = iris[, -5])
iris.svm.m2.trn.cm = confusionMatrix(iris.svm.m2.trn.pred, iris$Species)
iris.svm.m2.trn.cm$table
iris.svm.m2.trn.cm$overall[1:2]

#--------------------------------------
# Model 3
#--------------------------------------
# Model notes:
#   warnings() == F
#   sigma == varied, 6 times, each = 9
#   C == varied, 9 times, each = 6
#   Weight == NULL

# Build model
ptm = proc.time()
set.seed(123)
iris.svm.m3 = train(x = iris[, -5],
                    y = iris[, 5],
                    method = "svmRadialSigma",
                    preProcess = c("center", "scale"),
                    trControl = iris.svm.fc,
                    tuneLength = 9)
proc.time() - ptm; rm(ptm)

# In-sample summary
iris.svm.m3$finalModel
iris.svm.m3$results

# Plots
plot(iris.svm.m3, main = "Accuracy: iris.svm.m3")
plot(varImp(iris.svm.m3), main = "Var Imp: iris.svm.m3")

# In-sample fit
iris.svm.m3.trn.pred = predict(iris.svm.m3, newdata = iris[, -5])
iris.svm.m3.trn.cm = confusionMatrix(iris.svm.m3.trn.pred, iris$Species)
iris.svm.m3.trn.cm$table
iris.svm.m3.trn.cm$overall[1:2]

#--------------------------------------
# Model 4
#--------------------------------------
# Model notes:
#   warnings() == T
#   sigma == constant
#   C == varied, 9 times
#   Weight == ifelse(Weight == 1, T, F)

# Build model
ptm = proc.time()
set.seed(123)
iris.svm.m4 = train(x = iris[, -5],
                    y = iris[, 5],
                    method = "svmRadialWeights",
                    preProcess = c("center", "scale"),
                    trControl = iris.svm.fc,
                    tuneLength = 9)
proc.time() - ptm; rm(ptm)

# In-sample summary
iris.svm.m4$finalModel
iris.svm.m4$results

# Plots
plot(iris.svm.m4, main = "Accuracy: iris.svm.m4")
plot(varImp(iris.svm.m4), main = "Var Imp: iris.svm.m4")

# In-sample fit
iris.svm.m4.trn.pred = predict(iris.svm.m4, newdata = iris[, -5])
iris.svm.m4.trn.cm = confusionMatrix(iris.svm.m4.trn.pred, iris$Species)
iris.svm.m4.trn.cm$table
iris.svm.m4.trn.cm$overall[1:2]

#--------------------------------------
# Model 5
#--------------------------------------
# Model notes:
#   warnings() == F
#   sigma == varied, 30 times, each = 1
#   C == varied, 30 times, each = 1
#   Weight == NULL

# Specify fit parameters
iris.svm.m5.fc = trainControl(method = "cv",
                              number = 5,
                              classProbs = T,
                              search = "random")

# Build model
ptm = proc.time()
set.seed(123)
iris.svm.m5 = train(x = iris[, -5],
                    y = iris[, 5],
                    method = "svmRadialSigma",
                    preProcess = c("center", "scale"),
                    trControl = iris.svm.m5.fc,
                    tuneLength = 30)
proc.time() - ptm; rm(ptm)

# In-sample summary
iris.svm.m5$finalModel
iris.svm.m5$results

# In-sample fit
iris.svm.m5.trn.pred = predict(iris.svm.m5, newdata = iris[, -5])
iris.svm.m5.trn.cm = confusionMatrix(iris.svm.m5.trn.pred, iris$Species)
iris.svm.m5.trn.cm$table
iris.svm.m5.trn.cm$overall[1:2]

#--------------------------------------
# Model Comparison
#--------------------------------------

# Model Types
model.types = cbind(c(rep("SVM", each = 5)))

# Model Names
model.names = c("M1", "M2", "M3", "M4", "M5")

# Accuracy, Train
model.trn.acc = rbind(iris.svm.m1.trn.cm$overall[1],
                      iris.svm.m2.trn.cm$overall[1],
                      iris.svm.m3.trn.cm$overall[1],
                      iris.svm.m4.trn.cm$overall[1],
                      iris.svm.m5.trn.cm$overall[1])

# Kappa, Train
model.trn.kpp = rbind(iris.svm.m1.trn.cm$overall[2],
                      iris.svm.m2.trn.cm$overall[2],
                      iris.svm.m3.trn.cm$overall[2],
                      iris.svm.m4.trn.cm$overall[2],
                      iris.svm.m5.trn.cm$overall[2])

# Data Frame
model.comp = data.frame(model.types,
                        model.names,
                        model.trn.acc,
                        model.trn.kpp)
rownames(model.comp) = 1:nrow(model.comp)
colnames(model.comp) = c("Model Type",
                         "Model Name",
                         "Train: Accuracy",
                         "Train: Kappa")
model.comp

# Session info
sessionInfo()
