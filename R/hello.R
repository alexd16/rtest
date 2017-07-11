# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  #install packages MASS, neuralnet
  # Set a seed
  set.seed(500)
  library(MASS)
  data <- Boston
  # Check that no data is missing
  apply(data,2,function(x) sum(is.na(x)))
  # Train-test random splitting for linear model
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  # Fitting linear model
  lm.fit <- glm(medv~., data=train)
  summary(lm.fit)
  # Predicted data from lm
  pr.lm <- predict(lm.fit,test)
  # Test MSE
  MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
  #-------------------------------------------------------------------------------
  # Neural net fitting
  # Scaling data for the NN
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  # Train-test split
  train_ <- scaled[index,]
  test_ <- scaled[-index,]
  # NN training
  library(neuralnet)
  n <- names(train_)
  f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
  nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
  # Visual plot of the model
  # plot(nn)
  # Predict
  pr.nn <- compute(nn,test_[,1:13])
  # Results from NN are normalized (scaled)
  # Descaling for comparison
  pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  # Calculating MSE
  MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
  # Compare the two MSEs
  print(paste(MSE.lm,MSE.nn))
}
