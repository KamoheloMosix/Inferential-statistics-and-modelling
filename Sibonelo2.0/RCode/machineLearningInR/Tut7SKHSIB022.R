
library(foreach)

# functions I need

Kfoldsplit <- function(n_obs, K){
  
  # number of observations per window
  window <- n_obs %/% K
  
  origin <- sample(1:n_obs, n_obs, replace = F)
  indexes <- matrix(origin, nrow=K, ncol =window, byrow=T)
  
  # in R to return we just type in the thingy we wanna return
  indexes
  
}

PolyCV <- function(data, folds, polyNom, is_median = F){
  
  n.folds <- dim(folds)[1]
  
  MSE <- foreach(fold = 1:n.folds, .combine = c) %do% {
    
    train_ind <- c( folds[-fold, ])
    val_ind <- c(folds[fold, ])
    
    train.data <- data[train_ind, ]
    test.data <- data[val_ind, ]
    
    model <- lm(Y ~ poly(X, polyNom, raw =TRUE), data = train.data)
    
    #get the MSE of the model
    MSE <- mean( (test.data$Y - predict(model, newdata = test.data))^2)
    
    
    
  }
  
  # get mean of the entire model as a whole
  ifelse(is_median, median(MSE), mean(MSE))
}

# Question 1 --------------------------------------------------------------


# WE may now begin conduction of the test

TutData <- read.csv("data/TutWk7Data.csv")

n_observations <- nrow(TutData)

# set the seed
set.seed(2026)

polynomials <- c(1:5)
sets <- Kfoldsplit(n_observations, 5)
MSE_ALL_K <- foreach(polynomial = polynomials, .combine = c) %do% {
  
  PolyCV(data = TutData, folds = sets, polyNom = polynomial )
  
  
}

plot(seq(5), MSE_ALL_K, type = "b", pch = 19, col = "steelblue", xlab = "Polynomial Degree", ylab = "Cross-Validated MSE", main = "5-Fold CV Error vs Model Complexity", xaxt = "n")

axis(1, at = 1:5, labels = 1:5)
# vertical line highlighting the best model
abline(v = which.min(MSE_ALL_K), lty = 2, col = "red")


# Question 2 --------------------------------------------------------------



worstModel <-lm(Y ~ poly(X,1, raw =TRUE), data = TutData)

bestModel <- lm(Y ~ poly(X,3, raw =TRUE), data = TutData)

# data i will use for the predictions
feats <- data.frame(X = TutData$X)

# Worst model, with degree 1 -- Too simple
predWorst <- predict(worstModel, feats)

errorWorst <- mean( (TutData$Y - predWorst)^2)


# Best model, with degree 3 -- balanced
predBest <- predict(bestModel, feats)

errorBest <- mean( (TutData$Y - predBest)^2)

# 2.2
x_grid <- seq(min(TutData$X), max(TutData$X), length=500)
new_data <- data.frame(X = x_grid)  

bestLine <- predict(bestModel, newdata = new_data)
worstLine <- predict(worstModel, newdata = new_data)

plot(TutData$X, TutData$Y, pch  = 19, col = "grey60", xlab = "X", ylab = "Y", main = "Degree 1 vs Degree 3 Polynomial Fit")

lines(x_grid, bestLine, col = 'blue', lwd = 2)
lines(x_grid, worstLine, col = 'red', lwd =2)
