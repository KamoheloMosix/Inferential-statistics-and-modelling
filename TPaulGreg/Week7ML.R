library(foreach)

# Functions used for the cross-validation

Kfoldsplit <- function(n_obs, K){
  
  # how many observations go into each fold
  window <- n_obs %/% K
  
  # randomly shuffle the row numbers
  origin <- sample(1:n_obs, n_obs, replace = F)
  indexes <- matrix(origin, nrow=K, ncol =window, byrow=T)
  
  # return the fold indices
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
    
    # calculate the MSE on the validation fold
    MSE <- mean( (test.data$Y - predict(model, newdata = test.data))^2)
    
  }
  
  # return either the mean MSE or median MSE
  ifelse(is_median, median(MSE), mean(MSE))
}

# Read in the tutorial data

TutData <- read.csv("data/TutWk7Data.csv")

library(foreach)

# use the seed given in the tutorial
set.seed(2026)

# polynomial degrees to compare
polynomials <- 1:5

# total number of observations
n_observations <- nrow(TutData)

# create the 5 folds
sets <- Kfoldsplit(n_observations, 5)

# get the CV error for each polynomial degree
MSE_ALL_K <- foreach(polynomial = polynomials, .combine = c) %do% {
  PolyCV(data = TutData, folds = sets, poly = polynomial)
}

MSE_ALL_K

# plot CV error against polynomial degree
plot(polynomials, MSE_ALL_K,
     type = "b", pch = 19,
     xlab = "Polynomial degree",
     ylab = "Estimated mean error",
     main = "5-fold CV error vs model complexity")

# fit the worst model using all the data
worst_model <- lm(Y ~ poly(X, 1), data = TutData)

# fit the best model using all the data
best_model <- lm(Y ~ poly(X, 3), data = TutData)

# calculate training errors for both models
worst_error <- mean(residuals(worst_model)^2)
best_error  <- mean(residuals(best_model)^2)

worst_error
best_error

# make a grid of x-values so the fitted curves look smooth
x_grid <- seq(min(TutData$X), max(TutData$X), length.out = 500)

# get predictions from both fitted models
y_worst <- predict(worst_model, newdata = data.frame(X = x_grid))
y_best  <- predict(best_model,  newdata = data.frame(X = x_grid))

# plot the raw data
plot(Y ~ X, data = TutData, pch = 19,
     main = "Best and worst polynomial models",
     xlab = "X", ylab = "Y")

# add the fitted curves
lines(x_grid, y_worst, col = "red", lwd = 2)
lines(x_grid, y_best,  col = "blue", lwd = 2)

legend("topleft",
       legend = c("Worst model: degree 1", "Best model: degree 3"),
       col = c("red", "blue"),
       lwd = 2,
       bty = "n")