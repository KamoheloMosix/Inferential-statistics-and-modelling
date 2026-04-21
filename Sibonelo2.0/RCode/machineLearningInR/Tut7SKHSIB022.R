
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

# WE may now begin conduction of the test

TutData <- read.csv("data/TutWk7Data.csv")

n_observations <- nrow(TutData)

# set the seed
set.seed(2026)

polynomials <- c(1:5)
sets <- Kfoldsplit(n_observations, 5)
MSE_ALL_K <- foreach(polynomial = polynomials, .combine = c) %do% {
  
  PolyCV(data = TutData, folds = sets, poly = polynomial )
  
  
}

plot(seq(5), MSE_ALL_K)
lines(seq(5), MSE_ALL_K)


