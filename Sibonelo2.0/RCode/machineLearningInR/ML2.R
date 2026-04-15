#April 2025 
#ML demo on splitting data, measuring error, CV

#####################################
#functions
#####################################

#splits data into test and training
SplitData <- function(data, splits){
  
  df <- as.data.frame(data)                 # Convert the data object to a data frame object
  
  n.obs <- nrow(df)                             # Number of observations
  
  # Shuffle and separate indices
  
  shuffled_ind <- sample(1:n.obs)
  
  split_ind <- c(round(splits[1]*n.obs), round(splits[1]*n.obs) + round(splits[2]*n.obs))
  
  train_ind <- shuffled_ind[1:split_ind[1]]
  val_ind <- shuffled_ind[(split_ind[1] + 1):split_ind[2]]
  if (splits[3]!=0){
    test_ind <- shuffled_ind[(split_ind[2] + 1):n.obs]
    list("train" = df[train_ind, ], 
         "validation" = df[val_ind, ], 
         "test" = df[test_ind, ])  
  } else {
    list("train" = df[train_ind, ], 
         "validation" = df[val_ind, ])  
  }
}

#splits data into K folds and returns a matrix of mashed up indexes basically
Kfoldsplit <- function(n.obs, k){
  
  # Number of observations per fold
  
  wind <- n.obs%/%k     # integer division operator, rounds result down to the nearest whole number)            
  
  # generate random order of observations without replacement
  origin <- sample(1:n.obs, n.obs, replace = F)

  # Organises indices into a matrix to define k-folds (each fold corresponds to a row)
  indexes <- matrix(origin, k, wind, byrow = T)
  indexes
}

#performs CV for a specified model
#calcs errors and produces plots
#data needs columns named as X and Y
cross_val <- function(data, k = 5, poly = 2){
  
  N <- dim(data)[1]                     # Total number of observations 
  sets <- Kfoldsplit(N, k)                # Fold splits (i.e. indices contained in each fold)
  
  # A: Empty result matrices
  
  
  # create val.error and train.error to be a list with 'k' zeros
  val.error <- rep(0, k)                # Validation error
  train.error <- rep(0, k)              # Training error
  
  # B: Cross-validation
  
  for(i in 1:dim(sets)[1]){                      # For i in 1 : Number of folds
    
    # Separate training and validation indices and create data vectors
    
    # saying list[-i, ], removes the row at index i, so pretty cool
    train_ind <- c(sets[-i, ])                   # Vector of training indices
    val_ind <- sets[i, ]                         # Vector of validation indices
    train.y <- data[train_ind,"Y"]                      # Training Ys
    train.x <- data[train_ind,"X"]                      # Training Xs
    val.y <- data[val_ind,"Y"]                          # Validation Xs
    val.x <- data[val_ind,"X"]                          # Validation Ys

    # Train and evaluate a model
    model1 <- lm(train.y ~ poly(train.x, poly))      # Your model
    
    val.error[i] <- mean((predict(model1, newdata = data.frame(train.x = val.x)) - val.y)^2) # Validation error
    train.error[i]  = mean(residuals(model1)^2)                                              # Training error
    
    x.vec <- seq(0,10, length = 1000)
    fhat <- predict(model1, newdata = data.frame(train.x = x.vec))
    
    # Plot output
    cols = c('#222299','grey75')
    ylimit <- c(min(train.y, val.y), max(train.y, val.y))
    xlimit <- c(min(train.x, val.x), max(train.x, val.x))
    
    # Setup plot layout for multiple plots
    m <- matrix(c(1,2,3,4),nrow = 2,ncol = 2,byrow = TRUE)
    layout(mat = m, heights = c(0.5,0.5))

    #plot training data
    plot(train.y ~ train.x,                            # plot: training data sets 
         pch = 20,                                     # set plotting symbol
         xlab = "Training X's", ylab = "Training Y's", # label X and Y axes 
         main = paste0('Fold ', i, " excluded"),
         col = cols[1], ylim = ylimit, xlim = xlimit)
    
    lines(fhat ~ x.vec, col = 'firebrick')            # overlay the fitted line
    
    #plot validation data
    plot(val.y ~ val.x,                                # plot: validation set 
         pch = 20,                                     # set plotting symbol
         xlab = "Validation X's", ylab = "Validation Y's", # label X and Y axes 
         main = paste0('Validation fold ', i),
         col = cols[2], ylim = ylimit, xlim = xlimit)

    lines(fhat ~ x.vec, col = 'firebrick')            # overlay the fitted line
    
    # > Error
    plot(val.error,                              
         type = 'n', 
         ylab = 'Error', 
         xlab = 'Validation Run', 
         ylim = c(0,max(train.error, val.error)+0.15),
         main = ' Errors')
    
    lines(val.error[1:i], type = 'b', col = cols[2], pch = 16)     # Validation error lines ["b" = points and lines]
    lines(train.error[1:i], type = 'b', col = cols[1], pch = 16)    # Training error lines
    
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend('top', legend = c('Train', 'Validation'), col = cols, 
           pch = 16, bty = 'y', horiz = F)
    
    Sys.sleep(0.75)                                # Pause operation (to allow you to see each fold's plots)
    
  }
  
  c("train" = mean(train.error), "validation" = mean(val.error))
}

#alt version that uses foreach
#is passed a matrix of indices for the folds
PolyCV <- function(data, folds, poly = 2, is_median = F){
  
  n.folds <- dim(folds)[1]
  # Perform CV
  MSE <- foreach(m = 1:n.folds, .combine = c) %do% {
    
    train_ind <- c(folds[-m, ])                     # Vector of training indices
    val_ind <- folds[m, ]                           # Vector of validation indices
    train.data <- data[train_ind,]                  # Training data
    test.data <- data[val_ind,]                     # Test data

    model <- lm(Y ~ poly(X, poly),  data = train.data)               # Fit model to training data
    MSE <- mean((predict(model, newdata = test.data) - test.data$Y)^2)  # Evaluate the model on the test data (also returns test MSE)
  }                          
  ifelse(is_median,                       # Calculate mean of the collated MSE vector
         median(MSE), 
         mean(MSE))    
} 

######################################
#1. simulate data, split into train / test, fit model, calc errors
set.seed(5555)
n.points = 150 ; n.folds = 10

X <- rnorm(n.points, 5, 1.5)
Y <- 17 + sin(-0.5*X) + rnorm(n.points, sd = 0.4) 
df <- data.frame(X = X, Y = Y)
plot(Y ~ X, data = df, pch = 16)

# split into training, validation and test data
splits <- c(0.7, 0.3, 0)

data_splits <- SplitData(data = df, splits)

str(data_splits)
data_splits
train.data <- data_splits[[1]]
test.data <- data_splits[[2]]

#plot training data, train specified model, overlay fitted line, calc error
plot(Y ~ X, data = train.data, pch = 20, ylim=c(min(train.data$Y), max(train.data$Y)))
model <- lm(Y ~ poly(X, 2), data = train.data) 

x.vec <- seq(0,10, length = 100)
fhat <- predict(model, newdata = data.frame(X = x.vec))
lines(fhat ~ x.vec, col = "firebrick")

#calculate the error
train.error <- mean(residuals(model)^2)  
train.error
mean((fitted(model) - train.data$Y)^2)

#plot validation data, overlay fitted line, calc error
plot(Y ~ X, data = test.data, pch = 20, ylim=c(min(test.data$Y), max(test.data$Y)))
lines(fhat ~ x.vec, col = "firebrick")
mean((predict(model, newdata = data.frame(X = test.data$X)) - test.data$Y)^2)

#step through first function to illustrate use of debug
debug(SplitData)
SplitData(data = df, splits)
undebug(SplitData)

##################################################################################
#2. perform CV to estimate test error
#includes plots
set.seed(2025)
cross_val(df, k = n.folds, poly = 2)

debug(cross_val)
debug(Kfoldsplit)
cross_val(df, k =n.folds, poly = 2)
undebug(cross_val)
undebug(Kfoldsplit)

##################################################################################
#3. choose from a series of polynomial models 
#apply best model to test set
#need to try out different models
library(foreach) 
debug(PolyCV)

set.seed(5555)

n.folds = 5
n.obs <- nrow(df)
sets <- Kfoldsplit(n.obs, n.folds)                    # Fold indices

MSE_ALL_K <- foreach(k = 1:6, .combine = c) %do% {
  
  PolyCV(data = df, folds = sets, poly = k, is_median = F)
  
}

MSE_ALL_K

plot(x = 1:6, y = MSE_ALL_K,
     xlab = "k", ylab = "MSE",
     main = "CV MSE vs. Fitted Polynomial Degree",
     pch = 20, type = 'l')

#plot again with polynomial 3 overlaid
plot(Y ~ X, data = train.data, pch = 20, ylim=c(min(train.data$Y), max(train.data$Y)))
model <- lm(Y ~ poly(X, 3), data = train.data) 

x.vec <- seq(0,10, length = 100)
fhat <- predict(model, newdata = data.frame(X = x.vec))
lines(fhat ~ x.vec, col = "firebrick")

