# ============================================ #
# April 2025, STA3030F Neural network example #
# ============================================ #

#: Read in and scale the Data ========================================================
setwd("C:/Users/Greg/OneDrive - University of Cape Town/Documents/Teaching/STA3030F/ML stuff/Data")
setwd("~/Teaching/STA3030F/ML stuff/Data")
setwd("E:/STA3030F/Data & R")

# Focus: Associations between several risk factors and pregnancy durations
# Number of observations = 102

# Response: Categorical

# Covariates:
# > Nutrition: Nutritional status
# > Age21.30: Indicator - Age is between 20 - 30
# > Age30: Indicator - Age is greater than 30
# > Alc: Indicator - History of alcohol abuse
# > Smoke: Indicator - Smoking status 

PREG <- read.table('Pregnancy Duration.txt', h = T)
head(PREG, 5)
table(PREG$Cat)

Preg <- PREG[,-c(1,3:5)]
save(Preg, file = "Pregnancy.RData")


load("Pregnancy.RData")

Preg$Nutrition <- as.numeric(scale(Preg$Nutrition))
Preg$Cat <- factor(Preg$Cat, labels = c("<36", "36-37", ">37"))  
head(Preg,5)

# Q: Why do we scale the data? h2o does it automatically but not nn
# affects learning efficiency and GD

# 1: Fit a NN: Model 1 ==================================================================

library(neuralnet)

set.seed(2025)

mod1 <- neuralnet(Cat ~ Nutrition + Age21.30 + Age30 + Alc + Smoke, 
                  hidden = c(5), 
                  data = Preg,
                  learningrate = 0.05,              
                  threshold = 0.01,                  
                  algorithm = 'backprop',           # Learning algorithm
                  stepmax = 10^6,                   # Maximum number of steps / updates
                  err.fct = "ce",                   # Error function = cross-entropy
                  linear.output = FALSE,            # Specifies whether an activation function is applied to the final layer, in this case, YES
                  lifesign = 'full',                # Amount of infomation printed to the console during the operation 
                  startweights = runif(48, -2, 2),  # Weight and bias intialisation (note number)
                  act.fct = 'logistic')             # Activation function (throughout)

plot(mod1)

# Training error (Cross-Entropy) = 78.5 ; Steps = 33623
#    (No informative interpretations can be drawn wrt training error yet w/o 
#     comparative models or a validation / test set)

# Draw the response curve under the present model 
#    Assume a specific person profile, but with differing nutritional values
#    Purpose: Isolate the effect of nutrition on pregnancy duration (given profile)

# > Create a sequence of values for prediction purposes

dummy <- seq(min(Preg$Nutrition), max(Preg$Nutrition), length = 100)    # Change nutrition
XNEW <- data.frame(Nutrition = dummy,                                   # Hold other variables constant 
                   Age21.30 = 1, 
                   Age30 = 0, 
                   Alc = 1, 
                   Smoke = 1)  

# Plot the predicted probabilities	

pred1 <- predict(mod1, XNEW)
head(pred1)

plot(pred1[, 1] ~ dummy,            # Duration < 36 weeks vs. Nutrition
     xlab = 'Nutrition', 
     ylab = 'Probability', 
     ylim = c(0, 1), 
     type = 'l')

lines(pred1[, 2] ~ dummy, col = 'red')  # Duration between 36 - 37 weeks vs. Nutrition
lines(pred1[, 3] ~ dummy, col = 'green')  # Duration greater than 37 weeks vs. Nutrition

legend('left', col = c('black','red','green'), lty = rep(1,3),
       legend = paste0('Duration: ', c('<36', '36-37', '>37')), 
       bty = 'y')

# Interpretation: 
#  * Preterm pregnancies (< 36 weeks) are highly probable where low nutrition values 
#    (i.e. between -2 and 0.8) are recorded. 
#  * Low likelihood of pregnancies occuring between 36 - 37 weeks
#  * Likelihood of Duration > 37 highest where nutrition values exceed about 1

# 2: Fit a NN - Model 2 (different seed) ==================================================================== 	

set.seed(1234)

mod2 <- neuralnet(Cat ~ Nutrition + Age21.30 + Age30 + Alc + Smoke, 
                  hidden = c(5), 
                  data = Preg,
                  learningrate = 0.05, 
                  threshold = 0.01, 
                  algorithm = 'backprop',
                  stepmax = 10^6, 
                  err.fct = "ce", 
                  linear.output = FALSE, 
                  lifesign ='full',
                  startweights = runif(48, -2, 2), 
                  act.fct = 'logistic')

# Longer time for algorithm to converge
# Caused by: different initialisation / point on the loss surface

plot(mod2)

# Training error = 86.3
# Steps = 55218

# Slightly higher training error and more steps steps relative to Model 1

# A: Draw the response curve under the present model

pred2 <- predict(mod2, XNEW)

plot(pred2[, 1] ~ dummy, 
     xlab = 'Nutrition', 
     ylab = 'Probability', 
     ylim = c(0, 1), 
     type = 'l')

lines(pred2[, 2] ~ dummy, col = 'red')
lines(pred2[, 3] ~ dummy, col = 'green')

legend('left', col = c('black','red','green'), lty = rep(1,3),
       legend = paste0('Duration: ',c('<36','36-37','>37')), 
       bty = 'y')

# Different response curves
# Suggests a different loss surface local minimum was found (and trajectory followed)
# Require a validation set to determine true performance 

# 3: Fit a NN - Model 3 with h2o (regularisation and softmax) ==================================================================== 	
apply(pred2,1,sum)

# A: Cast data frame as an h2o object
library(h2o)
h2o.init()                                  # Initialize h2o	
set.seed(2025)

train <- as.h2o(Preg)                        # Cast dataframe as an h2o object 

# B: Fit a (5)-network to the data
model <- h2o.deeplearning(
  x = 2:6,                                 # Columns in `train` relating to predictors 
  y = 1,                                   # Column in `train` relating to the response
  training_frame = train,                  # Training data frame
  # validation_frame = val,                # If you have a validation dataframe named `val`
  distribution = "multinomial",            # Relates to the specification of the loss function, and the response (NOT focus), similar to softmax
  hidden = c(5),                           # 1 hidden layer with 5 nodes
  activation = 'Tanh',                     # Activation function for the hidden layer
  l2 = 0.01,                               # Level of L2 regularisation (nu)
  epochs = 250000,                         # Number of iterations through the dataset
  loss = 'CrossEntropy',                   # Loss / Objective function
  stopping_tolerance = 0.01,               # Relative tolerance for metric-based stopping criterion 
  mini_batch_size = 102,                   # Batch size (influence optimisation algorithm - e.g. gradient descent)
  adaptive_rate = F,                       # Logical - Allows learning rate to be set and change automatically 
  rate = 0.01)                             # Learning rate

plot(model) 

#NOT complexity, more about convergence

# learning rate, regularisation parm, batch size, hidden layers, number of nodes
# cost fn, act fn, different initialisation of weights
# 1 strategy(nielsen) strip problem down, eg if classifying between 10 classes maybe start with just 2

# > Predictions

pred <- h2o.predict(model, newdata = as.h2o(XNEW))  
pred <- as.data.frame(pred) 
head(pred)

plot(pred$X.36 ~ dummy,             # Category 1 
     xlab ='Nutrition', 
     ylab = 'Probability', 
     ylim = c(0, 1), 
     type = 'l')

lines(pred$X36.37 ~ dummy, col = 'red')   # Category 2 
lines(pred$X.37 ~ dummy, col = 'green')   # Category 3


legend('left', col = c('black','red','green'), lty = rep(1,3),
       legend = paste0('Duration: ', c('<36', '36-37', '>37')), bty = 'y')

# 4, in validation 	
# A: Cast data frame as an h2o object

#create validation set
set.seed(2025)
ind = sample(1:102,20)

train <- as.h2o(Preg[-ind,])                              # Cast dataframe as an h2o object 
validate <- as.h2o(Preg[ind,])

# B: Fit a (5)-network to the data
model <- h2o.deeplearning(
  x = 2:6,                                 # Columns in `train` relating to predictors 
  y = 1,                                   # Column in `train` relating to the response
  training_frame = train,                  # Training data frame
  validation_frame = validate,             # IF you have a validation dataframe named `val`
  distribution = "multinomial",            # Relates to the specification of the loss function, and the response (NOT focus), similar to softmax
  hidden = c(5),                           # 1 hidden layer with 5 nodes
  activation = 'Tanh',                     # Activation function for the hidden layer
  l2 = 0.01,                               # Level of L2 regularisation (nu)
  epochs = 250000,                         # Number of iterations through the dataset
  loss = 'CrossEntropy',                   # Loss / Objective function
  stopping_tolerance = 0.01,               # Relative tolerance for metric-based stopping criterion 
  adaptive_rate = F,                       # Logical - Allows learning rate to be set and change automatically 
  rate = 0.01)                             # Learning rate

plot(model)

h2o.shutdown()	 
