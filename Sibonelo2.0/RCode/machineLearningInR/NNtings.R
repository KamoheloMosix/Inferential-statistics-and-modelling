# ====================== #
# Topic: Neural networks #
# ====================== #

# 1: Activation Functions ====================================================

sigmoid <- function(z) 1/(1 + exp(-z))                # Output range: [0, 1]

tanh <- function(z) (exp(2*z) - 1)/(exp(2*z) + 1)     # Output range: [-1, 1]

relu <-  function(z) sapply(z, function(x) max(0, x)) # Output range: R+

identity <- function(z) z                             # Output range: R

# Plot the Activations for z in [-5, 5] ===================================

z_all <- seq(-5, 5, by = 0.01)

plot(sigmoid(z_all) ~ z_all,             # Sigmoid
     xlab = "z", 
     ylab = "Value", 
     xlim = c(-5, 5),
     ylim = c(-1, 1),
     type = "l",
     col = "black", 
     lty = 1, 
     main = "Activation Functions")

lines(tanh(z_all) ~ z_all,              # Tanh
      col = "blue", 
      lty = 2)

lines(relu(z_all) ~ z_all,              # Relu 
      col = "red", 
      lty = 3)

lines(identity(z_all) ~ z_all,          # Identity  
      col = "green", 
      lty = 4)

legend("topleft", 
       legend = c("sigmoid", "tanh", "relu", "identity"), 
       fill = c("black", "blue", "red", "green"), 
       lty = 1:4)

sigmoid(-1.5) ; sigmoid(0.8) ; sigmoid(7)

#############################
# 2: Simulate a dataset ===================================================================
set.seed(2025)
N <- 100           
X <- runif(N, -1, 1)

fx <- function(x){
  sin(3*pi*x)
}

Y <- fx(X) + rgamma(N, 1, 2)    # True f + Random error

Simulation_1 <- data.frame(Y = Y, X = X)

plot(Y ~ X, 
     data = Simulation_1, 
     pch  = 20, 
     main = "Simulated Dataset")

# 3: Fit a Neural Network =======================================================

library(neuralnet)
?neuralnet
set.seed(2025)
mod1 <- neuralnet(Y ~ X, 
                  hidden = c(7),        # 1 hidden layer with 7 nodes
                  linear.output = T,
                  act.fct = "logistic",
                  data = Simulation_1,
                  lifesign = "full")
plot(mod1)

# Plot: 
#   * Error (7.6): Training error - SSE {err.fct}
#   * Steps (15557): Number of steps until convergence 
#                   In this case: {threshold} specifies the minimum magnitude of gradients 
#                                  before the next step is taken [threshold reached] 
# > Convergence: Until a sufficiently small improvement in fit is obtained by taking an additional step

mod2 <- neuralnet(Y ~ X, 
                  hidden = c(7, 7),     # 2 hidden layers with 7 and 7 nodes respectively
                  linear.output = T,
                  act.fct = "logistic",
                  stepmax = 1e+6,      # Maximum number of steps allowed (increased to ensure convergence)
                  data = Simulation_1,
                  lifesign = "full")   
plot(mod2)

# Note: Function takes longer to run as the additional hidden layer adds model complexity 
# Plot:
#   * Error (5.5): Lower training error
#   * Steps (51247): More steps until convergence, more parameters

# 4. Predictions =======================================================

# > All x-values

x_all <- data.frame(X = seq(-1, 1, 0.01))

# > Predictions

pred1 = predict(mod1, newdata = x_all)
pred2 = predict(mod2, newdata = x_all)

# > Plot

plot(Y ~ X, 
     data = Simulation_1, 
     pch  = 20, 
     main = "Simulated Dataset")

lines(pred1 ~ x_all$X, lty = 1, lwd = 1)      # Predictions: Model 1
lines(pred2 ~ x_all$X, lwd = 2, lty = 2)      # Predictions: Model 2

x_all_v <- unlist(x_all)                 
lines(fx(x_all$X) ~ x_all_v, lty = 3)         # Known true f

legend('topright', 
       legend = c('(7)-network','(7,7)-network', "true f"), 
       lty = c(1, 2, 3), 
       lwd = c(1, 2, 1))

# > Interpretation

# Expected error: = shape / rate = 0.5 (biased)
# Observations lie above the line of best fit
# Comparison: (7)-network vs. (7, 7)-network
# > Smoother
# > Has sufficient flexibility to model fx
# > (7, 7)-network may be overfitting to the data given lack-of-smoothness 
# Both functions overestimate the true f given the error bias

###############################################################################
#NN example on iris data

library(neuralnet)

#scale inputs (if one large X dominates it also leads to smaller gradients from the outset, harder to learn)
newData <- iris
head(newData)

newData$Sepal.Length <- scale(newData$Sepal.Length)
newData$Sepal.Width <- scale(newData$Sepal.Width)
newData$Petal.Length <- scale(newData$Petal.Length)
newData$Petal.Width <- scale(newData$Petal.Width)
head(newData)

#Fit a (5) network using petal predictors
#Choose appropriate actvn and obj fns 
set.seed(5555)

res <- neuralnet(Species ~ Petal.Length+Petal.Width, 
                 hidden = c(5), 
                 err.fct = "ce", 
                 act.fct = "logistic", 
                 algorithm = "backprop", 
                 stepmax = 10^6, 
                 linear.output = FALSE, 
                 learningrate = 0.05, 
                 lifesign = "full", data = newData)
plot(res)

#response curve
M = 100
X1_dummy <- seq(min(newData$Petal.Length),max(newData$Petal.Length),length = M)
X2_dummy <- seq(min(newData$Petal.Width),max(newData$Petal.Width),length = M)
X1 <- rep(X1_dummy,M)
X2 <- rep(X2_dummy, each = M)

Lat <- data.frame(Petal.Width = X1, Petal.Length = X2)
Lat

pred <- predict(res,Lat)
pred

class <- apply(pred,1,which.max)
cols <- c('blue','grey','purple')

plot(Lat$Petal.Length ~ Lat$Petal.Width, pch = 16, col = cols[class])
text(newData$Petal.Length ~ newData$Petal.Width,labels = as.numeric(newData$Species))
