# ============================================ #
# Tutorial Week 9 - Question 2                 #
# h2o neural network on simulated region data  #
# ============================================ #

library(h2o)
h2o.init()
h2o.removeAll()

# Data
dataQ2 <- data.list[[2]]
head(dataQ2)

# -------------------------------------------- #
# 2(a) Appropriate output activation + objective function
# For binary classification:
# > logistic / sigmoid output layer
# > cross-entropy objective function
# In h2o, once Y is categorical, classification is handled automatically
# -------------------------------------------- #

# -------------------------------------------- #
# 2(b) Plot the data
# -------------------------------------------- #

plot(X2 ~ X1,
     data = dataQ2,
     pch = c(1, 16)[dataQ2$Y + 1],
     col = c("black", "red")[dataQ2$Y + 1],
     main = "Question 2 training data")

legend("topright",
       legend = c("Y = 0", "Y = 1"),
       pch = c(1, 16),
       col = c("black", "red"),
       bty = "n")

# -------------------------------------------- #
# 2(c) Fit two (7,7)-networks using h2o
# -------------------------------------------- #

# Make Y categorical in R first
dataQ2$Y <- factor(dataQ2$Y, levels = c(0, 1))

# Convert to H2O
train_h2o <- as.h2o(dataQ2)

# Force Y to categorical again inside H2O
train_h2o$Y <- as.factor(train_h2o$Y)

# Check what H2O sees
h2o.describe(train_h2o)

set.seed(2026)

# Model 1: no regularisation
mod1 <- h2o.deeplearning(
  x = c("X1", "X2"),
  y = "Y",
  training_frame = train_h2o,
  hidden = c(7, 7),
  activation = "Tanh",
  l2 = 0,
  epochs = 25000,
  seed = 2026
)

plot(mod1)

# Model 2: regularised
mod2 <- h2o.deeplearning(
  x = c("X1", "X2"),
  y = "Y",
  training_frame = train_h2o,
  hidden = c(7, 7),
  activation = "Tanh",
  l2 = 0.0007,
  epochs = 25000,
  seed = 2026
)

plot(mod2)

# -------------------------------------------- #
# 2(d) Predict over the feature space using expand.grid
# -------------------------------------------- #

M <- 200

Lat <- expand.grid(
  X1 = seq(-1, 1, length = M),
  X2 = seq(-1, 1, length = M)
)

Lat_h2o <- as.h2o(Lat)

# Predictions from model 1
pred_mod1 <- h2o.predict(mod1, newdata = Lat_h2o)
pred_mod1 <- as.data.frame(pred_mod1)
head(pred_mod1)

# Predictions from model 2
pred_mod2 <- h2o.predict(mod2, newdata = Lat_h2o)
pred_mod2 <- as.data.frame(pred_mod2)
head(pred_mod2)

# Plot predicted classification regions - Model 1
plot(Lat$X2 ~ Lat$X1,
     col = c("grey85", "red")[as.numeric(as.character(pred_mod1$predict)) + 1],
     pch = 15,
     xlab = "X1",
     ylab = "X2",
     main = "Predicted regions: Model 1 (l2 = 0)")

points(X2 ~ X1,
       data = dataQ2,
       pch = c(1, 16)[as.numeric(as.character(dataQ2$Y)) + 1],
       col = c("black", "red")[as.numeric(as.character(dataQ2$Y)) + 1])

# Plot predicted classification regions - Model 2
plot(Lat$X2 ~ Lat$X1,
     col = c("grey85", "red")[as.numeric(as.character(pred_mod2$predict)) + 1],
     pch = 15,
     xlab = "X1",
     ylab = "X2",
     main = "Predicted regions: Model 2 (l2 = 0.0007)")

points(X2 ~ X1,
       data = dataQ2,
       pch = c(1, 16)[as.numeric(as.character(dataQ2$Y)) + 1],
       col = c("black", "red")[as.numeric(as.character(dataQ2$Y)) + 1])

# Optional: plot predicted probabilities instead of hard classes

plot(Lat$X2 ~ Lat$X1,
     col = gray(1 - pred_mod1$p1),
     pch = 15,
     xlab = "X1",
     ylab = "X2",
     main = "Predicted probability of Y = 1: Model 1")

points(X2 ~ X1,
       data = dataQ2,
       pch = c(1, 16)[as.numeric(as.character(dataQ2$Y)) + 1],
       col = c("black", "red")[as.numeric(as.character(dataQ2$Y)) + 1])

plot(Lat$X2 ~ Lat$X1,
     col = gray(1 - pred_mod2$p1),
     pch = 15,
     xlab = "X1",
     ylab = "X2",
     main = "Predicted probability of Y = 1: Model 2")

points(X2 ~ X1,
       data = dataQ2,
       pch = c(1, 16)[as.numeric(as.character(dataQ2$Y)) + 1],
       col = c("black", "red")[as.numeric(as.character(dataQ2$Y)) + 1])

# h2o.shutdown(prompt = FALSE)