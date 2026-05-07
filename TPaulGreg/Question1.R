# ============================================ #
# Tutorial Week 9 - Question 1                 #
# Neural network on birth weight categories    #
# ============================================ #

# Data: dataQ1
# Response: LOW = "Low", "Medium", "Normal"
# Covariates: AGE, LWT, SMOKE, HT, UI

# 1(a): Training / test split ===================================================

dataQ1 <- data.list[[1]]

set.seed(2026)
index <- sample(1:189, size = 130, replace = FALSE)

train <- dataQ1[index, ]
test  <- dataQ1[-index, ]

head(train)

# 1(b): Standardise numeric predictors ==========================================
# Keep this close to the script, but use training mean / sd for both sets

age_mean <- mean(train$AGE)
age_sd   <- sd(train$AGE)

lwt_mean <- mean(train$LWT)
lwt_sd   <- sd(train$LWT)

train$AGE <- (train$AGE - age_mean) / age_sd
train$LWT <- (train$LWT - lwt_mean) / lwt_sd

test$AGE <- (test$AGE - age_mean) / age_sd
test$LWT <- (test$LWT - lwt_mean) / lwt_sd

# Answer to 1(b):
# We standardise variables so they are on a similar scale, preventing variables
# with large numerical values from dominating the model.
# This improves learning efficiency and helps gradient descent work better.
# This is the same motivation noted in the neural network scripts. 

# 1(c): Appropriate output activation and objective function =====================
# For classification, use:
# > logistic activation at the output layer
# > cross-entropy objective function
# For 3 classes, set up 3 output columns as in the pregnancy example / notes.

# Make sure LOW is a factor in the correct order
train$LOW <- factor(train$LOW, levels = c("Low", "Medium", "Normal"))
test$LOW  <- factor(test$LOW,  levels = c("Low", "Medium", "Normal"))

# Create dummy response columns
train$D1 <- ifelse(train$LOW == "Low", 1, 0)
train$D2 <- ifelse(train$LOW == "Medium", 1, 0)
train$D3 <- ifelse(train$LOW == "Normal", 1, 0)

test$D1 <- ifelse(test$LOW == "Low", 1, 0)
test$D2 <- ifelse(test$LOW == "Medium", 1, 0)
test$D3 <- ifelse(test$LOW == "Normal", 1, 0)

# 1(d): Fit a (5)-network =======================================================

library(neuralnet)

set.seed(2026)

mod1 <- neuralnet(D1 + D2 + D3 ~ AGE + LWT + SMOKE + HT + UI, 
                  hidden = c(5), 
                  data = train,
                  learningrate = 0.05,
                  threshold = 0.01,
                  algorithm = "backprop",           
                  stepmax = 10^6,                   
                  err.fct = "ce",                   
                  linear.output = FALSE,            
                  lifesign = "full",                
                
                  act.fct = "")

plot(mod1)

# 1(e): Response curve for LWT ================================================
# Woman at the mean age who smokes, and does not have HT or UI
# Keep variables in same order as model

dummy <- seq(min(train$LWT), max(train$LWT), length = 100)

XNEW <- data.frame(AGE = 0,      # mean age after standardisation
                   LWT = dummy, 
                   SMOKE = 1, 
                   HT = 0, 
                   UI = 0)

pred1 <- predict(mod1, XNEW)
head(pred1)

plot(pred1[, 1] ~ dummy,            
     xlab = "LWT", 
     ylab = "Probability", 
     ylim = c(0, 1), 
     type = "l")

lines(pred1[, 2] ~ dummy, col = "red")
lines(pred1[, 3] ~ dummy, col = "green")

legend("left", 
       col = c("black", "red", "green"), 
       lty = rep(1, 3),
       legend = c("Low", "Medium", "Normal"), 
       bty = "y")

# 1(f): Refit with h2o ==========================================================
# Main changes:
# > cast data frame to h2o object
# > use multinomial response
# > h2o handles multiclass outputs more naturally
# > can add regularisation

# library(h2o)
# h2o.init()
# set.seed(2026)
#
# train_h2o <- as.h2o(train)
# test_h2o  <- as.h2o(test)
#
# train_h2o$LOW <- as.factor(train_h2o$LOW)
# test_h2o$LOW  <- as.factor(test_h2o$LOW)
#
# model_h2o <- h2o.deeplearning(
#   x = c("AGE", "LWT", "SMOKE", "HT", "UI"),
#   y = "LOW",
#   training_frame = train_h2o,
#   validation_frame = test_h2o,
#   distribution = "multinomial",
#   hidden = c(5),
#   activation = "Tanh",
#   loss = "CrossEntropy",
#   adaptive_rate = FALSE,
#   rate = 0.01,
#   l2 = 0.01,
#   epochs = 100
# )

# 1(g): Classification matrices ================================================
# Training set predictions

pred_train <- predict(mod1, train[, c("AGE", "LWT", "SMOKE", "HT", "UI")])
pred_test  <- predict(mod1, test[, c("AGE", "LWT", "SMOKE", "HT", "UI")])

train_class_num <- apply(pred_train, 1, which.max)
test_class_num  <- apply(pred_test, 1, which.max)

levels_low <- c("Low", "Medium", "Normal")

train_pred_class <- factor(levels_low[train_class_num], levels = levels_low)
test_pred_class  <- factor(levels_low[test_class_num], levels = levels_low)

# Confusion matrices
train_cm <- table(train_pred_class, train$LOW)
test_cm  <- table(test_pred_class, test$LOW)

train_cm
test_cm

# Accuracy and misclassification error
train_accuracy <- mean(train_pred_class == train$LOW)
test_accuracy  <- mean(test_pred_class == test$LOW)

train_error <- 1 - train_accuracy
test_error  <- 1 - test_accuracy

train_accuracy
train_error

test_accuracy
test_error
