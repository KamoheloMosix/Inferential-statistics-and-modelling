# Sibonelo Sikhosatia
# Tutorial 9 submission
# Neural networks and all their majesty


# Question1 ---------------------------------------------------------------
load("data/TutWk9Data.RData")
q1_data <- data.list[[1]]
set.seed(2026)

q1_data$LOW <- as.factor(q1_data$LOW)

#basically going to choose random observations to fall into the training set 130/189--> 68.78% train
index <- sample(1:189, size = 130, replace = F)
train_set <- q1_data[index, ]
val <- q1_data[-index, ]


#Standardise the numeric data, we do this to ensure that the scale of a factor does not give it more influence over other factors even though it might not be more important, we want all factors to have the same effect on the response
age_mean <- mean(train_set$AGE)
age_sd <- sd(train_set$AGE)

lwt_mean <- mean(train_set$LWT)
lwt_sd <- sd(train_set$LWT)

train_set$AGE <- (train_set$AGE - age_mean) / age_sd
train_set$LWT <- (train_set$LWT - lwt_mean) / lwt_sd

val$AGE <- (val$AGE - age_mean) / age_sd
val$LWT <- (val$LWT - lwt_mean) / lwt_sd


# see how the levels are sitting
levels(train_set$LOW)

# c)Considering the fact that we have 3 output classes, LOW, MEDIUM AND NORMAL, an output function would be the softmax, and the act.fct would be the sigmoid function

library(neuralnet)
nn <- neuralnet(LOW ~ ., data = train_set, hidden = 5, err.fct = 'ce', act.fct = 'logistic', linear.output = F, lifesign = 'full', stepmax = 10^6)

# e)
lwt_seq <- seq(from =min(train_set$LWT),to= max(train_set$LWT),by = 0.001 )

# when predicting, must follow same order the train set was in
predFrame <- data.frame(AGE = 0,LWT = lwt_seq, SMOKE = 1,  HT = 0, UI=0)

predNN <- predict(nn, newdata = predFrame)


plot(nn)

plot(predNN[, 1] ~ lwt_seq, xlab = "LWT", ylab = "probability", ylim = c(0, 1), xlim = c(min(train_set$LWT),max(train_set$LWT)), type = 'l' , col='black')

lines(predNN[,2]~ lwt_seq, col = 'red', lwd = 1.5)

lines(predNN[,3]~ lwt_seq, col = 'green', lwd= 1.5)

legend('topleft',
       legend = levels(train_set$LOW),
       col = c("black", "red", 'green'),
       lty = 1,
       lwd = 1.5,
       bty='n')

#f) #construncting the sorta confusion matrix hehe
trainPreds <- apply( predict(nn, train_set), 1, which.max)
trainPreds <- factor(trainPreds, labels = levels(train_set$LOW))
table(Predicted = trainPreds, Actual = train_set$LOW)

valPreds <- apply( predict(nn, val), 1, which.max)
valPreds <- factor( valPreds, labels =levels(val$LOW))
table( Predicted = valPreds, Actual = val$LOW)


# Question2 Breva ---------------------------------------------------------
load("data/TutWk9Data.RData")
q2_data <- data.list[[2]]


#Plot nyana to see what these co-ordinates actually are
plot(X1 ~ X2, data = q2_data,col = c("black",'red')[q2_data$Y], pch = 16)

#a) Considering the binary thingamajig, Output we need sigmoid, objective = 'cross-entropy'

# B)
set.seed(2026)
library(h2o)
h2o.init()

q2_data$Y <- as.factor(q2_data$Y)
datah2o <- as.h2o(q2_data)

# model 1
oneModel <- h2o.deeplearning(x = 1:2, y = 3, training_frame = datah2o,
                             hidden = c(7, 7),
                             l2=0,
                             activation = "Tanh",
                             epochs =5000,
                             seed = 2026
                             )
# model 2
twoModel <-h2o.deeplearning(x = 1:2, y = 3, training_frame = datah2o,
                            hidden = c(7, 7),
                            l2=0.0007,
                            activation = "Tanh",
                            epochs = 5000,
                            seed = 2026
                            )

gridPred <- expand.grid(
  X1 = seq(-1, 1, length = 1000),
  X2 = seq(-1, 1, length = 1000)
)

h2oPred <- as.h2o(gridPred)

onePred <- h2o.predict(oneModel, newdata = h2oPred)
twoPred <- h2o.predict(twoModel, newdata = h2oPred)

onePredFrame <- as.data.frame(onePred)
twoPredFrame <- as.data.frame(twoPred)

plot(gridPred$X2 ~ gridPred$X1,
     col = gray(1- onePredFrame$p1)
     )

points(X2~X1, data = q2_data,
       )


# msix section ------------------------------------------------------------

library(h2o)
h2o.init()
h2o.removeAll()  # Clear everything in h2o memory

# Step 2: Split dataQ2 into training (500) observations
set.seed(2026)
idx      <- sample(1:1000, size = 500, replace = FALSE)
trainQ2  <- q2_data[idx, ]

# Step 3: Convert to h2o frame and make Y a factor IMMEDIATELY
trainQ2_h2o       <- as.h2o(trainQ2)
trainQ2_h2o$Y     <- h2o.asfactor(trainQ2_h2o$Y)

# Verify Y is now a factor
h2o.levels(trainQ2_h2o$Y)  # Should print "0" "1"

# Step 4: Fit Model 1 - NO regularisation
nn_q2_model1 <- h2o.deeplearning(
  x              = c("X1", "X2"),
  y              = "Y",
  training_frame = trainQ2_h2o,
  hidden         = c(7, 7),
  activation     = "Tanh",
  epochs         = 25000,
  l2             = 0,
  seed           = 2026
)

# Step 5: Fit Model 2 - WITH L2
nn_q2_model2 <- h2o.deeplearning(
  x              = c("X1", "X2"),
  y              = "Y",
  training_frame = trainQ2_h2o,
  hidden         = c(7, 7),
  activation     = "Tanh",
  epochs         = 25000,
  l2             = 0.0007,
  seed           = 2026
)

# Step 6: Create grid and predict
grid <- expand.grid(
  X1 = seq(-1, 1, length.out = 200),
  X2 = seq(-1, 1, length.out = 200)
)

grid_h2o <- as.h2o(grid)

# Predict from both models
pred1 <- as.data.frame(h2o.predict(nn_q2_model1, grid_h2o))$predict
pred2 <- as.data.frame(h2o.predict(nn_q2_model2, grid_h2o))$predict

# Check predictions look right
head(pred1)
table(pred1)

# Step 7: Plot both side by side
par(mfrow = c(1, 2))

plot(grid$X1, grid$X2,
     col  = ifelse(pred1 == "1", "firebrick3", "pink4"),
     pch  = 19, cex = 0.1,
     main = "Model with L2 = 0",
     xlab = "X1", ylab = "X2")

plot(grid$X1, grid$X2,
     col  = ifelse(pred2 == "1", "firebrick3", "pink4"),
     pch  = 19, cex = 0.1,
     main = "Model 2 with L2 = 0.0007",
     xlab = "X1", ylab = "X2")
