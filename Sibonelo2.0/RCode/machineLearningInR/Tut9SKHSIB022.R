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

plot(predNN[, 1] ~ lwt_seq, xlab = "LWT", ylab = "probability", ylim = c(0, 1), xlim = c(min(train_set$LWT),max(train_set$LWT)), type = 'l' )

lines(predNN[,2]~ lwt_seq, col = 'green', lwd = 1.5)

lines(predNN[,3]~ lwt_seq, col = 'red', lwd= 1.5)

legend('topleft',
       legend = levels(train_set$LOW),
       col = c("black", "green", 'red'),
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

datah2o <- as.h2o(q2_data)

# model 1
oneModel <- h2o.deeplearning(x = 1:2, y = 3, training_frame = datah2o,
                             hidden = c(7, 7),
                             l2=0,
                             activation = "Tanh",
                             epochs = 250000,
                             seed = 2026,
                             loss = 'CrossEntropy'
                             )
# model 2
twoModel <-h2o.deeplearning(x = 1:2, y = 3, training_frame = datah2o,
                            hidden = c(7, 7),
                            l2=0.0007,
                            activation = "Tanh",
                            epochs = 250000,
                            seed = 2026,
                            loss = 'CrossEntropy'
                            )