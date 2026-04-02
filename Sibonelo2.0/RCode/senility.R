# Create the from the slides
wais_scores <- c(9, 13, 6, 8, 10, 4, 14, 8, 11, 7, 9, 7, 5, 14, 13, 16, 10,12,11, 14, 15, 18, 7, 16, 9, 9, 11, 13, 15, 13, 10, 11, 6, 17, 14, 19, 9, 11, 14, 10, 16, 10, 16, 14, 13, 13, 9, 15, 10, 11, 12, 4, 14, 20)

senility <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# Combine into a data frame
senility_data <- data.frame(
  wais = wais_scores,
  senility = senility
)
# do some EDA on the dataset
table(senility)

# get that cross table thingy
cross_table <- with(senility_data, table(senility, wais))
prop.table(cross_table,2)

# create the glm model, with binary response senility and wais_score being the response variable
sen_mod <- glm(senility ~ wais, data = senility_data, family = 'binomial')
summary(sen_mod)

# make prediction
n1 <- data.frame(wais = 4)
prediction_prob <- predict(sen_mod, newdata = n1, type = 'response')

# eyyy..deepseek did this brevas
# Create a sequence of WAIS scores for smooth prediction
wais_seq <- seq(min(senility_data$wais), max(senility_data$wais), length.out = 100)
predicted_prob <- predict(sen_mod, 
                          newdata = data.frame(wais = wais_seq),
                          type = "response")

# Create the plot
plot(jitter(senility_data$wais, amount = 0.2), 
     jitter(senility_data$senility, amount = 0.05),
     xlab = "WAIS Score",
     ylab = "Senility (0 = No, 1 = Yes)",
     main = "Senility vs WAIS Score with Logistic Regression Curve",
     pch = 19,
     col = ifelse(senility_data$senility == 1, "red", "blue"),
     cex = 0.8)

# Add the fitted logistic curve
lines(wais_seq, predicted_prob, col = "blue", lwd = 2)

# Add a legend
legend("topright", 
       legend = c("Senility Present", "Senility Absent", "Fitted Probability"),
       col = c("red", "blue", "blue"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 1),
       lwd = c(NA, NA, 2),
       cex = 0.8)