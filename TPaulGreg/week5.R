# ---- Question 1: ANOVA ----
HS_A <- c(81.5,61.8,61,62.4,58.1,77,71.4,75.8,65.9,78.8,72.8,64.6,72.9,73.4,64,60.5,65.8,67.6,79.4,61.8)
HS_B <- c(64.6,67,61.1,61.1,77.6,76.4,61.5,62.5,67.5,70.4,59.2,65.1,54.3,64.4,71.4,77.8,62.2,56.4,67.5,65.2,64.7,69.1,63.6,63.1,62.7,55.6)
HS_C <- c(56.5,61.7,53.3,68,65.4,57.5,51.2,79.4,59.3,57.4,60.6,52.9,68.5,74.8,67.2,58.5)
HS_D <- c(53.1,64.8,65.3,72.1,55.1,74.6,65.2,69,74.6,67.9,57.9,56.2,68.5,70.4,68.2,47.2,70,65.5,62.6)

grades <- c(HS_A, HS_B, HS_C, HS_D)
school <- factor(c(rep("A",length(HS_A)), rep("B",length(HS_B)),
                   rep("C",length(HS_C)), rep("D",length(HS_D))))
aov_model <- aov(grades ~ school)
ss <- summary(aov_model)[[1]]
SSE <- ss["Residuals","Sum Sq"]
SST <- ss["school","Sum Sq"]
round(SSE/SST, 2)  # 9.06


# ---- Questions 2 & 3: Logistic Regression ----
OJ <- read.csv("OJ_extract.csv")
OJ$Purchase <- factor(OJ$Purchase, levels=c("MM","CH"))  # CH=1
model <- glm(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + PctDiscCH + PctDiscMM,
             data=OJ, family=binomial)
summary(model)

# Chi-squared GOF
model$null.deviance - model$deviance  # 106.5

# Row 5 log-odds
predict(model, newdata=OJ[5,])  # log-odds

# Row 10 P(no CH)
1 - predict(model, newdata=OJ[10,], type="response")  # 0.4765