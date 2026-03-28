library(tidyr)
# Question 1

# simple anova

# load in data using the 'import dataset' and now tis saved in aov6
newdat <- read.delim('aov6.txt', header=TRUE, dec = ',', na.strings = c('', 'NA'), strip.white = TRUE,
                     blank.lines.skip = TRUE, fill = TRUE)

# remove rows that are completely empty
newdat <- newdat[rowSums( !is.na(newdat)) > 0, ]

# compute the means, but exclude the values that are missing before computation
meanA <- mean(newdat$HS_A, na.rm = TRUE)
meanB <- mean(newdat$HS_B)
meanC <- mean(newdat$HS_C, na.rm = TRUE)
meanD <- mean(newdat$HS_D, na.rm = TRUE)

#turn the data into long format, basically a cheat code...haha
# using tidyverse
long_form <- pivot_longer(newdat, cols = c(HS_A, HS_B, HS_C, HS_D),
                          names_to = 'school', values_to = 'passMark', values_drop_na = TRUE)

TAPPL <- tapply(long_form$passMark, long_form$school, mean)

# make anov model, also cheatcode because no way i'm computing all that hehe
mod <- aov(passMark ~ school, data = long_form)
summary(mod)

# Question 2 --------------------------------------------------------------
# Load the data
oj <- read.table("OJuice.txt", header = TRUE, sep = "\t")

# Factor conversions
oj$Purchase <- as.factor(oj$Purchase)
oj$Store7   <- as.factor(oj$Store7)

oj$got_MM <- ifelse(oj$Purchase == "MM", 1, 0)

glm_model <- glm(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + PctDiscCH + PctDiscMM,
                 data = oj,
                 family = binomial)

summary(glm_model)

nl <- oj[12,]
pred <- predict(glm_model, newdata = nl, type = 'response')

n2 <- data.frame(PriceCH = 1.75, PriceMM = 1.99, DiscCH = 0, DiscMM = 0.4 , PctDiscMM = 0.201005, PctDiscCH = 0 )
pred2 <- predict(glm_model, newdata = n2, type = 'response')

