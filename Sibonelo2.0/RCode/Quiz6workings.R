# Question 1

# simple anova

# load in data using the 'import dataset' and now tis saved in aov6

# Read and clean the data
data <- list(
  HS_A = c(81.5,61.8,61,62.4,58.1,77,71.4,75.8,65.9,78.8,72.8,64.6,72.9,73.4,64,60.5,65.8,67.6,79.4,61.8),
  HS_B = c(64.6,67,61.1,61.1,77.6,76.4,61.5,62.5,67.5,70.4,59.2,65.1,54.3,64.4,71.4,77.8,62.2,56.4,67.5,65.2,64.7,69.1,63.6,63.1,62.7,55.6),
  HS_C = c(56.5,61.7,53.3,68,65.4,57.5,51.2,79.4,59.3,57.4,60.6,52.9,68.5,74.8,67.2,58.5),
  HS_D = c(53.1,64.8,65.3,72.1,55.1,74.6,65.2,69,74.6,67.9,57.9,56.2,68.5,70.4,68.2,47.2,70,65.5,62.6)
)

# Combine into a data frame for ANOVA
df <- data.frame(
  score = unlist(data),
  group = rep(names(data), lengths(data))
)


# Grand mean
grand_mean <- mean(df$score)

# SST: total sum of squares
SST <- sum((df$score - grand_mean)^2)

# SSE (within-group): sum of squared deviations from group means
SSE <- sum(sapply(names(data), function(g) {
  group_vals <- data[[g]]
  sum((group_vals - mean(group_vals))^2)
}))

# SSB (between-group) as a sanity check: SST = SSB + SSE
SSB <- SST - SSE

# Ratio SSE/SST
ratio <- SSE / SST

cat("Grand Mean:    ", round(grand_mean, 4), "\n")
cat("SST:           ", round(SST, 4), "\n")
cat("SSE:           ", round(SSE, 4), "\n")
cat("SSB:           ", round(SSB, 4), "\n")
cat("SSE/SST ratio: ", round(ratio, 4), "\n")

aov_result <- aov(df$score ~ df$group)
aov_summary <- summary(aov_result)

cat("=== aov() results ===\n")
cat("SSB (aov):", aov_summary[[1]]$`Sum Sq`[1], "\n")
cat("SSE (aov):", aov_summary[[1]]$`Sum Sq`[2], "\n")
cat("SST (aov):", sum(aov_summary[[1]]$`Sum Sq`), "\n")

cat("\n=== Manual results ===\n")
cat("SSB:", round(SSB, 4), "\n")
cat("SSE:", round(SSE, 4), "\n")
cat("SST:", round(SST, 4), "\n")

# Question 2 --------------------------------------------------------------
# Load the data
oj <- read.table("OJuice.txt", header = TRUE, sep = "\t")

# Factor conversions
oj$Purchase <- as.factor(oj$Purchase)
oj$Store7   <- as.factor(oj$Store7)

# Binary response: MM = 1, CH = 0
oj$PurchaseBin <- ifelse(oj$Purchase == "CH", 1, 0)

# Fit GLM
glm_model <- glm(PurchaseBin ~ PriceCH + PriceMM + DiscCH + DiscMM + PctDiscCH + PctDiscMM + SalePriceMM + SalePriceMM+ PriceDiff + Store7,
                 data = oj, family = binomial)

summary(glm_model)

nl <- oj[10,]
pred <- predict(glm_model, newdata = nl, type = 'response')
