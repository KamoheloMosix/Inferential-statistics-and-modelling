# SIMPLE SCRIPT - JUST THE ANSWERS
# Make sure D1.txt and calories.csv are in your working directory

# Clear environment
rm(list = ls())

# ============================================
# QUESTION 1 - Professor "Uh/Ah" Study (Centered around 0?)
# ============================================

# Load data
d1 <- read.table("D1.txt", header = TRUE, sep = "\t")
dept1 <- d1$Dept1
dept2 <- d1$Dept2
n <- length(dept1)

# Bootstrap distribution of mean difference (3000 samples, seed=50)
B = 3000
set.seed(50)
boot_diff <- numeric(B)
for(i in 1:B) {
  boot_diff[i] <- mean(sample(dept1, n, TRUE)) - mean(sample(dept2, n, TRUE))
}
hist(boot_diff, col = 'pink'); abline(v =0)

# Check if centered around 0 (mean of bootstrap distribution near 0)
boot_mean <- mean(boot_diff)
# Consider it centered around 0 if the mean is between -0.1 and 0.1
q1_answer <- ifelse(abs(boot_mean) < 0.1, "TRUE", "FALSE")

# ============================================
# QUESTION 2 - Hypothesis Test (One-tailed, H1: μd1 > μd2)
# ============================================

set.seed(50)  # Seed = 60 as specified
obs_diff <- mean(dept1) - mean(dept2)
combined <- c(dept1, dept2)

B=2500
boot_diff2 <- numeric(B)

for(i in 1:B) {
  boot_diff2[i] <- mean(sample(combined, n, TRUE)) - mean(sample(combined, n, TRUE))
}

# One-tailed p-value (proportion of bootstrap diffs >= observed diff)
#q2_sum <- (sum(boot_diff2 > obs_diff) + sum(boot_diff2 < -obs_diff))/5000
q2_sum <- sum(boot_diff2 > obs_diff)/B
p-val = pr(xb - mew > SE)
      = pr(X* - xb> SE)
      = pr(X* > SE + xb)
      = pr(X* > obs_diff )

# ============================================
# QUESTION 3 - Calorie Study
# ============================================

# Load calories data
cal <- read.csv("attendance (1).csv")

# QUESTION 3a - Average on Day 1
day1_vals <- cal$attendance[cal$stadium == "Green point"]
q3a_answer <- round(mean(day1_vals))

# QUESTION 3b - Calculate (SST × (N - k)) / (SSE × (k - 1))
# Add athlete IDs (5 athletes, 4 days)
cal$athlete <- rep(1:5, 4)

# Method 1: Two-way ANOVA without Error term to get SST and SSE easily
model2 <- aov(intake ~, data = cal)
summary_model <- summary(model2)

# Extract SST (for days) and SSE (residuals)
SST <- summary_model[[1]]["as.factor(days)", "Sum Sq"]
SSE <- summary_model[[1]]["Residuals", "Sum Sq"]

# N = total observations, k = number of days
N <- nrow(cal)  # 20 observations
k <- length(unique(cal$days))  # 4 days

# Calculate the ratio
ratio <- (SST * (N - k)) / (SSE * (k - 1))
q3b_answer <- sprintf("%.3f", ratio)

# ============================================
# PRINT ANSWERS
# ============================================

cat("\n")
cat("QUESTION 1:", q1_answer, "\n")
cat("QUESTION 2:", q2_answer, "\n")
cat("QUESTION 3a:", q3a_answer, "\n")
cat("QUESTION 3b:", q3b_answer, "\n")
cat("\n")

# Optional: Show some details
cat("--- DETAILS ---\n")
cat("Q1 - Bootstrap mean difference:", round(boot_mean, 4), "\n")
cat("Q2 - Observed difference:", round(obs_diff, 4), "\n")
cat("Q3b - SST:", round(SST, 2), "SSE:", round(SSE, 2), "\n")
cat("Q3b - Calculated ratio:", ratio, "\n")