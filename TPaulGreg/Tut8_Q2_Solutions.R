# ======================================================= #
# Tutorial 8 - Question 2: Iris Dataset (Virginica vs Other)
# ======================================================= #

# > Data: 150 iris flowers, 3 species (50 each)
# > Response: Virginica = "Virginica" OR "Other" (binary)
# > Predictors: Sepal.Length and Sepal.Width (cm)

# > Read in data
setwd("~/your/path/here")   # <-- Update to your working directory
Data <- read.csv("TutWk8Data.csv")
head(Data)

# Create binary indicator: 1 = Virginica, 0 = Other
Data$isVirginica <- ifelse(Data$Virginica == "Virginica", 1, 0)

# Index for plotting: 1 = Other, 2 = Virginica
ind <- Data$isVirginica + 1

# ============================================================= #
# Q2.1: Plot the feature space using different symbols to       #
#        differentiate Virginica flowers from other species.    #
# ============================================================= #

plot(Sepal.Width ~ Sepal.Length,
     data = Data,
     pch  = c(17, 19)[ind],               # Triangle = Other, Circle = Virginica
     col  = c('steelblue', 'red')[ind],
     xlab = 'Sepal Length (cm)',
     ylab = 'Sepal Width (cm)',
     main = 'Q2.1: Feature Space - Virginica vs Other Iris Species')

legend('topright',
       legend = c('Other', 'Virginica'),
       pch    = c(17, 19),
       col    = c('steelblue', 'red'),
       bty    = 'n')


# ============================================================= #
# Q2.2: Fit a logistic regression model and overlay the         #
#        decision boundary. Count misclassified points.         #
# ============================================================= #

# Fit logistic regression: isVirginica ~ Sepal.Length + Sepal.Width
LR.model <- glm(isVirginica ~ Sepal.Length + Sepal.Width,
                data   = Data,
                family = binomial(link = 'logit'))

summary(LR.model)

# Extract coefficients: b[1] = intercept, b[2] = Sepal.Length, b[3] = Sepal.Width
b <- coef(LR.model)

# Decision boundary: b[1] + b[2]*Sepal.Length + b[3]*Sepal.Width = 0
# Rearranging for Sepal.Width (y-axis):
#   Sepal.Width = -b[1]/b[3] - (b[2]/b[3]) * Sepal.Length
# IMPORTANT: Sepal.Length must be on the x-axis and Sepal.Width on the y-axis

# Re-draw plot and overlay the decision boundary
plot(Sepal.Width ~ Sepal.Length,
     data = Data,
     pch  = c(17, 19)[ind],
     col  = c('steelblue', 'red')[ind],
     xlab = 'Sepal Length (cm)',
     ylab = 'Sepal Width (cm)',
     main = 'Q2.2: Logistic Regression Decision Boundary')

legend('topright',
       legend = c('Other', 'Virginica'),
       pch    = c(17, 19),
       col    = c('steelblue', 'red'),
       bty    = 'n')

abline(a = -b[1]/b[3], b = -b[2]/b[3], lwd = 2, col = 'black')

# Count misclassifications at threshold = 0.5
preds_LR      <- ifelse(fitted(LR.model) > 0.5, 1, 0)
conf_LR       <- table(Predicted = preds_LR, Actual = Data$isVirginica)
conf_LR

misclassified_LR <- sum(preds_LR != Data$isVirginica)
cat("Q2.2 - Misclassified points (Logistic Regression):", misclassified_LR, "\n")


# ============================================================= #
# Q2.3: Fit a classification tree using the `tree` library      #
#        (default stopping criteria). Plot the feature space    #
#        partitioning and compare to logistic regression.       #
# ============================================================= #

library(tree)

# Fit classification tree with default stopping criteria
# Response must be a factor for a classification tree
tree.model <- tree(factor(isVirginica) ~ Sepal.Length + Sepal.Width, data = Data)
tree.model
summary(tree.model)

# Plot the tree structure
plot(tree.model)
text(tree.model, pretty = 0)
title(main = "Q2.3: Classification Tree Structure (Default)")

# Plot feature space with BOTH the tree partition AND logistic regression boundary
# Note: Sepal.Length must be on x-axis for partition.tree to draw correctly
plot(Sepal.Width ~ Sepal.Length,
     data = Data,
     pch  = c(17, 19)[ind],
     col  = c('steelblue', 'red')[ind],
     xlab = 'Sepal Length (cm)',
     ylab = 'Sepal Width (cm)',
     main = 'Q2.3: Tree Partition vs Logistic Regression Boundary')

legend('topright',
       legend = c('Other', 'Virginica'),
       pch    = c(17, 19),
       col    = c('steelblue', 'red'),
       bty    = 'n')

# Overlay tree partitions (green)
partition.tree(tree.model, add = T, col = 'darkgreen', lwd = 2)

# Overlay logistic regression decision boundary (black dashed)
abline(a = -b[1]/b[3], b = -b[2]/b[3], lwd = 2, col = 'black', lty = 2)

legend('bottomright',
       legend = c('Tree Partitions', 'Logistic Regression'),
       col    = c('darkgreen', 'black'),
       lty    = c(1, 2),
       lwd    = 2,
       bty    = 'n')

# Misclassification comparison
fitted_tree       <- predict(tree.model, type = 'class')
misclassified_tree <- sum(as.numeric(as.character(fitted_tree)) != Data$isVirginica)
cat("Q2.3 - Misclassified points (Decision Tree)    :", misclassified_tree, "\n")
cat("Q2.3 - Misclassified points (Logistic Regression):", misclassified_LR, "\n")
# >> Note: The tree uses axis-aligned rectangular splits; LR uses a linear boundary
# >> The tree can capture non-linear patterns in the feature space that LR cannot


# ============================================================= #
# Q2.4: Calculate the Shannon entropy for the full data         #
#        (i.e., at the root node)                               #
# ============================================================= #

# Shannon entropy: H = -sum( p_k * log2(p_k) ) over all classes k
# Note: the `tree` package uses DEVIANCE = -2 * sum(n_k * log(p_k)), NOT Shannon entropy

n_total     <- nrow(Data)                       # Total observations
n_Virginica <- sum(Data$isVirginica == 1)        # Count of Virginica
n_Other     <- sum(Data$isVirginica == 0)        # Count of Other

p_Virginica <- n_Virginica / n_total             # Proportion of Virginica
p_Other     <- n_Other     / n_total             # Proportion of Other

cat("\nQ2.4 - Root node class counts and proportions:\n")
cat("  Total n =", n_total, "\n")
cat("  Virginica: n =", n_Virginica, ", p =", round(p_Virginica, 4), "\n")
cat("  Other    : n =", n_Other,     ", p =", round(p_Other,     4), "\n")

# Shannon entropy (base 2 logarithm)
H_root <- -(p_Virginica * log2(p_Virginica) + p_Other * log2(p_Other))
cat("Q2.4 - Shannon entropy at root node: H =", round(H_root, 4), "\n")
# >> Maximum possible entropy (equal classes) = 1 bit
# >> H < 1 because classes are unequal (50 Virginica vs 100 Other)


# ============================================================= #
# Q2.5: Use rpart to fit a full unconstrained tree, then prune  #
#        using cost-complexity pruning. Show the tree structure  #
#        and calculate the reduction in Shannon entropy.         #
# ============================================================= #

library(rpart)
library(rpart.plot)

# Fit FULL unconstrained tree:
# cp = 0      : no cost-complexity penalty (grow as large as possible)
# minsplit = 2: allow splits with as few as 2 observations in a node
# minbucket = 1: allow terminal nodes with just 1 observation
# parms = list(split = 'information'): use Shannon entropy (information gain) as splitting criterion
rpart.full <- rpart(isVirginica ~ Sepal.Length + Sepal.Width,
                    data    = Data,
                    method  = 'class',
                    parms   = list(split = 'information'),
                    control = rpart.control(cp = 0, minsplit = 2, minbucket = 1))

cat("\nQ2.5 - Full unconstrained tree (cp table):\n")
printcp(rpart.full)

rpart.plot(rpart.full, type = 2, fallen.leaves = FALSE,
           main = "Q2.5: Full Unconstrained Tree (rpart)")

# Cost-complexity pruning: plot cross-validation error vs cp
plotcp(rpart.full, main = "Q2.5: Cost-Complexity Pruning - Choose Optimal cp")
# >> Look for the leftmost cp value within 1 standard deviation of the minimum xerror

# Select the optimal cp: use cp that minimises cross-validation error (xerror)
cp_table <- rpart.full$cptable
best_cp  <- cp_table[which.min(cp_table[, "xerror"]), "CP"]
cat("Q2.5 - Optimal cp selected (min xerror):", round(best_cp, 6), "\n")

# Prune the tree using the optimal cp
rpart.pruned <- prune(rpart.full, cp = best_cp)

cat("\nQ2.5 - Pruned tree structure:\n")
print(rpart.pruned)

rpart.plot(rpart.pruned, type = 4, fallen.leaves = FALSE,
           main = "Q2.5: Pruned Tree (Cost-Complexity Pruning)")

# ---- Calculate reduction in Shannon entropy for the pruned tree ----

# Extract terminal node information from the pruned tree frame
node_frame     <- rpart.pruned$frame
terminal_nodes <- node_frame[node_frame$var == "<leaf>", ]

cat("\nQ2.5 - Terminal nodes of pruned tree:\n")
print(terminal_nodes[, c("n", "yval", "yprob")])

# Weighted Shannon entropy across all terminal nodes
H_terminal_weighted <- 0

for (i in 1:nrow(terminal_nodes)) {
  n_node     <- terminal_nodes$n[i]
  probs      <- terminal_nodes$yprob[i, ]    # Class probabilities at this node
  probs_nz   <- probs[probs > 0]             # Remove zero probabilities (avoid log(0))
  H_node     <- -sum(probs_nz * log2(probs_nz))
  H_terminal_weighted <- H_terminal_weighted + (n_node / n_total) * H_node
  cat("  Terminal node", i, ": n =", n_node,
      ", probs =", round(probs, 3),
      ", H =", round(H_node, 4), "\n")
}

# Reduction in Shannon entropy = Information gained by the pruned tree
H_reduction <- H_root - H_terminal_weighted

cat("\nQ2.5 Summary:\n")
cat("  Shannon entropy at root (before splitting)       :", round(H_root,               4), "\n")
cat("  Weighted Shannon entropy at terminal nodes        :", round(H_terminal_weighted,  4), "\n")
cat("  Reduction in Shannon entropy (information gained) :", round(H_reduction,          4), "\n")


# ============================================================= #
# Q2.6: Predict P(Virginica) for Sepal.Length = 7, Width = 3   #
# ============================================================= #

new_flower <- data.frame(Sepal.Length = 7, Sepal.Width = 3)

prob_Q2.6 <- predict(rpart.pruned, newdata = new_flower, type = 'prob')
cat("\nQ2.6 - Predicted class probabilities:\n")
print(prob_Q2.6)
cat("Q2.6 - P(Virginica | Sepal.Length = 7 cm, Sepal.Width = 3 cm) =",
    round(prob_Q2.6[, "1"], 4), "\n")
# >> The predicted class follows from whichever terminal node this observation falls into


# ============================================================= #
# Q2.7: Do Virginica iris flowers have a different Sepal.Width  #
#        compared to the other species?                         #
# ============================================================= #

# Check which variables were used as split points in the pruned tree
cat("\nQ2.7 - Split variables used in the pruned tree:\n")
print(rpart.pruned$frame[rpart.pruned$frame$var != "<leaf>", "var", drop = FALSE])
# >> If Sepal.Width does NOT appear as a split variable, the tree found it
# >> was not informative enough to partition on (relative to Sepal.Length)

# Compare Sepal.Width distributions between groups using a boxplot
boxplot(Sepal.Width ~ Virginica,
        data = Data,
        col  = c('steelblue', 'red'),
        xlab = 'Species Group',
        ylab = 'Sepal Width (cm)',
        main = 'Q2.7: Sepal Width Distribution - Virginica vs Other')

# Summary statistics by group
cat("\nQ2.7 - Mean Sepal.Width by group:\n")
print(tapply(Data$Sepal.Width, Data$Virginica, summary))

# >> INTERPRETATION:
# >> If the pruned tree only splits on Sepal.Length, then Sepal.Width alone
# >> does not provide enough additional discriminatory power after accounting
# >> for Sepal.Length. This suggests that while there may be some overlap
# >> in Sepal.Width between Virginica and Other, Sepal.Length is the more
# >> useful variable for distinguishing Virginica from the other species.
