# Data to use for small question
Y <- c(15, 21, 6, 28)
X1 <- c(6, 8, 8, 4)
X2 <- c(1, 1, 0, 3)

# make the dataframe
toyDataSet <- data.frame(Y, X1, X2)

#Suitable metric of  fit to use since this is continius output, we want to use MSE
rss_Root <- sum((toyDataSet$Y - mean(Y))^2)

# split on X1 < 5
rightforX1great5 <- toyDataSet$Y[X1 > 5]
leftforX1less5 <- toyDataSet$Y[ X1 < 5]

RSSforsplitOnX1great5 <- sum( (mean(rightforX1great5)-rightforX1great5)^2)
RSSforsplitOnX1less5 <- sum( (mean(leftforX1less5)-leftforX1less5)^2)

RSS_split_X1_5 <- RSSforsplitOnX1great5 + RSSforsplitOnX1less5

# split on X1 > 5
rightforX1great7 <- toyDataSet$Y[X1 > 7]
leftforX1less7 <- toyDataSet$Y[ X1 < 7]

RSSforsplitOnX1great7 <- sum( (mean(rightforX1great7)-rightforX1great7)^2)
RSSforsplitOnX1less7 <- sum( (mean(leftforX1less7)-leftforX1less7)^2)

RSS_split_X1_7 <- RSSforsplitOnX1great7 + RSSforsplitOnX1less7

#Split on X2 < 0.5    ---> BEST SPLIT
rightforX2great0.5 <- toyDataSet$Y[X2 >= 0.5]
leftforX2less0.5 <- toyDataSet$Y[ X2 < 0.5]

RSSforsplitOnX2great0.5 <- sum( (mean(rightforX2great0.5)-rightforX2great0.5)^2)
RSSforsplitOnX2less0.5 <- sum( (mean(leftforX2less0.5)-leftforX2less0.5)^2)

RSS_split_X2_0.5 <- RSSforsplitOnX2great0.5 + RSSforsplitOnX2less0.5


#Split on X2 < 2
rightforX2great2 <- toyDataSet$Y[X2 >= 2]
leftforX2less2 <- toyDataSet$Y[ X2 < 2]

RSSforsplitOnX2great2 <- sum( (mean(rightforX2great2)-rightforX2great2)^2)
RSSforsplitOnX2less2 <- sum( (mean(leftforX2less2)-leftforX2less2)^2)

RSS_split_X2_2 <- RSSforsplitOnX2great2 + RSSforsplitOnX2less2

# Question 2, Using IRIS dataset ------------------------------------------
set.seed(2026)
Iris <- read.csv("data/TutWk8Data.csv")
#Change bin variable to either 1 or 0
Iris$isVirginica <- ifelse(Iris$Virginica == "Virginica", 1, 0)

# plot for Q1
plot(Iris$Sepal.Width  ~ Iris$Sepal.Length , pch = ifelse(Iris$isVirginica == 1, 19, 17), col = ifelse(Iris$isVirginica == 1, 'red', 'skyblue'), xlab = "Sepal.Length", ylab = "Sepal.Width", main = "virginica(RED) vs other")


# fit regression model
bin_model <- glm(isVirginica ~  Sepal.Length+ Sepal.Width, data = Iris, family = 'binomial')

#get the descider line
b <- coef(bin_model)

#draw the separator line
abline(a = -b[1]/b[3], b = -b[2]/b[3], lwd = 2, col = "black")

# Now start doing tree tings yeah, fit using a tree
library(tree)

tree_mod <- tree(factor(isVirginica) ~ Sepal.Length + Sepal.Width, data = Iris)

# draw the partitioning space and add it to plot on line 56
partition.tree(tree_mod, add =T)

# time to use Rpart - Recursive partioning, modern library nje
library(rpart)
rpart_tree <- rpart(isVirginica ~ Sepal.Width + Sepal.Length, data = Iris, method = 'class',
                    parms = list(split = 'information'))


best_cp <- rpart_tree$cptable[which.min(rpart_tree$cptable[, "xerror"]), "CP"()]

pruned_tree <- prune(rpart_tree, cp = best_cp)   # str8 from the horses mouth this one
pruned <- prune(rpart_tree, cp = 0.0105119)
