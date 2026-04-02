grassData <- data
options(digits = 2)
# check out what the data has as it's columns
names(grassData)

#check what's in the community variable in the form of a table, will list everything found there
table(grassData$Community_)

# to get the proportions of grass types in data
prop.table(table(grassData$Community_))

# we note that roughly 20% of the grass found in pixels is Lawn grass
# we are only interested in finding out which factors affect the presence of lawn grass hence we will model it and only it hecne we crearte a bin var for it

grassData$LG <- ifelse(grassData$Community_ == 'LG', 1, 0)

# with LG being the response var, let us investigate it's relationship with the Slope of pixel, so we create a table with strictly those 2 vars

# creates a cross-tabulation table, bascially counts of lawn grass(LG) for different kinds of slope values
slp_table <- with(grassData, table(LG, Slope))

# now i want to get the proportion of LG at each slope value, 2 says use columns for this
prop_table <- prop.table(slp_table, 2)

# partition the plotting space
par(mar = c(5.5, 5.5, 1, 1), mgp = c(4, 1, 0))
plot(0:19, prop_table[2,], las = 1, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "proportion lawn grass", pch = 19, col = "firebrick3",
     xlab = "slope")

# fit the model with response = LG, explanatory var = Slope, see the relationship between these 2
# since the response is binary, we will use family = 'binomial'
mod_slope <- glm(LG ~ Slope, data = grassData, family = 'binomial')
summary(mod_slope)
