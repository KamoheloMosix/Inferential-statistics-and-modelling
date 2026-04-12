data <- read.csv("data/TutWk7Data.csv")


# TUT 7 LOESS -------------------------------------------------------------

# LOESS -> Locally estimated scatterplot smoothing

seed(2025)
mod_loess <- loess(Y ~ X, data = data, span = k)


