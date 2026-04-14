# ==================== #
# Topic: True function # 
# April 2025           #
# ==================== #

library(rgl) # Allow the rotation of 3d plots

# 1: Example (2D) =====================================================================================

# > Assume a relationship exists between a person's age and level of strength. 

# > Plot these observations, adding a line representing the true functional mapping.

# -----------------------------------------------------------------------------------------------------

## 1A: Simulate a dataset. Create n = 100 (age, strength) observations.

### i: Create random ages

x_age <- runif(n = 100, min = 18, max = 25)

### ii: Define functional mapping between age and strength, f: x --> y

true_f <- function(age){
  2*age
}

### iii: Create corresponding age-related strengths

y_strength <- true_f(x_age) + rnorm(100, mean = 0, sd = 4)   # True f + random error

head(cbind(x_age,y_strength))

## 1B: Plot: 'x_age' vs 'y_strength', adding the 'true_f' line

### i: Plot - Age vs. Strength

plot(y_strength ~ x_age, pch = 20, 
     main = "Age vs. Strength",
     xlab = "Age", ylab = "Strength")

### ii: Add true functional line 

x_age_all <- seq(from = 18, to = 25, by = 0.1)

lines(true_f(x_age_all) ~ x_age_all, 
      col = "blue", lwd = 2)

model <- lm(y_strength ~ x_age)
abline(model, col = "red")

# ----------------------------------------------------------------------------------------------------
# 2: Example (3D) ========================================================================================

# > Assume a person's height is also related to his/her strength

# > A: Simulate a new dataset. Create n = 100 (age, height, strength) observations.
#      - Create random ages between 18 and 25 ('x_age') using R's runif function [already have]
#      - Create random heights that follow ~ N(170, 5^2) 
#      - Create corresponding age-height-related strengths ('y_strength') by
#            * assuming the following best functional mapping: strength = 2*age + 0.15*height
#            * allowing for random error ~ N(0, 3^2)

# > B: Plot these observations, adding a plane representing the true functional mapping.

# ---------------------------------------------------------------------------------------------------------

## 2A: Simulate a dataset. Create n = 100 (age, strength) observations.

### i: Create random heights

x_height <- rnorm(100, mean = 170, sd = 5)               # Note 'sd' argument in rnorm

### ii: Define functional mapping between age, height and strength, f: x1, x2 --> y

true_f <- function(age, height){
  2*age + 0.15*height
}

### iii: Create corresponding age-height-related strengths

y_strength <- true_f(x_age, x_height) + rnorm(100, mean = 0, sd = 3)     # True f + random error


## 2B: Plot: 'x_age', 'x_height' vs 'y_strength', adding the 'true_f' plane

### i: Plot - Age vs. Strength

plot3d(x = x_age, 
       y = x_height, 
       z = y_strength, 
       pch = 20, 
       main = "Age vs. Strength",
       xlab = "Age",
       ylab = "Height",
       zlab = "Strength")

### ii: Add true functional plane 

x_age_all <- seq(from = 18, to = 25, by = 0.1)
x_height_all <- seq(from = 150, to = 190, by = 0.5)
z <- outer(x_age_all, x_height_all, FUN = true_f)
length(x_age_all) ; length(x_height_all) ; dim(z)
z[1:10,]

persp3d(x = x_age_all, 
        y = x_height_all, 
        z = z, 
        add = TRUE, 
        col = "lightblue")

model2 <- lm(y_strength ~ x_age + x_height)
model2

### Define functional mapping for estimated function

est_f <- function(age, height){
  3.58 + 1.88*age + 0.15*height
}

z2 <- outer(x_age_all, x_height_all, FUN = est_f)

plot3d(x = x_age, 
       y = x_height, 
       z = y_strength, 
       pch = 20, 
       main = "Age vs. Strength",
       xlab = "Age",
       ylab = "Height",
       zlab = "Strength")

### ii: Add estimated functional plane 

persp3d(x = x_age_all, 
        y = x_height_all, 
        z = z2, 
        add = TRUE, 
        col = "red")

