library(readxl)
galaxy <- read.csv('galaxies.csv')

#NGC =  number of global clusters
#MVT = measure of a galaxies brightness, large negative values means very bright

View(galaxy)
head(galaxy)

pois_galaxy_model <- glm(NGC ~ MVT, family = 'poisson', data = galaxy)
summary(pois_galaxy_model)

# change plotting window to have 1 row and 2 cols
par(mfrow = c(1, 2))

# plot the raw scale
plot(NGC ~ MVT, data = galaxy, xlab = "visual magnitude", ylab = "n.o of seen clusters per galaxy",
     pch = 1, main = 'Raw scale')

# plot log-transformed scale
plot( log(NGC + 0.5) ~ MVT, data = galaxy, xlab = "visual magnitude", ylab = "log(n.o of seen clusters per galaxy)",
      pch = 1, main = 'log-transformed scale')
