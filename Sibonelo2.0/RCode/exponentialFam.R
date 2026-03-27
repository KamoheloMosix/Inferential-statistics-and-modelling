data<- lawn.bunch.grass
head(data)
names(data)
table(data$Community_)
prop.table(table(data$Community_))

round(prop.table(table(data$Community_))*100, 2)

lawn.bunch.grass$LG <- ifelse(data$Community_ == "LG", 1, 0)
lawn.bunch.grass$Height <- ifelse(data$Grass_heig == "patchy", 1, 0)
t1 <- with(lawn.bunch.grass, table(LG, Slope))
t2 <- prop.table(t1, 2)

# Second-sitting hehe

# This is what we use to split up the plotting area so that we can plot multiple graphs at the 
# same time
# par( mfrow = c( rows, columns), mar(margin), mgp(lines away from the plotting area)

par(mar = c(5.5, 5.5, 1, 1), mgp = c(4, 1, 2))
plot(0:19, t2[2,], las = 1, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Proportion lawn grass", pch = 19, col = 'firebrick3',
     xlab = 'Slope')

t11 <- with(lawn.bunch.grass, table(Height, Slope))
t22 <- prop.table(t11, 2)

par(mar = c(5.5, 5.5, 1, 1), mgp = c(4, 1, 2))
plot(0:19, t2[2,], las = 1, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Proportion patchy grass", pch = 19, col = 'firebrick3',
     xlab = 'Slope')

m1 <- glm(LG ~ Slope, family = binomial(link = "logit"), data = lawn.bunch.grass)
m2 <- glm(Height ~ Slope, family = binomial(link = "logit"), data = lawn.bunch.grass)
m3 <- glm(lawn.bunch.grass$Height ~ lawn.bunch.grass$Fire5698, family = binomial(link = "logit"))
m4 <- glm(LG ~ Slope+Fire5698, family = binomial(link = "logit"), data = lawn.bunch.grass)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

plot( jitter(LG) ~ jitter(Slope), data = lawn.bunch.grass, las = 1,
      xlab = 'Slope', pch= 14, col = 'pink',
      ylab = 'lawn grass', cex.lab=1.5, cex.axis = 1.5, yaxt = 'n')
