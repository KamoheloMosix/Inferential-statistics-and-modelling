nile <- Nile
options(digits = 7)


bvector <- matrix(0, nrow =5 ,ncol = 100)
set.seed(9)
for(i in 1:5){
  samp = sample(nile,size = length(nile),replace = TRUE)
  
  bvector[i,] = samp
  
  
}
set.seed(5)
uvector<- runif(1500,1,10)
var1<- sample(uvector,size = 300,replace = TRUE)
round(mean(var1),2)
round(sd(var1),2)
install.packages("gapminder")
library(gapminder)
Brazil = gapminder[gapminder$country=="Brazil",]
gdp_B = Brazil$gdpPercap
BstB <- matrix(0, nrow =1000 ,ncol = 12)
set.seed(10)
for(i in 1:1000){
  samp = sample(gdp_B,size = length(gdp_B),replace = TRUE)
  
  BstB[i,] = samp
  
  
}
total_hits <- 0

for(i in 1:1000) {
  # Check for the value in row i
  # We round to 3 decimals to avoid floating-point mismatches
  x <- sum(round(BstB[i, ], 3) == 9065.801)
  
  if(x >= 2) {
    total_hits <- total_hits + 1
  }
}
car <-cars
speed <- car$speed
carsM<- matrix(0,nrow = 1000,ncol = 50)
set.seed(300)
for(i in 1:1000){
  samp = sample(speed,size = length(speed),replace = TRUE)
  
  carsM[i,] = samp
  
  
}
meansC = c()
for(i in 1:1000){
  
  
 meansC[i]= mean (carsM[i,])
  
  
}
bootB = mean(meansC) - mean(speed)
correct=round( mean(speed) - bootB,3)

