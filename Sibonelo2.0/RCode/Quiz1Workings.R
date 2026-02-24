
###-----------------------------------------------------------------------------
#QUESTION 1
# Nile is the object that consists of the 100 measurements of annual river flow
nile <- Nile

#Generate 5 Bootstrap samples
B <- 5

#Array that will consist of 5 means of random samples
bstr <- matrix(NA, nrow = B, ncol = length(nile))

set.seed(9)

for(k in 1:B){
  
  bstr[k, ] <- sample(nile, replace = TRUE, size = length(nile))
  
}

median(bstr[4, ])
#max(bstr_sample[3,])

###----------------------------------------------------------------------------
# QUESTION 2

set.seed(5)

normal_obs <- rnorm(n = 1500,mean = 2, sd = 10 )

var1 <- sample(normal_obs, size = 300, replace = TRUE)

# The general Summary
round( summary(var1), 2)

#Mean of x
#round(mean(var1), 2)

#SD of x
#round(sd(var1), 2)

# Variance of x
#round( var(var1), 2)

###-------------------------------------------------------
#QUESTION 4
install.packages('gapminder')
library('gapminder')

#Extract the GDP per capita for South Africa and call it gdp_SA

India = gapminder[gapminder$country=='Brazil',]

gdp_Ind = India$gdpPercap

#Generate 1000 bootstrap samples of the same size as the gdp_SA dataset, use a seed of 10

set.seed(10)
outCount = 0

for( i in 1:1000){
  inCount = 0
  
  samp = sample(gdp_Ind, replace = TRUE, size = 12)
  
  for( k in 1:length(samp)){
    
    if( round(samp[k], 3) == 9065.801){
      
      inCount = inCount + 1
      
    }
  }
  if(inCount >= 2){
    outCount = outCount + 1
  }
}
outCount

##---------------------------------------------------------------------------------------
# QUESTION 5

dist_bstr_means = c()

speed = cars$dist

set.seed(21)

obs_mean = mean(speed)

for( h in 1:5000){
  
  dist_bstr_means[h] = mean( sample(speed, replace = TRUE, size = 50))
}

mean_of_means = mean(dist_bstr_means)

bias_bstr = mean_of_means - obs_mean

corrected_mean = obs_mean - bias_bstr

bias_bstr = mean_of_means - obs_mean

round(bias_bstr, 3)
#round(corrected_mean, 3)


