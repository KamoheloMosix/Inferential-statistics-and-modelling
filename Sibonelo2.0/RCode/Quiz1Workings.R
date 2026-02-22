
###-----------------------------------------------------------------------------
#QUESTION 1
# Nile is the object that consists of the 100 measurements of annual river flow
nile <- Nile

#Generate 5 Bootstrap samples
B <- 5

#Array that will consist of 5 means of random samples
bstr <- c()

set.seed(101)

for(k in 1:B){
  
  bstr_sample <- sample(nile, replace = TRUE, size = length(nile))
  bstr[k] = mean(bstr_sample)
  
}

###----------------------------------------------------------------------------
# QUESTION 2

set.seed(5)

exp_obs <- rexp(n = 1500, rate = 0.5)

x <- sample(exp_obs, size = 300, replace = TRUE)

#Mean of x
mean(x)

#Variance of x
var(x)

###-------------------------------------------------------
#QUESTION 4
install.packages('gapminder')
library('gapminder')

#Extract the GDP per capita for South Africa and call it gdp_SA

RSA = gapminder[gapminder$country=='South Africa',]

gdp_SA = RSA$gdpPercap

#Generate 1000 bootstrap samples of the same size as the gdp_SA dataset, use a seed of 10

set.seed(10)
outCount = 0

for( i in 1:1000){
  inCount = 0
  
  samp = sample(gdp_SA, replace = TRUE, size = length(gdp_SA))
  
  for( k in 1:12){
    
    if( round(samp[k],3) == 9269.658){
      
      inCount = inCount + 1
      
    }
  }
  if(inCount == 4){
    outCount = outCount + 1
  }
}
outCount

##---------------------------------------------------------------------------------------
# QUESTION 5

dist_bstr_means = c()

set.seed(21)
sample1 = sample(cars$dist, replace=TRUE, size = 50)
sample_mean = mean(sample1)

for( h in 1:5000){
  
  dist_bstr_means[h] = mean( sample(cars$dist, replace = TRUE, size = 50))
}

mean_of_means = mean(dist_bstr_means)

bias_bstr = mean_of_means - sample_mean
round(bias_bstr, 3)

