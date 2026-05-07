# Modelling bayes tings

# X =  n.o of defective items in a batch

# Market characteristics
high_sell <-c(4000, 0.1, 500)
med_sell <- c(2000, .15, 300)
low_sell <- c(1000, .25, 200)

theta <- c(.05, .1, .25)

#get probability of defective rate
p_theta <- function(theta){
  if(theta == .05) 0.5
  else if(theta == .01) 0.3
  else if(theta == 0.25) 0.2
}

# Using Binomial with n = 3, p = theta
get_sell_price <- function(theta, marketType){
  
  X <- c(0, 1, 2, 3)
  profit <- c()
  for(curr in 1:4){
    def_units <- dbinom(X[curr], 3, p_theta(theta))
    profit <- 4000 - max(0, 100*marketType[2] - def_units )*marketType[3]  
  }
  

}