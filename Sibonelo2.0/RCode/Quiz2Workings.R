c1 <- A1$City1

obs_mean <- mean(c1)

hyp_mean = 500

boostr_means = c()

set.seed(4)

B = 4000

for(i in 1:B){
    samp = sample(c1, size = length(c1), replace =TRUE)
    
    boostr_means[i] = mean(samp)
    
  
}
se = obs_mean - hyp_mean

p_value = sum( bstr_means[])