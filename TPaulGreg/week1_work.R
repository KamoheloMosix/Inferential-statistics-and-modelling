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
