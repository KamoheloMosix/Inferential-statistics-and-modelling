Dep1<- D1$Dept1
Dep2<- D1$Dept2

DepT<- c(Dep1,Dep2) #combined samples

mean_diff<- mean(Dep1)- mean(Dep2) #mean diff of samples 

bsize<- 3000 #number of bootstrap samples
bstr_diffs = c()
set.seed(50)
for( i in 1:bsize){
  samp= sample(DepT,size = length(DepT), replace = TRUE) # taking a sample of size 100 for bootstarping
  temp1 = samp[1:50]
  temp2 = samp[51:10]
  bstr_diffs[i]= mean(temp1)- mean(temp2)
  
}
hist(bstr_diffs)
