got#
copper = c(39,35,44,33,19,6,27,24,40,13,35,34,34,33,61,
              56,43,19,34,40,34,28,29,45,46,28,41,46,25,17)

zinc = c(39,32,22,32,55,34,46,37,31,45,41,40,46,66,36,
              42,37,43,35,62,48,47,34,42,43,33,47,41,34,41)

# observed F_ratio and mean differ.
mean_diff = mean(copper)-mean(zinc)

## Bootstrapping
# combine the two data sets
all_markets = c(copper,zinc)
# number of bootstrap samples
B = 5000
bstr_diffs = c()
set.seed(2026)
for(j in 1:B){
  samp = sample(all_markets,size = length(all_markets),replace=TRUE)
  copper_mean = mean(samp[1:30])
  zinc_mean = mean(samp[31:60])
  bstr_diffs[j] = copper_mean-zinc_mean
}

# Distribution of the bootstrap mean difference
hist(bstr_diffs,col="blue",main = "Distribution of mean differences",
     xlab = expression(bar(X)[c]^'*'-bar(X)[z]^'*'))

########### CONFIDENCE INTERVALS #################
sort_diffs = sort(bstr_diffs)

# 95% CI
lb = round(sort_diffs[0.025*B],3)
ub = round(sort_diffs[0.975*B],3)

cat("BMCI = ","(",lb," , ",ub,")",sep='')
cat("The Sampling Errors are given by",lb,"and ",ub)
cat("95% CI for mu1 - mu2: (",mean_diff-ub," , ",mean_diff-lb,")",sep ='')

########### HYPOTHESIS TESTING #################
pvalue = (sum(sort_diffs < mean_diff)+sum(sort_diffs > -mean_diff))/B
pvalue

#########################################################################################################
#################################### THREE OR MORE SAMPLES ##############################################

# Store all the locations in a list LOC
loc1 = c(344,382,353,395,207,312,407,421,366,222)
loc2 = c(365,391,538,471,431,450,299,371,442,343)
loc3 = c(261,429,402,391,239,295,129,301,317,386)
loc4 = c(422,408,470,523,398,387,433,440)
loc5 = c(367,445,480,323,366,325,316,381,407,339)
LOC  = list(loc1,loc2,loc3,loc4,loc5)

# Box plots of traffic counts on different locations
windows()  # opens a plotting window
boxplot(LOC,ylab = "Traffic counts",xlab = "Location",
        outline = TRUE,border = "blue")
mean_traffic <- sapply(LOC, mean)
points(1:5, mean_traffic, pch = 0, col = "darkblue")
dev.off()

# Find the overall mean(Y..) and group means (Yi.)
n = sapply(LOC,length)
N = sum(n)
k = length(LOC)
Yi. = sapply(LOC,mean)
Y.. = mean(unlist(LOC))

# Calculate SSE and SST
SSE = SST = 0
for(j in 1:k){
  SSE = SSE + sum((LOC[[j]]-Yi.[j])^2)
  SST = SST + n[j]*(Yi.[j]-Y..)^2
}

# observed F-ratio
Fr_obs = (SST/(k-1))/(SSE/(N-k))

# Bootstrapping 
B = 5000
bstr_ratios = numeric(B)

# combine all the traffic counts
all_counts = unlist(LOC)

set.seed(2026)
for (b in 1:B) {
  # Empty list to store bootstrap samples
  boots = list()
  draw = sample(all_counts,N,TRUE)
  boots[[1]]  = draw[1:10]
  boots[[2]]  = draw[11:20]
  boots[[3]]  = draw[21:30]
  boots[[4]]  = draw[31:38]
  boots[[5]]  = draw[39:48]

  Y.. = mean(unlist(boots))
  Yi. = sapply(boots,mean)
  SSE = SST = 0
  for(j in 1:k){
    SSE = SSE + sum((boots[[j]]-Yi.[j])^2)
    SST = SST + n[j]*(Yi.[j]-Y..)^2
  }
  
  # Store the bootstrap F ratios
  bstr_ratios[b] = (SST/(k-1))/(SSE/(N-k))
}

hist(bstr_ratios,col = "blue",main = "Bootstrapped F-ratios",breaks=30,
     xlab ='F ratios')
abline(v = Fr_obs,col = 'red',lwd = 2)

### HYPOTHESIS TESTING ####
pvalue = sum(bstr_ratios > Fr_obs)/B

# compare to p-value obtained using normal theory
theory_pvalue = pf(Fr_obs,k-1,N-k,lower.tail = FALSE)
pvalue
theory_pvalue
