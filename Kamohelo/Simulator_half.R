pdelays <- c(37,42,38,44,39,35,43,41,42,38,36,34,37,42,
             36,38,41,39,39,37,34,34,41,41,40,38,38,46,38,42)
boxplot(pdelays,ylab = "Days",main = "Boxplot")
hist(pdelays,breaks = 10 ,xlab = "days" , main = "Histogram")
mean(pdelays)
sd(pdelays)
median(pdelays)
bootx <- sample(pdelays , size =30 ,replace = TRUE)
all_boots <- matrix(NA,nrow = 5000000 ,ncol = 30)
for(j in 1:5000000){
  
  boot <- sample(pdelays ,size = 30 ,replace = TRUE)
  all_boots[j,] <- boot
  
}

bs_means <- apply(all_boots ,MARGIN = 1 , FUN = sd)
mean(bs_means)
sd(bs_means)
min(bs_means)
max(bs_means)
boxplot(bs_means ,ylab = "Days",main = "Boxplot")
hist(bs_means ,xlab = "Days" ,main = "Histogram")

sorted_bs_means <- sort(bs_means ,decreasing = FALSE)
sorted_bs_means[125]
sorted_bs_means[4875]
