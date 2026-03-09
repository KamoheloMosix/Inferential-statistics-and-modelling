dep1 <- D1$Dept1
dep2 <- D1$Dept2
dep1mean <- mean(D1$Dept1)
dep2mean <- mean(D1$Dept2)
obs_diff <- dep1mean - dep2mean
# Question1 Tings ---------------------------------------------------------

# Counted the number of times per minute professors from different departments said 'uh' or 'ah' during lectures to fill gaps between words. Data was derived for observing them for 50 minutes


B = 3000
set.seed(50)

diffs = c()

for(k in 1:B){
  samp1 = sample(dep1, replace=TRUE, size = length(dep1))
  samp2 = sample(dep2, replace=TRUE, size = length(dep2))
  
  diffs[k] = mean(samp1) - mean(samp2)
}

hist(diffs, col='pink', breaks = 30)
abline(v = obs_diff, col = 'black', lwd = 2)


# Question2 ---------------------------------------------------------------

# Hypothesis Test with an Alternative 'greater than'

B2 = 2500
set.seed(50)

bootstr_diffs = c()

for(k in 1:B2){
  s1 = sample(dep1, size = length(dep1), replace = TRUE)
  s2 = sample(dep2, size = length(dep2), replace = TRUE)
  bootstr_diffs[k] = mean(s1) - mean(s2)
}
p_value = sum( bootstr_diffs[bootstr_diffs > obs_diff ])/B2

# p-value = Pr( bootst_means - obs_mean > | obs_meanD1 - obs_meanD2 - (meanD1 - meanD2) )
#         ~ Pr( boostr_means > | SE_obs + obs_mean) # but obs_mean is sorta 0 brev
#         ~ Can get this with the picture in my head now


# Question3 ---------------------------------------------------------------

attendance <- read.csv("attendance (1).csv")
soccerCity <- attendance$attendance[attendance$stadium == 'Soccer city']
greenPoint <- attendance$attendance[attendance$stadium == 'Green point']
ellisPark <- attendance$attendance[attendance$stadium == 'Ellis Park']
nelsonMandela <- attendance$attendance[attendance$stadium == 'Nelson Mandela']

meanSoccer <- mean(soccerCity)
meanGreen <- mean(greenPoint)
meanEllis <- mean(ellisPark)
meanNelson <- mean(nelsonMandela)



