data<- lawn.bunch.grass

# getting the number of each type of community (type of grass)
num_Community=table(data$Community_) 
options(digits = 2)
# getting those numbers as proportions for a good look at overoll spread of data
prop_comunity=prop.table(num_Community) 


#focusing now only on LG(lawn grass)    

dat_LG<- ifelse(data$Community_ == "LG" , 1, 0)
View(dat_LG)

#  Because the
#response is binary, and the resolution of the slope variable is not very
# high, the points are all on top of each other
plot(data$Slope , dat_LG)
plot(jitter(data$Slope) , jitter(dat_LG)) # this is to get a better picture by using a random explantory varible in data

# Modeling the relationship of the probablioty of LG and  how it changes with Slope 

t1<- table(dat_LG, data$Slope)
t2 <- prop.table(t1,2)
plot(0:19 , t2[2,])
