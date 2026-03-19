data<- lawn.bunch.grass
head(data)
names(data)
table(data$Community_)
prop.table(table(data$Community_))

round(prop.table(table(data$Community_))*100, 2)

lawn.bunch.grass$LG <- ifelse(data$Community_ == "LG", 1, 0)
lawn.bunch.grass$Height <- ifelse(data$Grass_heig == "patchy", 1, 0)
t1 <- with(lawn.bunch.grass, table(LG, Slope))
t2 <- prop.table(t1, 2)
