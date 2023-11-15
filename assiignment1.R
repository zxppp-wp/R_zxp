data1 <- read.csv("/Users/zhaoxiuping/My R/datawork_1.csv")
attach(data1)
df <- data.frame(genders=gender,rgrades=R ,pythongrades=python)
df

par(mai=c(0.6,0.6,0.4,0.4),cex=0.7,cex.main=1,font.main=1)
plot(R,python,main="scatter plot")

sum <- 0
x<- rnorm(50)
for(i in 1:5){
  sum = sum +x[i]
}
avg <- sum/50
avg

