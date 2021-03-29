#To do all data
glimpse(mpg)
head(mpg)
summary(mpg)

#수랑형변수
summary(mpg$hwy)
mean(mpg$hwy)
median(mpg$hwy)
range(mpg$hwy)
quantile(mpg$hwy)
opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)

#T-test
hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu=mu0, alternative = "greater")  #only left
t.test(hwy, mu=mu0, alternative = "less")  #only right
t.test(hwy)
(
#Outliers : Q1-1.5*IQR(Q3-Q1) ~ Q3+1.5*IQR(Q3-Q1)
c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))  # insteed c(mean(hwy), sd(hwy)) in robust statistical methods

#변수형변수

