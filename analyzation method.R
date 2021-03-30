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

#Outliers : Q1-1.5*IQR(Q3-Q1) ~ Q3+1.5*IQR(Q3-Q1)
c((mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))  # insteed c(mean(hwy), sd(hwy)) in robust statistical methods

#변수형변수

set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
table(x)
prop.table(table(x))
#if we dont know Approval rating
binom.test(x=length(x[x=='Yes']), n = length(x), p =0.5, alternative = "two.sided")
#about margin of error
binom.test(x=5400, n =10000)
n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96 * sqrt(1/(4 * n)),4))
curve(1.96 * sqrt(1/(4 * x)), 10, 10000, log='x')

# x, y : 수량형변수

ggplot(mpg, aes(cty, hwy)) + geom_jitter() + geom_smooth(method="lm") #First, Scatterplot
cor(mpg$cty, mpg$hwy)  #Second, correlation coeffcient
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method = "kendall"))
with(mpg, cor(cty, hwy, method = "spearman"))
(hwy_lm <- lm(hwy ~ cty, data = mpg))  #Third, linear regression model and goodness fo fit
summary(hwy_lm)  # In example, hwy = 0.892 + 1.34 * cty
#Forth, predict liner regression model
predict(hwy_lm)
resid(hwy_lm)  #calculate residual
predict(hwy_lm, newdata = data.frame(cty=c(10, 20, 30)), se.fit = TRUE)
#Fifth, diagnosis Assumption of linear regression model
class(hwy_lm)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(hwy_lm, las = 1)
par(opar)
resid(hwy_lm)

#if data has outliner, should use lqs from MASS
library(dplyr)
library(MASS)
set.seed(123)
lqs(stack.loss ~ ., data = stackloss) #lqs
lm(stack.loss ~ ., data = stackloss) #General linear model

#LOESS model
plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~ displ, data=mpg)
mpg_lo
summary(mpg_lo)
xs <- seq(2,7,length.out = 100)  #Visualzation
mpg_pre <- predict(mpg_lo, newdata=data.frame(displ=xs), se=TRUE)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96*mpg_pre$se.fit, lty=2)
lines(xs, mpg_pre$fit + 1.96*mpg_pre$se.fit, lty=2)
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
