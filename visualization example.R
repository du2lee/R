library(gapminder)
library(ggplot2)
library(dplyr)

gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()+ scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_freqpoly()+ scale_x_log10()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_density()+ scale_x_log10()     

# 1개연속형변수

diamonds %>% ggplot(aes(cut)) + geom_bar()
table(diamonds$cut)
prop.table(table(diamonds$cut))
round(prop.table(table(diamonds$cut))*100, 1)
diamonds %>%
  + group_by(cut) %>%
  + tally() %>%
  + mutate(pct = round(n/sum(n)*100,1))

# 1개범주형변수

diamonds %>% ggplot(aes(carat,price))+ geom_point()
diamonds %>% ggplot(aes(carat,price))+ geom_point(alpha=.01)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()
pairs(diamonds %>% sample_n(1000))  # 두개 이상의 연속 변수를 다룰 때는 산점도 행렬이 효과적이다.

# 2개연속형변수

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()
mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)
mpg %>% mutate(class=reorder(class,hwy,median)) %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)
mpg %>% mutate(class=factor(class,levels=c("2seater","subcompact","compact","midsize","minivan", "suv","pickup"))) %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5)
mpg %>% mutate(class=factor(class,levels=c("2seater","subcompact","compact","midsize","minivan", "suv","pickup"))) %>% ggplot(aes(class, hwy)) + geom_jitter(col='gray') + geom_boxplot(alpha=.5) + coord_flip()

#x =범주형 y = 연속형

xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))
mosaicplot(Titanic, main = "Survival on the Titanic", color=TRUE)
apply(Titanic, c(3,4), sum) 
round(prop.table(apply(Titanic, c(3,4), sum),margin = 1), 3)      #child vs adult
apply(Titanic, c(2,4), sum)
round(prop.table(apply(Titanic, c(2,4), sum),margin = 1), 3)      #male vs female

t2 %>% group_by(Sex) %>%
  + summarize(n = sum(Freq), survivors=sum(ifelse(Survived=="Yes", Freq, 0))) %>%
  + mutate(rate_survival=survivors/n)
