
gapminder %>% filter(year==2007) %>%
  + ggplot(aes(gdpPercap, lifeExp)) + 
  + geom_point() + scale_x_log10()

#위 자료에 대륙,인구수 변수 추가
gapminder %>% filter(year==2007) %>%
  + ggplot(aes(gdpPercap, lifeExp)) + 
  + geom_point(aes(size=pop, col=continent)) + scale_x_log10()

#


gapminder %>% ggplot(aes(year, lifeExp, group=country)) + geom_line()

#위 자료에 대륙 변수 추가
gapminder %>% ggplot(aes(year, lifeExp, group=country, col=continent)) + geom_line()

#facet_wrap 사용해서 대륙 변수 추가
gapminder %>% ggplot(aes(year, lifeExp, group=country)) + geom_line() + facet_wrap(~ continent)


# should read page 87-88 