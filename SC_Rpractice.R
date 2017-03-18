library(plyr)
library(ggplot2)
library(gapminder)
library(wesanderson)

# REFS:  GGPLOT CHEAT SHEET http://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#####    Learn to plot awesome graphics:   http://swcarpentry.github.io/r-novice-gapminder/08-plot-ggplot2/
#####    Documentation on Plotting:     http://docs.ggplot2.org/current/
#####    http://www.cookbook-r.com/Graphs/
#####    https://github.com/PromyLOPh/pianobar

setwd("C://Users/Ruth/Documents/R/Practice")
gapminder <- read.csv("C://Users/Ruth/Documents/R/Practice/gapminder-FiveYearData.csv")

one <- gapminder[gapminder$year==1957,]
two <- gapminder[,-(1:4)]
three <- gapminder[gapminder$lifeExp > 80,]
four <- gapminder[1,4:5]
five <- gapminder[gapminder$year== 2002 | gapminder$year== 2007,]

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color=continent))+
  geom_point(size=3, shape="x")+
  scale_x_log10()+
  geom_smooth(method = "lm", size=2)+
  scale_color_manual(values = wes_palette("Darjeeling"))

starts.with <- substr(gapminder$country, start = 1, stop = 1)  
az.countries <- gapminder[starts.with %in% c("A", "Z"),]
ggplot(data=az.countries, aes(x=year, y=lifeExp, color=continent))+
  geom_line(size = 1)+
  facet_wrap(~country)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  xlab("Year")+
  ylab("Life expectancy")+
  ggtitle("Figure 1")+
  scale_color_discrete(name="continent")


ggplot(data = gapminder, aes(x=gdpPercap, fill=continent))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  facet_wrap(~year)+
  scale_color_manual(values = wes_palette("Darjeeling2"))
  
kelvin_to_celsius <- function(kelvin) {
  celsius <- kelvin - 273.15
  return(celsius)
}

