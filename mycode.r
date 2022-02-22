anscombe
str(anscombe)
anscombe[,c('x1','y1')]


par(mfrow=c(2,2))

df <- anscombe
plot(df$x1, df$y1, pch=19, col='tomato')
abline(lm(y1~x1, data = df), col = 'blue')

plot(df$x2, df$y2, pch=19, col='tomato')
abline(lm(y2~x2, data = df), col = 'blue')

plot(df$x3, df$y3, pch=19, col='tomato')
abline(lm(y3~x3, data = df), col = 'blue')

plot(df$x4, df$y4, pch=19, col='tomato')
abline(lm(y4~x4, data = df), col = 'blue')


install.packages('tidyverse')
library(tidyverse)

str(mpg)
?mpg

p <- ggplot(data = mpg,
            mapping = aes(x=displ, y = hwy))
p + geom_point()
p + geom_point(mapping = aes(color = class))

colors()

p + geom_point(mapping = aes(color = class, size = class))
p + geom_point(mapping = aes(color = class, shape = class))
p + geom_point(mapping = aes(color = class, size = class, alpha = 0.3))

p + geom_point(color = 'salmon') + facet_wrap(~ class, nrow = 3)

p + geom_point(color = 'salmon') + facet_grid(drv ~ cyl)
p + geom_point(color = 'salmon') + geom_smooth(color = 'cyan')
p + geom_point(mapping = aes(color = class)) + geom_smooth(color = 'cyan')

library(ggplot2)
data('diamonds')
str(diamonds)

p <- ggplot(data = diamonds)
p + geom_bar(mapping = aes(x = cut), fill = 'steelblue')
p + stat_count(mapping = aes(x = cut), fill = 'steelblue')

p + geom_bar(mapping = aes(x = cut, fill = clarity))
p + geom_bar(mapping = aes(x = cut, fill = clarity)) + coord_flip()
p + geom_bar(mapping = aes(x = cut, fill = clarity),
             position= 'fill')     #바를 끝까지 채워서 표현
p + geom_bar(mapping = aes(x = cut, fill = clarity),
             position= 'dodge')    #바를 나눠서 표현

ggplot(data = mpg) +
    geom_point(aes(x = displ, y = hwy, color = class),
              position = 'jitter')     #점들은 흔들어 겹쳐진 부분이 보이게 함
                                       #점들이 옳지 않은 값이 되어 좋지않음

p <- ggplot(data = mpg, mapping = aes(x = class, y = hwy))
p + geom_boxplot(fill = 'lightyellow')
p + geom_boxplot(fill = 'lightyellow') + coord_flip()  #그래프를 90도 회전

p <- ggplot(diamonds, aes(x=cut, fill=cut))
p + geom_bar(show.legend = F, width = 1) +
    coord_polar() +
    labs(x = NULL, y = NULL)



world <- map_data('world')
ggplot(world, aes(long, lat, group = group)) +
    geom_polygon(fill = 'orange', color = 'tomato')

world <- map_data('nz')
ggplot(world, aes(long, lat, group = group)) +
    geom_polygon(fill = 'orange', color = 'tomato')

world <- map_data('world2', 'South Korea')
ggplot(world, aes(long, lat, group = group)) +
    geom_polygon(fill = 'orange', color = 'tomato')

?map_data()

p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth(se = T)
p + labs(title = "Fuel efficiency .vs. engine size",  #타이틀
         subtitle = "Two seaters are exceptional",    #서브타이틀
         caption = "Data for fueleconomy.gov",        #설명
         x = '엔진 크기',                             #x축 명
         y = '연비',                                  #y축 명
         color = '유형별 색상')                       #색상 구분

df <- mpg[, c(2,3,9,11)]

df %>% group_by(class) %>% filter(hwy > 30)

df.trim <- df %>% group_by(class) %>% filter(row_number(desc(hwy)) == 1)
best_in_case

p + geom_text(data = df.trim,
              aes(label = model)) # 텍스트 추가

p + geom_label(data = df.trim,
              aes(label = model),
              nudge_y = 2, alpha = 0.3) # 텍스트와 테두리 추가

label = tibble(
    displ = Inf, hwy = Inf,
    label = '마구니가 끼엇구나'
)
p + geom_text(data = label,
              aes(label = label),
              vjust = 'top',
              hjust = 'right')

p + theme(legend.position = 'left')
p + theme(legend.position = 'right')
p + theme(legend.position = 'top') + 
    guides(color = guide_legend(nrow = 1,
                                override.aes = list(size = 4)))
p + theme(legend.position = 'bottom')

p + theme_classic()
p + theme_bw()
p + theme_light()
p + theme_linedraw()
p + theme_dark()
p + theme_minimal()
p + theme_gray()
p + theme_void() + theme(legend.)

ggsave(file='myplot.pdf')

ggsave(file='myplot.jpg',
       width = 1920, height = 1080, units = 'px')


install.packages('networkD3')
# Library
library(networkD3)
library(dplyr)

# Make a connection data frame
links <- data.frame(
    source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
    target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
    value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
    name=c(as.character(links$source), as.character(links$target)) %>% 
        unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["red", "orange" , "yellow", "green", "blue", "purple", "pink", "white"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor1.html"))

links


install.packages('gapminder')
library(gapminder)

ggplot(gapminder,
       aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(alpha = 0.5) +
    scale_x_log10(labels = scales::dollar) +
    labs(title = 'GapMiner: GDP .vs. Life Expectancy',
         x = 'GDP per capita',
         y = 'Life Expectancy') +
    theme(plot.title = element_text(size = 18,
                                    face = 'bold',
                                    color = 'pink3'))

ggsave(file='myplot.jpg',
       width = 1920, height = 1080, units = 'px')


# 동그라미의 크기를 인구수(pop)에 대비시키시오.
ggplot(gapminder,
       aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point(alpha = 0.5, mapping = aes(size = pop)) +
    scale_x_log10(labels = scales::dollar) +
    labs(title = 'GapMiner: GDP .vs. Life Expectancy',
         x = 'GDP per capita',
         y = 'Life Expectancy') +
    theme(plot.title = element_text(size = 18,
                                    face = 'bold',
                                    color = 'pink3'))

str(Titanic)





install.packages('data.table')
library(data.table)
df <- fread('subwayfee.csv', encoding = 'UTF-8')

df$유임승하차 = df$유임승차 + df$유임하차
df$무임승하차 = df$무임승차 + df$무임하차

str(df)

ggplot(df,
       aes(x = 유임승하차, y = 무임승하차)) +
    geom_point(alpha = 0.5, mapping = aes(color = 호선명)) +
    geom_smooth() +
    labs(title = '지하철 유임승하차 .vs. 무임승하차',
         x = '유임승하차 인원 수',
         y = '무임승하차 인원 수') +
    scale_x_continuous(labels = scales::comma) +   #일반적인 스케일
    scale_y_continuous(labels = scales::comma) +   #일반적인 스케일
    theme(plot.title = element_text(size = 14,
                                    face = 'bold',
                                    color = 'pink3'))

pdf.options(family = "Korea1deb")

ggsave(file='subway2.pdf',
       width = 1920, height = 1080, units = 'px')

df$비율 = df$무임승하차 / df$유임승하차
df2 = df[,c('지하철역','비율')]

install.packages("wordcloud2")
library(wordcloud2)
word.subway <- wordcloud2(df2,size = 1.5)
?wordcloud2

install.packages('webshot')
library(webshot)
webshot::install_phantomjs()

library("htmlwidgets")
saveWidget(word.subway,"tmp.html",selfcontained = F)

webshot("tmp.html","subway.pdf", delay = 60, vwidth = 1920, vheight = 1080)







