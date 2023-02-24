3+4
x=c(2,5,1)
sort(x,
     decreasing = TRUE
     )
X
x


####################################################
#19차 일변량 질적자료분석

install.packages("ggplot2")
library(ggplot2)
# 일변량 질적자료 cut, color, clarity 변수

## (1) 빈도구하기
##  table(데이터명$변수명)
table(diamonds$cut)
table(diamonds$color)
table(diamonds$clarity)

## sort(빈도의 결과, decreasing=TRUE)
sort(table(diamonds$cut),decreasing = TRUE)
sort(table(diamonds$color),decreasing = TRUE)
sort(table(diamonds$clarity),decreasing = TRUE)

## (2) 백분율 구하기
## prop.table(빈도의 결과)*100
prop.table(table(diamonds$cut))*100

## round( 수치, digit=1)
round(prop.table(table(diamonds$cut))*100, digits = 1)
## sort( 백분율의 결과, decresing=TRUE)
sort(round(prop.table(table(diamonds$cut))*100, digits = 1), decreasing = TRUE)


install.packages("prettyR")
library(prettyR)
## prettyR::freq()
freq(diamonds$cut)
freq(diamonds$cut, display.na=FALSE)



# (3) 막대그래프
## barplot
barplot(table(diamonds$cut))
barplot(sort(table(diamonds$cut), decreasing = TRUE), col="purple")
barplot(sort(table(diamonds$cut), decreasing = TRUE), main = "다이아몬드 품질현황")
barplot(sort(table(diamonds$cut), decreasing = TRUE), main = "다이아몬드 품질현황", ylab="빈도")
barplot(sort(table(diamonds$cut), decreasing = TRUE), main = "다이아몬드 품질현황", ylab="빈도",
        ylim=c(0,25000))
barplot(sort(table(diamonds$cut), decreasing = TRUE), main = "다이아몬드 품질현황", xlab="빈도",
        xlim=c(0,25000), horiz=TRUE)

## plotly 패키지
install.packages("plotly")
library(plotly)
plot_ly(data=diamonds, x= ~ levels(cut), y= ~ table(cut), type="bar")
ggplot(data=diamonds, mapping=aes(x=cut)) + geom_bar()

# (4) 원그래프
# pie 함수
pie(table(diamonds$cut))
pie(table(diamonds$cut), radius=1) #반지름을 최대로 지정
# 원조각의 처음 시작하는 각도를 -30으로 지정
pie(table(diamonds$cut), radius=1, init.angle=-30)

plot_ly(data=diamonds, values=~table(cut), levels=~levels(cut), type="pie")

ggplot(data=diamonds, aes(x="", fill=cut)) + geom_bar(width=1) + coord_polar("y")
