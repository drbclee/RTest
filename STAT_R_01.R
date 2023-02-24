
# https://kilhwan.github.io/bizstat-book/ch-introDescStat.html#%EA%B8%B0%EC%88%A0%ED%86%B5%EA%B3%84-%EA%B8%B0%EB%B2%95%EC%9D%98-%EC%A4%91%EC%9A%94%EC%84%B1

# 데이터 전체에 대한 요약 정보 파악하기

install.packages("devtools")
devtools::install_github("kilhwan/bizstatp")
library(bizstatp) 
course
# course 데이터는 어는 대학 과목의 수강생 데이터이다. 
# 그 과목을 들은 수강생 45명의 전공(major), 학년(grade), 성별(gender),
# 분반(class), 중간고사 점수(midterm), 기말고사 점수(final), 숙제 점수(hw), 최종 평가 점수(scores)가 포함되어 있다.
# 전공은 ME 전공인가 기타 전공인가로 구분되어 있고, 
# 분반은 해당 과목이 2개의 분반으로 진행되었으므로 1분반, 2분반으로 구분되어 있다.


# 1 데이터의 모든 열에 대하여 수치적으로 요약
summary(course) 
# 범주형 변수 - 빈도
# 수치형 변수는 최대, 최소, 평균, 1분위, 중앙앖, 3분위 (결측치 NA 갯수)


# 2. plot 을 이용한 변수사이의 상관성 - 산점도 
plot(course)

plot(course[,5:8]) # 5번째~8번째열이 수치형이므로 산점도


head(course)
# 3. 범주형 변수의 도수분포표
# # 기본 base 패키지의 table() 함수 (별도의 패키지 적재 불필요 없음)
# 기본 stat 패키지의 xtabs() 함수 (별도의 패키지 적재 불필요 없음)
# reshape2 패키지의 acast() 또는 dcast() 함수 (reshape2 패키지 적재 필요)
# dplyr 패키지의 count() 함수 (dplyr 패키지 적재 필요)

#3-1 table
# table(벡터), table(데이터프레임$열이름)

x <- c("a", "b", "b")
table(x)

table(course$year)
table(course$major)
table(course$gender)
table(course$class)


# 3-2 xtabs() 함수로 절대 빈도표 만들기
# 데이터 프레임에 있는 열을 table() 함수로 빈도표를 만드려면, $ 연산자를 사용하여 
# 데이터 프레임의 열을 지정해야 한다. 뒤에서 보겠지만 여러 열을 조합하여 분할표를 만드려면 매번 데이터 프레임의 열을 $ 연산자로 지정하여야 한다. 이는 매우 번거롭고 오류 발생을 증가시킨다.
# 
# xtabs() 함수를 사용하면 data 인수에 데이터 프레임을 지정한 후 빈도표나 분할표를 만들 때 사용할 데이터 프레임의 열을 수식(formula)의 형식으로 지정할 수 있다. 다음은 xtabs()의 기본 문법이다.
# 
# xtabs(수식, data=데이터프레임)
xtabs(~ year, data = course)
xtabs(~ year, data = course, subset = gender == "M")
xtabs(~ year, data = course, subset = class == 2)

# 상대빈도표
freqYear <- xtabs(~ year, data = course); freqYear
freqMajor <- xtabs(~ major, data = course); freqMajor
freqGender <- xtabs(~ gender, data = course); freqGender
freqClass <- xtabs(~ class, data = course); freqClass

proportions(freqYear)
proportions(freqGender)
proportions(freqYear) * 100
round(proportions(freqYear) * 100, digits=2) # 반올림 소수둘째자리


# 3-3 범주형 변수 분포를 그래프로 요약하기
# ggplot2 패키지의 geom_bar() 함수를 이용하면 x 속성에 매핑된 범주형 변수에 대하여 
# 범주별 절대 빈도(관측 도수)에 따라 막대 그래프를 그려준다. 
# 다음은 geom_bar() 함수를 사용하여 절대 빈도로 막대 그래프를 그리는 문법이다.
# ggplot(데이터, aes(x = 범주형변수)) + geom_bar()

library(ggplot2)    # R 세션을 다시 시작하였으면 ggplot2를 먼저 적재한다.  

# 절대빈도 그래프
ggplot(course, aes(x = year)) + geom_bar()
ggplot(course, aes(x = gender)) + geom_bar(fill="orange")

# 상대빈도 그래프
# ggplot(데이터, aes(x = 범주형변수, y = ..prop.., group = 1)) + geom_bar()  - group 속성 1.. 전체가 1이되도록.. 상대빈도
ggplot(course, aes(x = year, y=..prop.., group=1)) + geom_bar()

# 좌표축반전
ggplot(mpg, aes(x = class)) + geom_bar() 
ggplot(mpg, aes(x = class)) + geom_bar() + coord_flip()

## 빈도 순으로 막대그래프
# 범주형 변수의 나열 순서를 빈도 순으로 바꾸고 싶으면 다음처럼 reorder() 함수를 사용하여 범주가 나타나는 순서를 바꾼 다음에 x 속성에 매핑한다.
ggplot(mpg, aes(x = reorder(class, class, length))) + geom_bar() + coord_flip() + labs(x = "자동차 종류") 

# reorder(범주형변수, 기준변수, FUN)
course$year
reorder(course$year, course$year, length)

ggplot(course, aes(x=reorder(year, year, length)))  +   geom_bar()


course$gender
reorder(course$gender, course$score, mean)
ggplot(course, aes(x=reorder(gender, score, mean))) + geom_bar()
ggplot(course, aes(x=reorder(gender, score, mean), y=score))   +   geom_boxplot()


# 파이차트
pie(freqYear, main = "학년별 수강생 비율")
yearLabels <- paste0(names(freqYear), "학년: ", round(proportions(freqYear) * 100, digits=2),"%")
pie(freqYear, label = yearLabels, main = "학년별 수강생 비율")



########################################################################################
# 4 둘 이상의 범주형 변수 상관성 분석
 
# 기본 base 패키지의 table() 함수 (별도의 패키지 적재 불필요 없음)
# 기본 stat 패키지의 xtabs() 함수 (별도의 패키지 적재 불필요 없음)
# reshape2 패키지의 acast() 또는 dcast() 함수 (reshape2 패키지 적재 필요)
# dplyr 패키지의 count() 함수 (dplyr 패키지 적재 필요)


# 4.1 
# table(벡터1. 벡터2, ...)
# table(데이터프레임$열이름1, 데이터프레임$열이름2, ...)

table(course$gender, course$year)
table(course$year, course$gender)
table(course$gender, course$year, course$class)

# xtabs(~ 열이름1 + 열이름2 + ..., data = 데이터프레임)
freqGenderYear <- xtabs(~ gender + year, data=course); freqGenderYear
freqGenderClass <- xtabs(~ gender + class, data=course); freqGenderClass
freqGenderYearClass <- xtabs(~ gender + year + class, data=course); freqGenderYearClass


xtabs(~ gender + year, data=course, subset = score >= 80)
xtabs(~ gender + year, data=course, subset = score < 80)

# 교차표에서 빈도표 
# marginSums(교차표)               # 총 빈도 합산
# marginSums(교차표, margin = 1)   # 행으로 빈도 합산
# marginSums(교차표, margin = 2)   # 열로 빈도 합산
marginSums(freqGenderYear)

marginSums(freqGenderYear, margin=1)
addmargins(freqGenderYear) # 교차표에 행,열 합계 같이 표시


