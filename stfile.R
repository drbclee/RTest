##################################################################################
# 4차시 상관분석
install.packages("corrplot")
library(corrplot)
install.packages("lattice")
library(lattice) #다운 받은 패키지를 사용하기 위해 쓰는 함수

# 상관분석을 위한 mtcars 데이터셋 불러오기
a=mtcars
a

#   gear와 carb의 상관계수 산출
mcor2 = cor(mtcars$gear, mtcars$carb)
mcor2

xyplot(gear~carb, data=mtcars)

# 다른 그림
lm=plot(mtcars$gear,mtcars$carb)
abline(lm(mtcars$gear~mtcars$carb)) # lm 리니어모델 회귀분석

mcor=cor(mtcars)
mcor

# 소수점 2자리까지 정리
round(mcor,2)

corrplot(mcor)  # 상관계수 패키지 

plot(mtcars)


## ggplot2 패키지 아용하여 qplot 명령어 사용하면 산점도 가능
## 미적특성 - 색상, 모양, 크기 매핑하여 적용하는 시각화 패키지
install.packages("ggplot2")
library(ggplot2)
qplot(gear, carb, data=mtcars)


# wt 무게와 mpg 연비 상관관계
cor(mtcars$wt, mtcars$mpg)
qplot(wt,mpg, data=mtcars, color=factor(carb))




###############################################################################
# 5차시 회귀분석
# 근무년수가 연봉에 영향을 주는가?
year=c(26,16,20,7,22,15,29,28,17,3,1,16,19,13,27,4,30,8,3,12)
annual_salary=c(1267,887,1022,511,1193,795,1713,1477,991,455,324,944,1232,808,1296,486,1516,565,299,830)
Data=data.frame(year,annual_salary)
summary(Data)

plot(year,annual_salary)
cor(year,annual_salary)
LS=lm(annual_salary~year, data=Data)
summary(LS)



##############################
# 분산분석
# 등분산성을 가정함 검정전 levene의 등분산성 검정을 통해 등분산성을 확인
install.packages('car')
library(car)
# anorexia 거식증 데이터
data(anorexia, package = "MASS")
anorexia
leveneTest(Postwt~Treat, data=anorexia)

# 일원분산분석 aov
out1=aov(Postwt~Treat, data = anorexia)
out1
summary(out1)


out2=anova(lm(Postwt~Treat, data=anorexia))
out2
summary(out2)


out3=oneway.test(Postwt~Treat, data=anorexia)
out3
summary(out3)


##################################################
# 로지스틱 회귀분석
# 복약종류와 횟수에 따른 반응여부(반응없음 0, 반응있음1)


dose=c(1,1,2,2,3,3)
response=c(0,1,0,1,0,1)
count=c(7,3,5,5,2,8)
toxic=data.frame(dose,response,count)
toxic
out=glm(response~dose, weights=count, family = binomial, data = toxic)
summary(out)


#---------------------
plot(response~dose, data = toxic, type = "n", main="predic")

curve(predict(out,data.frame(dose=x), type="resp"), add=TRUE)





##########################################
# 의사나무 분석 rpart
# 붓꽃의 꽃잎 길이와 너비를 규칙으로, 종류를 분석
install.packages("rpart")
library(rpart)
x11() # R Graphics 실행


formula=Species~.
iris.df=rpart(formula, data=iris)
plot(iris.df)
text(iris.df, use.n=T, cex=0.7) # cex = 문자의 크기
post(iris.df, file="")


################################################
# 시계열분석
# 한국의 2018~2021 1인당 GDP 예측하기
# 한국의 GDP 내용이 포함된 패키지: WDI
install.packages('WDI')
library(WDI)

gdp <- WDI(country = 'KR', 
           indicator = c('NY.GDP.PCAP.CD', 'NY.GDP.MKTP.CD'),
           start = 1960,
           end = 2017)
gdp


# 칼럼명 수정
names(gdp) <- c('iso2c', 'Country', 'Year', 'PerCapGDP', 'GDP')
head(gdp)


# 시계열 데이터로 변환
kr = gdp$PerCapGDP[gdp$Country == "Korea, Rep."]
kr = ts(kr, start = min(gdp$year, end = max(gdp$yea)))
kr


# 시계열 예측 모델 import
install.packages('forecast')
library(forecast)

# 데이터를 활용하여 최적의 ARIMA 모형 선택
krts = auto.arima(x = kr)
krts
# 미래예측
Forecasts = forecast(object = krts, h = 5)
Forecasts

# 시계열 그래프 출력
plot(Forecasts)




####################################################
# R군집화
# Iris를 이용해 K = 3인 K-means 군집화

# 데이터 로드
data(iris)
a = iris
a

# 붓꽃의 종에 대한 사전정보를 제거(군집분석은 사전정보가 필요 없음)
a$Species = NULL

# k=3인 k-means 군집분석
kc = kmeans(a, 3)
kc
summary(kc)

table(iris$Species, kc$cluster)
plot(a[c('Sepal.Length', 'Sepal.Width')], col = kc$cluster)



#########################################################
# 11차시 파생변수 실습
# melt(): 식별자 id, 측정 변수 variable, 측정치 value 형태로 데이터를 재구성하는 함수
# reshape2::melt.data.frame(
#   data,         # melt할 데이터
#   id.vars,      # 식별자 컬럼들
#   measuer.vars, # 측정치 컬럼들
#   na.rm = False # NA인 행을 결과에 포함시킬지 여부. False는 NA를 제거하지 않음
# )
# 
# ex) melt1 = melt(data, id = 'names') # names 변수를 id변수로 재구성
# 
# 
# cast(): 새로운 구조로 데이터를 만드는 함수(melt_Data, 변수1, 변수2 ~ column 생성 변수명)
#  
 

# dplyr 패키지: 데이터 프레임을 처리하는 함수로 구성된 패키지
# # - data.table
# # - 각종 데이터베이스: MySQL, PostgreSQL, SQLite, BigQuery 지원
# # - 데이터 큐브: dplyr 패키지 내부에 실험적으로 내장됨
# filter()	지정한 조건식에 맞는 데이터 추출	subset()
# select()	열의 추출	data[, c('Year', 'Month')]
# mutate()	열 추가	transform()
# arrange()	정렬	order, sort()
# summarise()	집계	aggregate()

# hflights 데이터: 미국 휴스턴에서 출발하는 
# 모든 비행기의 2011년 이착륙기록이 수록된 것으로 227496건의 
# 이착륙기록에 대해 21개 항목을 수집한 데이터


install.packages('dplyr')
install.packages('hflights')
library(hflights)
library(dplyr)
str(hflights) # hflights의 구조 출력


# 변수명 확인
colnames(hflights)


# 자료를 보기 좋게 한 화면에 편집
hflights_df <- tbl_df(hflights)
hflights_df

# 파생변수 추가(칼럼 추가)
aa <- mutate(hflights_df, gain=ArrDelay - DepDelay)

# 변수가 새로 생성된 것을 확인
aa





#########################################################
# 12차시앙상블 기법을 활용한 분석모델 확장
# 1) 랜덤포레스트
install.packages("randomForest")
library(randomForest)

idx = sample(2, nrow(iris), replace = T, prob = c(0.7,0.3)) 
# sample = 분류 지표(예: 2 -> 1 or 2)
# prob = probability(예: c(0.7, 0.3) -> 1 = 70%, 2 = 30%)

idx  

trainData = iris[idx == 1,] # 150개 데이터를 1 train 70%, 2 test 30%로 만듬
testData = iris[idx == 2,]

model = randomForest(Species~., data = trainData, ntree = 100, proximity = T)
# ntree: 의사결정나무 갯수

model

# model 결과화면 설명
# - OOB(Out Of Bag)(estimate of error rate, 에러 추정치): 3.74%(모델 훈련에 사용되지 않은 데이터를 사용해 에러 추정)
# - 의사결정나무 갯수(Number of trees): 100
# - 혼동행렬을 통해 정분류, 오분류된 케이스 갯수를 알 수 있음


importance(model) # 지니계수: 값이 높은 변수가 클래스를 분류하는데 가장 큰 영향을 줌

# 기타
# - 배깅은 party와 caret 라이브러리를 사용해서 부트스트랩과 모델링을 함
# - 부스팅은 tree 라이브러리와 rpart 패키지를 사용하여 표본 추출과 트리 모델을 형성할 수 있음


#########################################################
# 13차시 예측 오차를 통한 예측 모델 성능 평가

#R제공 데이터 cars 사용
head(cars, 3)  #speed와 제동거리 회귀

# lm 함수 이용, 회귀 모델 생성
a = lm(dist~speed, cars)
a
# => 추정된 회귀식: -17.579 + 3.932*speed + e
# * 회귀계수, 예측값, 잔차, 회귀계수 신뢰구간, 잔차제곱합 계산
# - 회귀계수: coef()
# - 예측값: fitted()
# - 잔차: residuals()
# - 회귀계수 신뢰구간: confint()
# - 잔차제곱합: deviance()

# 회귀계수
coef(a)

# 예측값 계산
fitted(a)[1:4]

# 잔차 계산
residuals(a)[1:4]

# 회귀계수 신뢰구간 계산
confint(a)

# 잔차 제곱합 계산
deviance(a)


# predict 함수 사용, x = 4일때 예측값 구하기
predict(a, newdata = data.frame(speed=4))

# 예측값의 신뢰구간 구하기
predict(a, newdata = data.frame(speed=4), interval = 'confidence')

# 오차항 고려한 예측값의 신뢰구간
predict(a, newdata = data.frame(speed=4), interval = 'prediction')

# 회귀모형 평가
summary(a)


# 잔차의 등분산성 검정: 출력창 2*2 분할
# - 기울기가 0인 직선일수록 등분산성 검정에서 이상적
# - 패턴없이 무작위 분포를 보일수록 좋은 적합

par(mfrow=c(2, 2))
plot(a)

# (1) Residuals vs Fitted Graph
# - X축: 선형회귀로 예측된 값
# - Y축: 잔차
# - 선형 회귀(오차: 평균 0, 분산: 정규분포)로 가정하였으므로 기울기 0인 직선이 관측되는 것이 이상적
# (2) Normal Q-Q Graph
# - 잔차가 정규 분포를 따르는지 확인하기 위한 Q-Q도
# (3) Scale-Location Graph
# - X축: 선형회귀로 예측된 값
# - Y축: 표준화 잔차
# - 기울기 0인 직선이 이상적
# (4) Residuals vs Leverage
# - X축: Leverage
# - Y축: 표준화 잔차
# - Leverage: 설명 변수가 얼마나 극단에 치우쳐 있는지를 의미
# - 이상치가 나타나지 않는 것이 이상적


# 잔차의 정규성 검정: 잔차 추출
res = residuals(a)

# 샤피로 윌크 정규성 검정
# => p-value > 0.05: 정규성을 따름
# => p-value < 0.05: 정규성을 따르지 않음
shapiro.test(res)


# 잔차의 독립성 검정(Durbin-Watson)

library(lmtest)
dwtest(a) #==> (dw = 1.6762) : 2에 가까우므로, 자기상관이 거의 없다. => 회귀분석을 실시할 수 있다.




#####################################################################
# R을 이용한 컨퓨전 매트릭스 분류 모델 성능 평가
# R제공 데이터 iris 이용, iris 데이터 10개 추출
head(iris, 10)

# iris 데이터 특성치 확인
summary(iris)

# import 
# 분류와 컨퓨전 매트릭스 수행
install.packages('party')
install.packages('caret')
install.packages('e1071')

library(party) # 의사결정나무 시행
library(caret) # 교차분석 패키지
library(e1071) # 컨퓨전 매트릭스 패키지

# data 구분
# sample data
sp = sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))

# 70%, 30% 학습 데이터와 테스트 데이터 set 생성
trainData = iris[sp==1, ]
testData = iris[sp==2, ]


# 분류 알고리즘 생성
# 크기와 종에 따른 분류 알고리즘 생성
myFomula = Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width

# 알고리즘을 이용한 학습 데이터 셋 ctree(분류나무) 생성
iris_ctree = ctree(myFomula, data = trainData)
iris_ctree

# 컨퓨전 매트릭스 생성
# 예측 정도와 학습 데이터 셋의 컨퓨전 매트릭스 생성
table(predict(iris_ctree), trainData$Species)

# 컨퓨전 매트릭스 함수를 이용한 혼동행렬 생성
confusionMatrix(predict(iris_ctree), trainData$Species)

# ctree 도표 생성
plot(iris_ctree)



# Test Data 적용
# 테스트셋 데이터 예측분류 모델
testPred = predict(iris_ctree, newdata = testData)

# 테스트셋 분류 데이터 컨퓨전 매트릭스
table(testPred, testData$Species)

# 컨퓨전 매트릭스 생성 함수 이용
confusionMatrix(testPred, testData$Species)
