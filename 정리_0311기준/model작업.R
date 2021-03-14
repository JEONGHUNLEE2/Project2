#Model 적합
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(PerformanceAnalytics)

rm(list = ls())

save.image("model작업.RData")

#1. train, test 나누기!(kosis_data, merge_data) 두개다
#1-1 전체데이터
train_data <- drop_na((merge_data))
test_data <- train_data %>% filter(년도 == "2020" & 월 %in% c("10","11","12"))
data2019 <- train_data %>% filter(년도 != "2020")
data2020 <- train_data %>% filter(년도 == "2020")
data2020 <- data2020 %>% filter(월 %in% c("1","2","3","4","5","6","7","8","9"))
train_data <- rbind(data2019, data2020)

head(test_data)
head(train_data)

#kosis만 --------------k_test, train 
k_test_data <- kosis_data %>% filter(년도 == "2020" & 월 %in% c("10","11","12"))
data2019 <- kosis_data %>% filter(년도 != "2020")
data2020 <- kosis_data %>% filter(년도 == "2020")
data2020 <- data2020 %>% filter(월 %in% c("1","2","3","4","5","6","7","8","9"))
k_train_data <- rbind(data2019, data2020)

#2. kosis_data 데이터로만 다시 한번 모델적합 - > 여기서 상관성이 있고 없고를 시각화를 통해 보여주고, 변수 채택을 했다 이런식에 스토리
head(k_train_data)
colnames(k_train_data)

k_train_data %>% group_by(년도, 월) %>% summarise(월별_매출액 = sum(매출액), 소비자심리지수 = mean(소비자지수_증감률_2017기준)) %>% 
  ggplot(aes(x = 소비자심리지수, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)
  
k_train_data %>% group_by(년도, 월) %>% summarise(월별_매출액 = sum(매출액), 경기심리지수 = mean(경기심리지수_증감률_2017기준)) %>% 
  ggplot(aes(x = 경기심리지수, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

k_train_data %>% filter(년도 == "2020") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 = sum(매출액), 코로나_확진자수 = mean(코로나확진자수)) %>% 
  ggplot(aes(x = 코로나_확진자수, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

k_train_data %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 = sum(매출액), 코로나_확진자수 = mean(코로나확진자수)) %>% 
  ggplot(aes(x = 코로나_확진자수, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

tail(kosis_data)
tail(merge_data)
kosis_data %>% filter(년도 == "2020") %>% group_by(월) %>% summarise(코로나 = mean(코로나확진자수))
merge_data %>% filter(년도 == "2020") %>% group_by(월) %>% summarise(코로나 = mean(코로나확진자수))
#여기서 -----------------x= 모바일 검색추이, y 는 매출액 이걸로 한번 점찍어보기 
head(train_data)
#모바일
train_data %>% filter(판매매체별 =="모바일") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(기기별_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

train_data %>% filter(판매매체별 =="인터넷") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(기기별_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

head(train_data)

train_data %>% filter(판매매체별 =="모바일") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(선호_성별_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

train_data %>% filter(판매매체별 =="인터넷") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(선호_성별_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

train_data %>% filter(판매매체별 =="모바일") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(선호연령대_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)

train_data %>% filter(판매매체별 =="인터넷") %>% 
  group_by(년도, 월) %>% summarise(월별_매출액 =sum(매출액), 검색추이 = mean(선호연령대_검색추이)) %>% 
  ggplot(aes(x = 검색추이, y = 월별_매출액)) +
  geom_point(stat = "identity", color = "red" , size = 2)+
  scale_y_continuous(label = scales::comma)


#---------------------------------------------------------------------------------------------------
covid <- read.csv("covid_data_2020.csv")
covid 

zz <- merge(merge_data, covid, 
      by.x = c("년도", "월"), 
      by.y = c("year", "month"), 
      all.x = TRUE)

zz %>% filter(년도 == "2020") %>% group_by(월) %>% summarise(코로나 = mean(코로나확진자수))
head(zz)
tail(zz)
zz %>% filter(년도 == "2020" & 월 == "9")

zz$코로나확진자수 <- 0
i <- 1
for(i in 1:nrow(zz)){
  if(is.na(zz$total_case.y[i])) {
    zz$코로나확진자수[i] <- 0
  }else{
    zz$코로나확진자수[i] <- zz$total_case.y[i]
  }
}

zz<-zz %>% select(-total_case.x, -total_case.y)
head(zz)
tail(zz)
zz <- zz[,c(1:10, 21, 11:20)]
merge_data <- zz
colnames(zz)

kosis_data <- zz[,c(1:15)]
#---------------------------------------------------------------------------------------------------
#2-2 k_tain_data / k_test_datas
#상관분석
head(k_train_data)
tail(k_train_data)
str(k_train_data)
colnames(k_train_data)

cor_data <- k_train_data[,c(1,2,7,11,12,13,14,15)]
str(cor_data)
cor(cor_data, use = "na.or.complete")

chart.Correlation(cor_data, histogram=TRUE, pch=19)

cor <- map_lgl(.x = cor_data, .f = function(x){
  test<- cor.test(x = x, y = cor_data$매출액)
  result <- test$p.value > 0.05
  return(result)
})
cor

#범주형 컬럼------------------------------------------------------------------------------------------------
shapiro.test(anova_data$매출액) #-> 생략

t_data_1<-k_train_data[k_train_data$판매매체별=="모바일",]
t_data_2<-k_train_data[k_train_data$판매매체별=="인터넷",]

var.test(t_data_1$매출액, t_data_2$매출액) #0.05 이하니까 등분산성 만족하지 않는다.

t.test(매출액~판매매체별,data=k_train_data,var.equal=F) 

#상품군별_분산분석(그룹이 3개이상이라서)
head(k_train_data)
anova_data <- k_train_data[,c(4,5,6,7)]

ggplot(anova_data,aes(x=factor(중분류),y=매출액,fill=factor(판매매체별))) + geom_boxplot()

#정규성검증 -> 중심극한정리로 생략
#등분산성 검증
bartlett.test(매출액 ~ as.factor(상품군별),data=anova_data) # 등분산성 만족 X  -> var.equal = F로 진행
oneway.test(매출액 ~ as.factor(상품군별), data=anova_data,var.equal = F) #각 그룹간의 평균이 다르다. 

#여기서 중분류와 상품군별 그거를 잘 정리해야할거 같네.. 어떻게 사용할지, 중분류는 merge용으로만 쓰고 버려야할지 
a1 <- aov(매출액 ~ as.factor(상품군별), data=anova_data)
head(anova_data)
#a2 <- aov(매출액 ~ as.factor(중분류), data=anova_data)
#LDuncan(a2, "group")
library(laercio)
LDuncan(a1, "group")
#----------------------------------------------------------------------------------------------------------통계검정끝







#다중선형회귀분석-----------여기선 이제 전월대비매출현황, 전월대비 증감제거, -- 범주형 예측할 땐 증감 컬럼 넣고 
#RMSE : 199057.4
colnames(k_train_data)
kosis_train_data <- k_train_data %>% select(-전월_대비매출현황, -전월대비_증감) 
kosis_test_data <-  k_test_data %>% select(-전월_대비매출현황, -전월대비_증감) 

kosis_train_data$년도 <- as.factor(kosis_train_data$년도)
kosis_train_data$월 <- as.factor(kosis_train_data$월)

str(kosis_train_data)
str(kosis_test_data)

full <- lm(매출액 ~., data = kosis_train_data)
null <- lm(매출액 ~ 1, data = kosis_train_data)
fit1<- step(null, 
            scope = list(lower = null, upper = full),
            direction = 'both')


summary(fit1)
#install.packages("MLmetrics")



real <- kosis_test_data$매출액
pred1 <-predict(fit1, newdata = kosis_test_data, type = 'response') #여기에 test_data 를 넣었어야하네 


regMeasure(real, pred1)

regMeasure <- function(real, pred){
  library(MLmetrics)
  result <- data.frame(
    MSE = MSE(y_pred = pred, y_true = real),
    RMSE = RMSE(y_pred = pred, y_true = real),
    MAE = MAE(y_pred = pred, y_true = real),
    MAPE = MAPE(y_pred = pred, y_true = real)
  )
  return(result)
}

#랜덤포레스트
#RMSE : 186315.5
#install.packages("randomForest")
library(randomForest)
set.seed(seed = 1234)
fit2 <- randomForest(formula = 매출액 ~.,
                     data = kosis_train_data,
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE)

print(fit2)
importance(fit2, type = 1) #변수중요도 확인
varImpPlot(fit2, main = 'Variable Importance', type = 1)

pred2 <-predict(fit2, newdata = kosis_test_data, type = 'response')

regMeasure(real, pred2)

#의사결정나무
#RMSE : 182041.3
library(rpart)
Ctrl <- rpart.control(minsplit = 20,
                      cp = 0.01,
                      maxdepth = 10)
set.seed(seed = 1234)

fit3 <- rpart(formula = 매출액~.,
              data = kosis_train_data,
              control = Ctrl)

summary(fit3)
printcp(fit3)
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit3, 
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)

plotcp(fit3)

pred3 <- predict(object = fit3, newdata = kosis_test_data, type = 'vector')
RMSE(real, pred3)


#3. train_data / test_data로 -------------------------------------------------------------------------------------
#랜덤포레스트
#RMSE : 224199.5
#install.packages("randomForest")
library(randomForest)
set.seed(seed = 1234)
fit2 <- randomForest(formula = 매출액 ~.,
                     data = merge_train_data,
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE)

print(fit2)
importance(fit2, type = 1) #변수중요도 확인
varImpPlot(fit2, main = 'Variable Importance', type = 1)

real <- merge_test_data$매출액
pred2 <-predict(fit2, newdata = merge_test_data, type = 'response')

regMeasure(real, pred2)

#의사결정나무
#RMSE : 124755.5
library(rpart)
Ctrl <- rpart.control(minsplit = 20,
                      cp = 0.01,
                      maxdepth = 10)
set.seed(seed = 1234)

fit3 <- rpart(formula = 매출액~.,
              data = merge_train_data,
              control = Ctrl)

summary(fit3)
printcp(fit3)
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit3, 
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)

plotcp(fit3)

pred3 <- predict(object = fit3, newdata = merge_test_data, type = 'vector')

RMSE(real, pred3)

#--> 의사결정나무로 모델을 돌리면 훨씬 높은 성능을 보이는데 랜덤포레스트는 오히려 RMSE값이 KOSIS만 돌렸을 때 보다 더 높게 나옴....
#--> 결론 naver 검색추이까지 합쳐서 의사결정나무 돌리는게 훨씬 좋음
