#project 2
#install.packages("xlsx")
save.image("project2_R(0217).RData")
rm(list = ls())
library(tidyverse)
library(dplyr)
library(xlsx)
library(lubridate)

#1. 온라인쇼핑몰_판매매체별(온라인, 모바일)
sales_mobile <- read.xlsx2("온라인쇼핑몰_판매매체별_상품군별거래액_20210209181222.xlsx",1)
sales_mobile

colnames(sales_mobile)

name = c("상품군별","판매매체별",
         "2017_1","2017_2","2017_3","2017_4","2017_5","2017_6","2017_7","2017_8","2017_9",
         "2017_10","2017_11","2017_12",
         "2018_1","2018_2","2018_3","2018_4","2018_5","2018_6","2018_7","2018_8","2018_9",
         "2018_10","2018_11","2018_12",
         "2019_1","2019_2","2019_3","2019_4","2019_5","2019_6","2019_7","2019_8","2019_9",
         "2019_10","2019_11","2019_12",
         "2020_1","2020_2","2020_3","2020_4","2020_5","2020_6","2020_7","2020_8","2020_9",
         "2020_10","2020_11","2020_12")

names(sales_mobile) = name

sales_mobile <- gather(sales_mobile, 년월, 매출액, c("2017_1":"2020_12"))
head(sales_mobile)

sales_mobile %>% filter(판매매체별 %in% c("인터넷쇼핑","모바일쇼핑"))
#년, 월 컬럼 만들기
sales_mobile <- sales_mobile %>%  mutate(년도 = substr(sales_mobile$년월, 1,4))
sales_mobile <- sales_mobile %>%  mutate(월 = substr(sales_mobile$년월, 6,7))
head(sales_mobile)

#2. 온라인쇼핑몰_취급상품범위별(종합몰, 전문몰)
sales_speciality <- read.xlsx2("온라인쇼핑몰_취급상품범위별_상품군별거래액_20210209181201.xlsx", 1)
head(sales_speciality)
colnames(sales_speciality)
name2 <- c("상품군별","범위별",
           "2020_12","2020_11","2020_10","2020_9","2020_8","2020_7","2020_6","2020_5","2020_4","2020_3",
           "2020_2","2020_1",
           "2019_12","2019_11","2019_10","2019_9","2019_8","2019_7","2019_6","2019_5","2019_4","2019_3",
           "2019_2","2019_1",
           "2018_12","2018_11","2018_10","2018_9","2018_8","2018_7","2018_6","2018_5","2018_4","2018_3",
           "2018_2","2018_1",
           "2017_12","2017_11","2017_10","2017_9","2017_8","2017_7","2017_6","2017_5","2017_4","2017_3",
           "2017_2","2017_1")

names(sales_speciality) = name2
sales_speciality <- gather(sales_speciality, 년월, 매출액, c("2020_12":"2017_1"))
head(sales_speciality)
sales_speciality <- sales_speciality %>%  mutate(년도 = substr(sales_speciality$년월, 1,4))
sales_speciality <- sales_speciality %>%  mutate(월 = substr(sales_speciality$년월, 6,7))

#3.온라인쇼핑몰_운영형태별(온라인, 온/오프라인)
sales_manage <- read.xlsx2("온라인쇼핑몰_운영형태별_상품군별거래액_20210209181126.xlsx", 1)
head(sales_manage)

colnames(sales_manage)
name3 = c("상품군별","운영형태별",
         "2017_1","2017_2","2017_3","2017_4","2017_5","2017_6","2017_7","2017_8","2017_9",
         "2017_10","2017_11","2017_12",
         "2018_1","2018_2","2018_3","2018_4","2018_5","2018_6","2018_7","2018_8","2018_9",
         "2018_10","2018_11","2018_12",
         "2019_1","2019_2","2019_3","2019_4","2019_5","2019_6","2019_7","2019_8","2019_9",
         "2019_10","2019_11","2019_12",
         "2020_1","2020_2","2020_3","2020_4","2020_5","2020_6","2020_7","2020_8","2020_9",
         "2020_10","2020_11","2020_12")

names(sales_manage) <- name3
sales_manage <- gather(sales_manage, 년월, 매출액, c("2017_1":"2020_12"))

sales_manage <- sales_manage %>%  mutate(년도 = substr(sales_manage$년월, 1,4))
sales_manage <- sales_manage %>%  mutate(월 = substr(sales_manage$년월, 6,7))

unique(sales_manage$상품군별)

View(sales_manage)
#----------
head(sales_mobile)
head(sales_manage)
tail(sales_speciality)

data <- sales_mobile
data <- data %>% filter(상품군별 != "합계" |
                      판매매체별 != "계")
colnames(data)


data$season <- '0'
str(data)
data$season[data$월 == "12"] <- 'winter'
data$season[data$월 == "5"] <- 'spring'
data$season[data$월 == "8"] <- 'summer'
data$season[data$월 == "11"] <- 'fall'

data
table(data$season)

#----------코로나 합치기 
covid<-read.csv("owid-covid-data.csv")

covid<- covid%>%
  filter(iso_code=='KOR')


tail(covid)
str(covid)
colnames(covid)
#sales_mobile <-
covid <- covid %>% mutate(year = substr(covid$date, 1,4))
covid <- covid %>% mutate(month = substr(covid$date, 6,7))


covid <- covid[,c(1:6, 60, 61)]
head(covid)
tail(covid)
covid <- covid %>% filter(year == "2020")
#-------------------------------------
covid[is.na(covid)]<-0

covid<-covid %>% group_by(year, month) %>% summarise(total_case = sum(new_cases))

str(data)
tail(data)
str(covid)
str(data)

covid <- as.data.frame(covid)

data2 <- merge(data, covid, by.x = c("년도", "월"),
      by.y = c("year", "month"), all.x = T)

data2[is.na(data2)]<-0

data2$월 <- as.numeric(data2$월)
data2$년도<- as.numeric(data2$년도)

data2 <- data2 %>% arrange(년도, 월)

str(data2)
tail(data2)
head(data2)


str(data2)
tail(data2, 100)
head(data2)

data2$월 <- as.factor(data2$월)
data2$년도<- as.factor(data2$년도)

str(data2)

#---------------------------
merge_data <- data2
merge_data <- merge_data %>% filter(판매매체별 != "계")
#------------------
unique(merge_data$상품군별)
#----
head(merge_data)
colnames(merge_data)
merge_data <- merge_data %>% filter(상품군별 != "합계")
#-----------------------------
customer <- read.xlsx2("경제심리지수.xlsx", 1)
