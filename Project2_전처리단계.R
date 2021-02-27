#project 2
#install.packages("xlsx")
#install.packages("kimisc")
#install.packages('data.table')

save.image("project2_R(0226).RData")
rm(list = ls())
library(tidyverse)
library(dplyr)
library(xlsx)
library(lubridate)
library(kimisc)
library(readxl)
library(data.table)


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
head(customer)

name4 = c("지표선택별",
          "2017_1","2017_2","2017_3","2017_4","2017_5","2017_6","2017_7","2017_8","2017_9",
          "2017_10","2017_11","2017_12",
          "2018_1","2018_2","2018_3","2018_4","2018_5","2018_6","2018_7","2018_8","2018_9",
          "2018_10","2018_11","2018_12",
          "2019_1","2019_2","2019_3","2019_4","2019_5","2019_6","2019_7","2019_8","2019_9",
          "2019_10","2019_11","2019_12",
          "2020_1","2020_2","2020_3","2020_4","2020_5","2020_6","2020_7","2020_8","2020_9",
          "2020_10","2020_11","2020_12")
names(customer) <- name4
customer <- customer %>% filter(지표선택별 == "경제심리지수(원계열)")
customer <- gather(customer, 년월, 경기심리지수, c("2017_1":"2020_12"))


customer$지표선택별[customer$지표선택별 == "경제심리지수(원계열)"] <- '경제심리지수'
customer[is.na(customer)] <- 0
str(customer)
customer$경기심리지수 <- as.numeric(customer$경기심리지수)
customer$증감률_2017기준 <- customer$경기심리지수 - 96.2

customer <- rename(customer, "변동치(2017기준)" = "증감률_2017기준")
#2017기준 증감률

#customer$증감률 <- 
#성장률 공식: R = (C - O) / O 
customer$증감률_2017기준 <- round(((customer$경기심리지수 - 96.2) / 96.2),3) * 100

customer$증감률_전월대비 <- shift(customer$경기심리지수, fill = customer$경기심리지수[1])
customer$증감률_전월대비 <- round((((customer$경기심리지수/customer$증감률_전월대비) - 1) * 100),2)
#------------------------------------------
str(merge_data)
merge_data <- merge_data %>% select(-년월)
head(customer)
str(customer)
customer <- customer %>% mutate(년도 = substr(customer$년월, 1,4))
customer <- customer %>% mutate(월 = substr(customer$년월, 6,7))
##----
merge_data <- merge(merge_data, customer, 
      by.x = c("년도", "월"),
      by.y = c("년도", "월"),
      all.x)

str(merge_data)
merge_data <- merge_data %>% select(-c("지표선택별","년월"))
#--------------------------------------------------
#오프라인 데이터 
of_df <- read.xlsx2("오프라인_유통데이터.xlsx", 1)
str(of_df)

of_df$이용금액 <- as.character(of_df$이용금액)
of_df$이용금액 <- as.numeric(of_df$이용금액)

of_df$전체_이용금액 <- as.character(of_df$전체_이용금액)
of_df$전체_이용금액 <- as.numeric(of_df$전체_이용금액)

off_df_gender <- of_df
write.csv(off_df_gender, "off_df_gender.csv")

#---
of_df2 <- read.xlsx2("오프라인_유통데이터.xlsx", 2)
str(of_df2)
head(of_df2)
of_df2$이용금액 <- as.character(of_df2$이용금액)
of_df2$이용금액 <- as.numeric(of_df2$이용금액)

of_df2$전체_이용금액 <- as.character(of_df2$전체_이용금액)
of_df2$전체_이용금액 <- as.numeric(of_df2$전체_이용금액)

off_df_age <- of_df2
write.csv(off_df_age, "off_df_age.csv")

#----
rm(of_df2)
#merge_data, off_df_age, gender
View(merge_data)
unique(merge_data$상품군별)
merge_data <- merge_data %>% filter(상품군별 != "합계")
#----0224
merge_data <- merge_data %>% filter(상품군별 != c("음식서비스","기타서비스","기타"))

write.csv(merge_data, "merge_data.csv")
#----------------------------------------------------------------------------
#소비자심리지수 
head(merge_data) 
colnames(merge_data)

customer_2 <- read.csv("301_DT_040Y002_20210226115452.csv")
head(customer_2)
View(customer_2)

customer_2 <- customer_2 %>% filter(지수코드별 == "소비자심리지수")

colnames(customer_2)

name5 = c("지수코드별","분류코드별","항목", "단위",
          "2017_1","2017_2","2017_3","2017_4","2017_5","2017_6","2017_7","2017_8","2017_9",
          "2017_10","2017_11","2017_12",
          "2018_1","2018_2","2018_3","2018_4","2018_5","2018_6","2018_7","2018_8","2018_9",
          "2018_10","2018_11","2018_12",
          "2019_1","2019_2","2019_3","2019_4","2019_5","2019_6","2019_7","2019_8","2019_9",
          "2019_10","2019_11","2019_12",
          "2020_1","2020_2","2020_3","2020_4","2020_5","2020_6","2020_7","2020_8","2020_9",
          "2020_10","2020_11","2020_12")
names(customer_2) <- name5

customer_2 <- customer_2 %>% select(1, 5:52)
customer_2 <- gather(customer_2, 년월, 소비자심리지수, c("2017_1" : "2020_12"))
str(customer_2)

customer_2$소비자지수_변동치_2017기준 <- customer_2$소비자심리지수 - 93.4
customer_2$소비자지수_증감률_2017기준 <- round(((customer_2$소비자심리지수 - 93.4) / 93.4),3) * 100
customer_2$소비자지수_증감률_전월대비 <- shift(customer_2$소비자심리지수, fill = customer_2$소비자심리지수[1])
customer_2$소비자지수_증감률_전월대비 <- round((((customer_2$소비자심리지수/customer_2$소비자지수_증감률_전월대비) - 1) * 100),2)


head(customer_2)
customer_2 <- customer_2 %>% mutate(년도 = substr(customer_2$년월, 1,4))
customer_2 <- customer_2 %>% mutate(월 = substr(customer_2$년월, 6,7))
#----------------------------------------------------
colnames(merge_data)

merge_data <- merge_data %>% select(-c("소비자심리지수","소비자지수_증감률_2017기준","소비자지수_증감률_전월대비"))
merge_data <- merge(merge_data, customer_2, 
                    by.x = c("년도", "월"),
                    by.y = c("년도", "월"),
                    all.x)
merge_data <- merge_data %>% select(-지수코드별, -년월)

View(merge_data)
colnames(merge_data)
merge_data <- rename(merge_data, "경기심리지수_변동치_2017기준" = "변동치(2017기준)")
merge_data <- rename(merge_data, "경기심리지수_증감률_2017기준" = "증감률_2017기준")
merge_data <- rename(merge_data, "경기심리지수_증감률_전월대비" = "증감률_전월대비")
#--------------------------------------------------
View(merge_data)
merge_data <- merge_data %>% arrange(년도, 월)
write.csv(merge_data, "merge_data.csv")

