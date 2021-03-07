#EDA
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

rm(list = ls())

save.image("EDA작업_0307.RData")

View(merge_data)
head(merge_data)
tail(merge_data)
str(merge_data)

merge_data$분기 <- 0
merge_data$분기[i] <- "1Q"

i<-1
for(i in 1:nrow(merge_data)){
  if(merge_data$월[i] %in% c("1","2","3")){
    merge_data$분기[i] <- "1Q"
  }else if(merge_data$월[i] %in% c("4","5","6")){
    merge_data$분기[i] <- "2Q"
  }else if(merge_data$월[i] %in% c("7","8","9")){
    merge_data$분기[i] <- "3Q"
  }else if(merge_data$월[i] %in% c("10","11","12")){
    merge_data$분기[i] <- "4Q"
  }
}



#1. 온라인 시장자체가 커지고 있다는 그래프 
merge_data %>% group_by(년도, 분기) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  arrange(년도, 분기) %>%
  ggplot(aes(x = paste(년도, 분기), y  = 분기별_매출액, fill = paste(년도, 분기)))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)


#2.모바일 비율이 높아지고 있다는 그래프
merge_data %>% group_by(년도, 분기, 판매매체별) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  arrange(년도, 분기) %>%
  mutate(전체_분기매출 = sum(분기별_매출액)) %>%
  mutate(분기_매출비율 = round(분기별_매출액 / 전체_분기매출, 3) * 100) %>% 
  ggplot(aes(x = paste(년도, 분기), y  = 분기별_매출액, fill = 판매매체별 ))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기_매출비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
  

#3. 종합몰/전문몰 , 온/온+오프라인 
sales_manage <- read.csv("kosis_운영형태별(온라인온오프라인).csv")
sales_speciality <- read.csv("kosis_운영형태별(종합몰전문몰).csv")
head(sales_manage)
head(sales_speciality)
#--> 추세 보기 
sales_manage <- sales_manage %>% select(-X)
sales_speciality <- sales_speciality %>% select(-X)


sales_manage <- sales_manage %>% filter(상품군별 != "합계")
sales_manage <- sales_manage %>% filter(운영형태별 != "계")

sales_speciality <- sales_speciality %>% filter(상품군별 != "합계")
sales_speciality <- sales_speciality %>% filter(범위별 != "계")

sales_manage$분기 <- 0
sales_manage$분기[i] <- "1Q"

i<-1
for(i in 1:nrow(sales_manage)){
  if(sales_manage$월[i] %in% c("1","2","3")){
    sales_manage$분기[i] <- "1Q"
  }else if(sales_manage$월[i] %in% c("4","5","6")){
    sales_manage$분기[i] <- "2Q"
  }else if(sales_manage$월[i] %in% c("7","8","9")){
    sales_manage$분기[i] <- "3Q"
  }else if(sales_manage$월[i] %in% c("10","11","12")){
    sales_manage$분기[i] <- "4Q"
  }
}
#온라인, 온오프라인 운영형태별 그래프
sales_manage %>% group_by(년도, 분기, 운영형태별) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  mutate(전체_분기매출 = sum(분기별_매출액)) %>%
  mutate(분기_매출비율 = round(분기별_매출액 / 전체_분기매출, 3) * 100) %>% 
  ggplot(aes(x = paste(년도, 분기), y = 분기별_매출액, fill = 운영형태별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기_매출비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


#종합몰, 전문몰 
sales_speciality$분기 <- 0
sales_speciality$분기[i] <- "1Q"

i<-1
for(i in 1:nrow(sales_speciality)){
  if(sales_speciality$월[i] %in% c("1","2","3")){
    sales_speciality$분기[i] <- "1Q"
  }else if(sales_speciality$월[i] %in% c("4","5","6")){
    sales_speciality$분기[i] <- "2Q"
  }else if(sales_speciality$월[i] %in% c("7","8","9")){
    sales_speciality$분기[i] <- "3Q"
  }else if(sales_speciality$월[i] %in% c("10","11","12")){
    sales_speciality$분기[i] <- "4Q"
  }
}
head(sales_speciality)

sales_speciality %>% group_by(년도, 분기, 범위별) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  mutate(전체_분기매출 = sum(분기별_매출액)) %>%
  mutate(분기_매출비율 = round(분기별_매출액 / 전체_분기매출, 3) * 100) %>% 
  ggplot(aes(x = paste(년도, 분기), y = 분기별_매출액, fill = 범위별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기_매출비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

#산업군별 pc, mo 비중
head(merge_data)
View(merge_data %>% filter(중분류 == "디지털_가전"))
merge_data %>% filter(상품군별 != "자동차 및 자동차용품")#자동자 및 자동차 용품 -> 자동차용품
View(merge_data)

#산업군별 온라인 시장 비중 
head(merge_data)
merge_data %>% group_by(년도, 분기, 중분류) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  mutate(전체_분기매출 = sum(분기별_매출액)) %>%
  mutate(분기_매출비율 = round(분기별_매출액 / 전체_분기매출, 3) * 100) %>% 
  ggplot(aes(x = paste(년도, 분기), y = 분기별_매출액, fill = 중분류))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기_매출비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

#pc, mo
head(merge_data)
merge_data %>% filter(판매매체별 == "인터넷") %>% 
  group_by(년도, 분기, 중분류) %>% 
  summarise(분기별_매출액 = sum(매출액)) %>% 
  mutate(전체_분기매출 = sum(분기별_매출액)) %>%
  mutate(분기_매출비율 = round(분기별_매출액 / 전체_분기매출, 3) * 100) %>% 
  ggplot(aes(x = paste(년도, 분기), y = 분기별_매출액, fill = 중분류))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기_매출비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
