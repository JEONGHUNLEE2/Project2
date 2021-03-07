#2021-03-06 
#전처리 데이터 merge 
# 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

save.image("merge작업_0306.RData")


age <- read.csv("age_one.csv")
gender <- read.csv("df_gender.csv")
device <- read.csv("df_device.csv")
kosis_data <- read.csv("merge_data.csv")

str(age)
age <- age %>% select(-X)

vars <- c('period','group','카테고리명','년도','월')
age[vars] <- map_df(.x=age[vars],.f = as.factor)

str(gender)
gender <- gender %>% select(-X)

vars <- c('년도','월')
gender[vars] <- map_df(.x=gender[vars],.f = as.factor)

str(device)
device <- device %>% select(-X)

device[vars] <- map_df(.x=device[vars],.f = as.factor)

head(age)
head(device)
head(gender)
merge_data <- merge(device, gender, 
      by.x = c('년도','월','검색기기', '카테고리명'), 
      by.y = c('년도','월','검색기기','카테고리명'), all.x = TRUE)
head(merge_data)

head(age)
tail(age)
age <- rename(age, '선호연령대_성별' = '성별')
age <- rename(age, '선호연령대_검색추이' = 'ratio')
age <- rename(age, '선호연령대' = 'group')

str(age)

age$검색기기 == "mobile"
age <- age %>% arrange(period)
head(age)
i <- 1
age[i, 6] <- "mobile"

age[i, 6] == "mobile"
age$검색기기[i] <- "모바일" #factor형 일때는 오류가 난다. 

for(i in 1:nrow(age)){
  if(age[i, 6] == "mobile"){
    age$검색기기[i] <- "모바일"
  }else if(age$검색기기[i] == "pc") {
    age$검색기기[i] <- "인터넷"
  }
}

table(age$검색기기)
#--
naver_data <- merge(merge_data, age, 
      by.x = c('period','년도', '월','카테고리명','검색기기'),
      by.y = c('period','년도', '월','카테고리명','검색기기'), all.x = TRUE)

head(naver_data)
head(age)

naver_data <- naver_data[,c(1,2,3,4,5,6,8,7,10,11,9)]

#---

str(kosis_data)
kosis_data <- kosis_data %>% select(-X)
kosis_data <- kosis_data %>% select(-경기심리지수_증감률_전월대비, -소비자지수_증감률_전월대비)

vars <- c('년도','월','중분류')
kosis_data[vars] <- map_df(.x = kosis_data[vars], .f = as.factor)
head(kosis_data)
head(naver_data)

merge_data <- merge(kosis_data, naver_data, 
      by.x = c('년도', '월', '판매매체별','중분류'), 
      by.y = c('년도', '월', '검색기기', '카테고리명'), all.x = TRUE)
merge_data <- merge_data %>% arrange(년도, 월, 중분류)

View(merge_data)
View(merge_data %>% filter(중분류 == "여가_생활편의"))

write.csv(merge_data, "kosis_naver_merge_data.csv")
