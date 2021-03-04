#KOSIS + 네이버데이터랩
#install.packages("tidyverse")
#install.packages('data.table')

library(dplyr)
library(ggplot2)
#library(xlsx)
library(lubridate)
library(data.table)
#library(PerformanceAnalytics)

rm(list = ls())

save.image("project2_Kosis_Naver_data0304.RData")
aa <- read.csv("mw_data_기기별.csv")
bb <- read.csv("mw_data_성별.csv")
cc_age <- read.csv("mw_data_연령대.csv")

shead(aa)
head(bb)
head(gender)
head(df_gender)
head(bb)
head(device)

df_device <- read.csv("mw_data_기기별.csv")
df_gender <- read.csv("mw_data_성별.csv")
df_age <- read.csv("mw_data_연령대.csv")

head(df)

# 여기서 나름대로 합쳐보거나
# 기기별에서 각 파트별로 성별 정하기 
View(df_device)
df_device <- df_device %>%  mutate(년도 = substr(df_device$period, 1,4))
df_device <- df_device %>%  mutate(월 = substr(df_device$period, 6,7))
df_device %>% arrange(년도, 월)
head(df_age)
head(df_gender)
#--
str(df_device)

df_device 
tail(df_device)

#pc 와 m 사용량 -> mo인 계속해서 증가하고 있는데 pc는 변화량이 거의 없음을 볼 수 있다.
#내가 추가한 컬럼으로는 나누면 안되겠다. 상대적 비율이기 때문에 
df_device %>% filter(성별 == "여자" & 카테고리명 == "디지털_가전") %>% 
  ggplot(aes(x = period, y = ratio, fill = 월)) + 
  geom_bar(stat = "identity") + facet_grid(~group)

#전체
df_gender %>% 
  ggplot(aes(x = period, y= ratio))+ 
  geom_bar(stat = "identity") + facet_grid(~group)

#산업군별 확실히 성별 특징이 나타난다. 
df_gender %>% filter(카테고리명 == "가구_인테리어") %>%
  ggplot(aes(x = period, y= ratio))+ 
  geom_bar(stat = "identity") + facet_grid(~group)

#연령대별 주 잠재고객이 누군지 알 수 있음 -> 연령대를 두개씩 빼야겠다.
df_age
df_age %>% filter(검색기기 == "mobile", 카테고리명 == "가구_인테리어") %>%
  ggplot(aes(x = period, y= ratio))+ 
  geom_bar(stat = "identity") + facet_grid(~group)

unique(df_age$카테고리명)

#위 과정을 통해서 
df_age %>% group_by(period)
unique(df_age$카테고리명)

#----- merge_data 전처리 
merge_data <- read.csv("merge_data.csv")
head(merge_data)
unique(merge_data$상품군별)

merge_data <- merge_data %>% filter(상품군별 != "음식서비스")
merge_data <- merge_data %>% filter(상품군별 != "기타서비스")
merge_data <- merge_data %>% filter(상품군별 != "기타")
merge_data <- merge_data %>% select(-X)

#------------------------------------------------------------------
#1. 산업군별_성별 관심도 파악 
unique(df_gender$카테고리명)

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "가구_인테리어") #female 승
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "가구_인테리어") #pc에선 male 승 

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "디지털_가전") #female 승
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "디지털_가전") #pc에선 male 승 

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "생활_건강") #male 이 압도적
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "생활_건강") #male 이 압도적 

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "스포츠_레져") #male 승
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "스포츠_레져") #male 승

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "식품") # female 승 
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "식품") # male 승

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "여가_생활편의") # female 승 
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "여가_생활편의") # male 승

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "출산_육아") # female 압도적 승 
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "출산_육아") # female 승

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "패션잡화") # female 승 
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "패션잡화") # male 승

df_gender %>% filter(검색기기 == "mobile" & 카테고리명 == "화장품_미용") # female 압도적 승 
df_gender %>% filter(검색기기 == "pc" & 카테고리명 == "화장품_미용") # female 승

#---- 나온 결과로 해당 성별 골라내는 작업
df1 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "가구_인테리어") #female 승
df2 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "가구_인테리어") #pc에선 male 승 

df3 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "디지털_가전") #female 승
df4 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "디지털_가전") #pc에선 male 승 

df5 <- df_gender %>% filter(group == "m" & 검색기기 == "mobile" & 카테고리명 == "생활_건강") #male 이 압도적
df6 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "생활_건강") #male 이 압도적 

df7 <- df_gender %>% filter(group == "m" & 검색기기 == "mobile" & 카테고리명 == "스포츠_레져") #male 승
df8 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "스포츠_레져") #male 승

df9 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "식품") # female 승 
df10 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "식품") # male 승

df11 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "여가_생활편의") # female 승 
df12 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "여가_생활편의") # male 승

df13 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "출산_육아") # female 압도적 승 
df14 <- df_gender %>% filter(group == "f" & 검색기기 == "pc" & 카테고리명 == "출산_육아") # female 승

df15 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "패션잡화") # female 승 
df16 <- df_gender %>% filter(group == "m" & 검색기기 == "pc" & 카테고리명 == "패션잡화") # male 승

df17 <- df_gender %>% filter(group == "f" & 검색기기 == "mobile" & 카테고리명 == "화장품_미용") # female 압도적 승 
df18 <- df_gender %>% filter(group == "f" & 검색기기 == "pc" & 카테고리명 == "화장품_미용") # female 승

data <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18)
data <- data %>% arrange(period)

data <- data %>%  mutate(년도 = substr(data$period, 1,4))
data <- data %>%  mutate(월 = substr(data$period, 6,7))

data2018 <- data %>% filter(년도 == 2018)
data2019 <- data %>% filter(년도 == 2019)
#--> 이게 필요없겠는데..?
#아니네 2019에서 2018뺀 나머지수를 2018에 빼서 2017를 만들어야하네
#df_gender 에서 2017년만 추가하면 됌
df_gender

head(df_gender)
data <- df_device

df_gender <- df_gender %>%  mutate(년도 = substr(df_gender$period, 1,4))
df_gender <- df_gender %>%  mutate(월 = substr(df_gender$period, 6,7))

data2018 <- df_gender %>% filter(년도 == 2018)
data2019 <- df_gender %>% filter(년도 == 2019)

data2017 <- data2018 

data_ratio <- data2019$ratio - data2018$ratio

data2017$ratio <- data2018$ratio - data_ratio

head(data2017)
head(data2018)
head(data2019)

tail(data2017)
tail(data2018)
tail(data2019)

data2017$년도 <- "2017"
data2017$period <- gsub("2018", "2017", data2017$period)

df_gender <- rbind(data2017, df_gender)
df_gender <- df_gender %>% arrange(년도, 월)

head(df_gender)



#------------------------------------------------------
#merge_data와 data2017_2020 merge하기 
i <- 1

for( i in 1:nrow(merge_data)){
  if(merge_data[i,3] %in% c("컴퓨터 및 주변기기", "자동차 및 자동차용품","가전·전자·통신기기")){
    merge_data$중분류[i] <- "디지털_가전"
  } else if (merge_data[i,3] %in% c("의복", "신발","가방","패션용품 및 액세서리")){
    merge_data$중분류[i] <- "패션잡화"
  } else if (merge_data[i,3] == "스포츠·레저용품"){
    merge_data$중분류[i] <- "스포츠_레져"
  } else if (merge_data[i,3] == "화장품"){
    merge_data$중분류[i] <- "화장품_미용"
  } else if (merge_data[i,3] == "아동·유아용품"){
    merge_data$중분류[i] <- "출산_육아"
  } else if (merge_data[i,3] %in% c("음·식료품" ,"농축수산물")){
    merge_data$중분류[i] <- "식품"
  } else if (merge_data[i,3] == "가구"){
    merge_data$중분류[i] <- "가구_인테리어"
  } else if (merge_data[i,3] %in% c("생활용품" ,"애완용품","서적", "사무·문구")){
    merge_data$중분류[i] <- "생활_건강"
  } else if (merge_data[i,3] %in% c("여행 및 교통서비스" ,"문화 및 레저서비스","e쿠폰서비스")){
    merge_data$중분류[i] <- "여가_생활편의"
  }
}
table(merge_data$중분류)
View(merge_data %>% filter(중분류 == 0)
)#-------------------------------------------------------------------------------------
device
df_device <- rename(df_device, "기기별_검색추이" = "ratio")
#----------------------------------------------
View(merge_data)

unique(data2017_2020$카테고리명) 
unique(merge_data$중분류)

merge_data$중분류 <- as.factor(merge_data$중분류)
merge_data$년도 <- as.factor(merge_data$년도)
merge_data$월 <- as.factor(merge_data$월)

data2017_2020$년도 <- as.factor(data2017_2020$년도)
data2017_2020$월 <- as.factor(data2017_2020$월)



#-----------------------------------------------------------------------------
head(df_gender)

i <- 1

for (i in 1:nrow(df_gender)){
  if(df_gender[i,7] == "01"){
    df_gender$월[i] <- "1"
  }else if(df_gender[i,7] == "02"){
    df_gender$월[i] <- "2"
  }else if(df_gender[i,7] == "03"){
    df_gender$월[i] <- "3"
  }else if(df_gender[i,7] == "04"){
    df_gender$월[i] <- "4"
  }else if(df_gender[i,7] == "05"){
    df_gender$월[i] <- "5"
  }else if(df_gender[i,7] == "06"){
    df_gender$월[i] <- "6"
  }else if(df_gender[i,7] == "07"){
    df_gender$월[i] <- "7"
  }else if(df_gender[i,7] == "08"){
    df_gender$월[i] <- "8"
  }else if(df_gender[i,7] == "09"){
    df_gender$월[i] <- "9"
  }else if(df_gender[i,7] == "10"){
    df_gender$월[i] <- "10"
  }else if(df_gender[i,7] == "11"){
    df_gender$월[i] <- "11"
  }else if(df_gender[i,7] == "12"){
    df_gender$월[i] <- "12"
  }
}

head(df_gender)
str(df_gender)
head(device)
unique(df_gender$검색기기)
#---------------------------------------------------
# data2017_2020 은 성별 검색추이 
# merge_data == kosis 
merge(merge_data, data2017_2020, by.x = c("년도", "월", "중분류"), by.y = c("년도", "월", "카테고리명"), all.x = TRUE)
View(dd)#pc , mobile을 인터넷쇼핑이랑 모바일쇼핑으로 바꿔서...
head(merge_data)
merge_data[i, 4] == "인터넷쇼핑"

i <- 1
for( i in 1:nrow(merge_data)){
  if(merge_data[i, 4] == "인터넷쇼핑"){
    merge_data$판매매체별[i] <- "인터넷"
  }else if(merge_data[i, 4] == "모바일쇼핑"){
    merge_data$판매매체별[i] <- "모바일"
  }
}
#----------------------------------------------------------------------
i <- 1
data2017_2020$검색기기 <- 0

head(data2017_2020)

i %% 2 == 0
data2017_2020$검색기기[i] <- "인터넷"


for( i in 1:nrow(data2017_2020)){
  if(i %% 2 == 0){
    data2017_2020$검색기기[i] <- "인터넷"
  }else{
    data2017_2020$검색기기[i] <- "모바일"
  }
}
# 바꿔놓고 기기별 부터 붙이자 ....
#data2017_2020 --> 성별
gender <- data2017_2020
#merge_data --> kosis
#1. 우선 2017년 데이터 만들기 
unique(df_device$카테고리명)
head(df_device)

data <- df_device

data2017 <- data2018 

data2018 <- df_device %>% filter(년도 == 2018)
data2019 <- df_device %>% filter(년도 == 2019)

data_ratio <- data2019$기기별_검색추이 - data2018$기기별_검색추이
data2017$기기별_검색추이 <- data2018$기기별_검색추이 - data_ratio

head(data2017)
head(data2018)
head(data2019)

data2017$년도 <- "2017"

str_replace(data2017$년도, substr(data2017$period, 1, 4), 2017)
data2017$period <- gsub("2018", "2017", data2017$period)
data2017

device <- rbind(data2017, df_device)
device <- device %>% arrange(년도, 월)
table(device$년도)
table(gender$년도)

table(device$카테고리명)
table(gender$카테고리명)
head(gender)
head(device)
View(gender)
View(device)
##작업중!! 
#일단 age 제외하고 합쳐보기 
device$group <- "0"
i <- 1

for(i in 1: nrow(device)){
  if(i %% 2 == 0){
    device$group[i] <- "인터넷"
  }else{
    device$group[i] <- "모바일"
  }
}  

i <- 1
device$월 <- "0"

for (i in 1:nrow(device)){
  if(device[i,5] == "01"){
    device$월[i] <- "1"
  }else if(device[i,5]  == "02"){
    device$월[i] <- "2"
  }else if(device[i,5]  == "03"){
    device$월[i] <- "3"
  }else if(device[i,5]  == "04"){
    device$월[i] <- "4"
  }else if(device[i,5]  == "05"){
    device$월[i] <- "5"
  }else if(device[i,5]  == "06"){
    device$월[i] <- "6"
  }else if(device[i,5]  == "07"){
    device$월[i] <- "7"
  }else if(device[i,5]  == "08"){
    device$월[i] <- "8"
  }else if(device[i,5]  == "09"){
    device$월[i] <- "9"
  }else if(device[i,5]  == "10"){
    device$월[i] <- "10"
  }else if(device[i,5]  == "11"){
    device$월[i] <- "11"
  }else if(device[i,5]  == "12"){
    device$월[i] <- "12"
  }
}
#----
head(merge_data)
head(device)
str(device)
str(merge_data)
device$년도 <- as.factor(device$년도)
device$월 <- as.factor(device$월)
merge(merge_data, device, 
      by.x = c("년도", "월", "중분류"),
      by.y = c("년도", "월", "카테고리명"), all.x = TRUE)
#--- 우선 네이버끼리 묶기
#df_gender device(이건 완료)
head(device)
head(df_gender)
head(gender)
#----------------------
device %>% filter(카테고리명 == "가구_인테리어" & 년도 == "2017" & 월 == "01")
#아...device 관심도는 성별 상관없이 전체로 해야겠네
df_device <- device %>% group_by(년도, 월, group,카테고리명) %>% summarise(mean = mean(기기별_검색추이)) %>% arrange(년도, 월, 카테고리명)
head(device)

head(df_device)
head(gender)

df_device <- rename(df_device, "검색기기" = "group", "기기별_검색추이" = "mean")
gender <- rename(gender, "선호_성별_검색추이" = "ratio", "성별" = "group")

merge(df_device, gender, by.x = c("년도","월", "검색기기", "카테고리명"),
      by.y = c("년도","월", "검색기기", "카테고리명"), all.x = TRUE)

write.csv(df_device, "df_device.csv")
write.csv(gender, "df_gender.csv")
write.csv(merge_data, "merge_data.csv")
head(merge_data)
#--- age데이터 정리하고 새로운 창에서 하자 

head(df_age)
df_age %>% group_by(period, group, 성별, 카테고리명, 검색기기) 
#2017년도 부터
df_age <- df_age %>% mutate(년도 = substr(df_age$period, 1, 4))
df_age <- df_age %>% mutate(월 = substr(df_age$period, 6, 7))

head(df_age)

df_gender <- df_gender %>%  mutate(년도 = substr(df_gender$period, 1,4))
df_gender <- df_gender %>%  mutate(월 = substr(df_gender$period, 6,7))

df_age2018 <- df_age %>% filter(년도 == "2018")
df_age2019 <- df_age %>% filter(년도 == "2019")

df_age_ratio <- df_age2019$ratio - df_age2018$ratio 
df_age2017 <- df_age2018

df_age2017$ratio <- 0

df_age2017$ratio <- df_age2018$ratio - df_age_ratio

head(df_age2017)
head(df_age2018)
head(df_age2019)

df_age2017$년도 <- "2017"
df_age2017$period <- gsub("2018","2017", df_age2017$period)

age <- rbind(df_age2017, df_age)
#--
table(age$년도)
nrow(age)
nrow(device)
nrow(df_gender)

write.csv(device, "raw_device.csv")
write.csv(df_gender,"raw_geder.csv")
write.csv(age, "raw_age.csv")
#------------------------------여기서부터 !  
df1 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "가구_인테리어" & 성별 == "여자"  & group %in% c("30","40"))  #30,40
df2 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "가구_인테리어" & 성별 == "남자" & group %in% c("30","40")) #30,40

df3 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "디지털_가전" & 성별 == "여자" & group %in% c("30","20"))  # 30,20
df4 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "디지털_가전" & 성별 == "남자" & group %in% c("30","40")) # 30, 40 

df5 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "생활_건강" & 성별 == "남자" & group %in% c("30","40")) #30, 40
df6 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "생활_건강" & 성별 == "남자" & group %in% c("30","40")) # 30, 40 

df7 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "스포츠_레져" & 성별 == "남자" & group %in% c("30","40")) # 30,40
df8 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "스포츠_레져" & 성별 == "남자" & group %in% c("30","40")) # 30,40

df9 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "식품" & 성별 == "여자" & group %in% c("30","40")) # 30, 40
df10 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "식품" & 성별 == "남자" & group %in% c("30","40")) # 30, 40 

df11 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "여가_생활편의" & 성별 == "여자" & group %in% c("30","40")) # 30, 40
df12 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "여가_생활편의" & 성별 == "여자" & group %in% c("30","20")) # 30, 20 

df13 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "출산_육아" & 성별 == "여자" & group %in% c("30","40")) # 30, 40
df14 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "출산_육아" & 성별 == "여자" & group %in% c("30","40")) # 30, 40

df15 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "패션잡화" & 성별 == "여자" & group %in% c("30","20")) #  30, 20 
df16 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "패션잡화" & 성별 == "남자" & group %in% c("30","20")) # 30, 20

df17 <- age %>% filter(검색기기 == "mobile" &  카테고리명 == "화장품_미용" & 성별 == "여자" & group %in% c("30","20")) # 30, 20 
df18 <- age %>% filter(검색기기 == "pc" &  카테고리명 == "화장품_미용" & 성별 == "여자" & group %in% c("30","20")) # 30, 20

data <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18)
# gender 결과 참고해서 최대 잠재고객(성별) 중에서 연령대!
#---이거 비슷하게

#각 주제별 원데이터도 정리(EDA)

#헷갈리니까 새로운 창에서 시작하기 age 전처리 끝나면 
# 790개있는 gender, df_device




