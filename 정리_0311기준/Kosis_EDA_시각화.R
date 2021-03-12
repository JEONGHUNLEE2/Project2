#EDA
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)


rm(list = ls())

save.image("Kosis_EDA_시각화.RData")

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

#--------------------------------판매매체별(pc,mo) 그래프
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
  

#-----------------------------------------온라인, 온오프라인 운영형태별 그래프
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


#-----------------------------------------종합몰, 전문몰 
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

#매출액 추세와 소비자심리지수 추세 _ 단위 백만원
#왼쪽에 매출액 스케일 , 오른쪽에 심리지수 스케일 하는방법----------------------------------------질문문
head(merge_data)
str(merge_data)
merge_data <- merge_data %>% arrange(년도, 월)

merge_data %>%  group_by(년도_분기 = paste(년도, 분기)) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, group = 1))+
  geom_line(color = "red", size = 1.5)+
  scale_y_continuous(label = scales::comma)
  
merge_data %>% group_by(년도_분기 = paste(년도, 분기)) %>% 
  summarise(소비자_심리지수 = mean(소비자심리지수)) %>% 
  ggplot(aes(x = 년도_분기, y = 소비자_심리지수, group = 1)) +
  geom_line(color = "blue", size = 1.5)


merge_data %>% group_by(년도_분기 = paste(년도, 분기)) %>% 
  summarise(경기심리지수 = mean(경기심리지수)) %>% 
  ggplot(aes(x = 년도_분기, y = 경기심리지수, group = 1)) +
  geom_line(color = "darkgreen", size = 1.5)

#--------------------------------------------------------
#------------------2020년 온라인 매출액과 코로나 관계 파악 
merge_data %>% filter(년도 == "2020") %>% 
  group_by(월) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  ggplot(aes(x = 월, y = 분기_매출액, group = 1))+
  geom_line(color = "red", size = 1.5)+
  scale_y_continuous(label = scales::comma)

merge_data %>% filter(년도 == "2020") %>% 
  group_by(월) %>% 
  summarise(월별_확진자수 = sum(확진자수)) %>% 
  ggplot(aes(x = 월, y = 월별_확진자수, group = 1))+
  geom_line(color = "blue", size = 1.5)+
  scale_y_continuous(label = scales::comma)
#
merge_data %>% filter(년도 == "2020") %>% 
  group_by(월) %>% 
  summarise(월별_매출액 = sum(매출액), 월별_확진자수 = sum(확진자수)) %>% 
  ggplot(aes(x = 월별_확진자수, y = 월별_매출액)) +
  geom_point(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  scale_x_continuous(label = scales::comma)

cor(merge_data$매출액, merge_data$확진자수)
head(merge_data)
merge_data %>% filter(중분류 == "여가_생활편의")

#여가 생활편의는 모델 돌릴때는 제거해야겠다...네이버 검색추이가 없음 
unique(merge_data$중분류)
head(merge_data)

#-------------------------------------------------------------중분류별 mo, pc 검색추이 
merge_data %>% filter(중분류 == "가구_인테리어") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "디지털_가전") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "생활_건강") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "스포츠_레져") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "식품") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "여가_생활편의") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "출산_육아") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "패션잡화") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "화장품_미용") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(분기_매출액 = sum(매출액)) %>% 
  mutate(분기별_전체매출액 = sum(분기_매출액)) %>%
  mutate(분기별_매출액_비율 = round(분기_매출액/분기별_전체매출액,3)*100) %>% 
  ggplot(aes(x = 년도_분기, y = 분기_매출액, fill = 판매매체별))+
  geom_bar(stat = "identity")+
  scale_y_continuous(label = scales::comma)+
  geom_text(stat = "sum",aes(label = paste0(분기별_매출액_비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
#--------------------------
View(data)
head(kosis_data)
View(kosis_data)

data <- kosis_data %>% group_by(년도,월, 판매매체별, 상품군별) %>% summarise(매출총액 = sum(매출액)) %>% arrange(년도, 월, 상품군별)#----------여기서부터 다시 

head(data)
View(data)

table(kosis_data$중분류)

data$전월_대비매출현황 <- shift(data$매출총액, fill = data$매출총액[1])
data$전월_대비매출현황 <- shift(data$전월_대비매출현황, fill = data$전월_대비매출현황[1])

for(i in 1:25){
  data$전월_대비매출현황 <- shift(data$전월_대비매출현황, fill = data$전월_대비매출현황[1])
}

dd201702 <- data %>% filter(년도 == "2017" & 월 == "2")
dd201701 <- data %>% filter(년도 == "2017" & 월 == "1")

dd0102 <- dd201702$매출총액 - dd201702$전월_대비매출현황 

dd201701$전월_대비매출현황 <- dd201701$매출총액 - dd0102

head(dd201701)
head(dd201702)

data <- data %>% filter(년도 != "2017" | 월 != "1")

data <- rbind(dd201701, data)

head(data)
View(data)

data$전월대비_증감 <- 0

i <- 1

for(i in 1:nrow(data)){
  if(data$매출총액[i] - data$전월_대비매출현황[i] < 0 ){
    data$전월대비_증감[i] <- "감소"
  }else{
    data$전월대비_증감[i] <- "증가"
  }
}

View(data)
head(merge_data)
head(data)
table(data$전월대비_증감)
str(merge_data)
str(data)

data <- as.data.frame(data)

dd <- merge(merge_data, data, 
            by.x = c("년도", "월","판매매체별","상품군별"),
            by.y = c("년도", "월","판매매체별","상품군별"), all.x = TRUE)

View(dd)
View(dd %>% arrange(년도, 월, 중분류))

dd <- dd %>% select(-매출총액)
head(dd)
colnames(dd)

dd <- dd[,c(1,2,22,3,5,4,6,23,24,7:21)] %>% arrange(년도, 월, 중분류)
dd <- dd %>% select(-경기심리지수_변동치_2017기준, -소비자지수_변동치_2017기준)
dd <- dd %>% select(-period)

merge_data <- dd
kosis_data <- dd %>% select(c(1:15))

head(dd)
colnames(dd)
head(naver_data)
write.csv(kosis_data, "kosis_data.csv")
write.csv(merge_data, "kosis_naver_merge_data.csv")
#------------------------------------------------------------------------------전처리 완전끝끝

