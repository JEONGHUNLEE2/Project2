#네이버 데이터랩 시각화 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

save.image("Naver_EDA_시각화.RData")


rm(list = ls())



#----------------------------------------------네이버_데이터랩
#----------------------------------------------기기별_검색빈도 
head(merge_data)
unique(merge_data$중분류)


merge_data %>% filter(중분류 == "가구_인테리어") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "디지털_가전") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "생활_건강") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "스포츠_레져") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "식품") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

merge_data %>% filter(중분류 == "여가_생활편의") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

unique(merge_data$중분류)

merge_data %>% filter(중분류 == "출산_육아") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

head(device)
merge_data %>% filter(중분류 == "화장품_미용") %>% 
  group_by(년도_분기 = paste(년도, 분기), 판매매체별) %>% 
  summarise(검색빈도 = mean(기기별_검색추이)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = 판매매체별)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")



#-----------------------------------------성별_검색빈도
head(raw_gender)
unique(raw_gender$카테고리명)

raw_gender %>% 
  filter(년도 == "2020") %>% 
  group_by(월,group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 월, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
#------------
raw_gender %>%
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
#----------------
raw_gender %>% filter(카테고리명 == "가구_인테리어" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "디지털_가전" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "생활_건강" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

unique(raw_gender$카테고리명)

raw_gender %>% filter(카테고리명 == "스포츠_레져" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "식품" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "출산_육아" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "패션잡화" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "화장품_미용" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "여가_생활편의" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

##-------------------------------------------------------------pc

raw_gender %>% filter(카테고리명 == "가구_인테리어" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "디지털_가전" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "생활_건강" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

unique(raw_gender$카테고리명)

raw_gender %>% filter(카테고리명 == "스포츠_레져" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "식품" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "출산_육아" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_gender %>% filter(카테고리명 == "패션잡화" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "화장품_미용" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_gender %>% filter(카테고리명 == "여가_생활편의" &
                             검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

#----------------------------------------연령대_검색빈도_Mobile&pc

raw_age %>%  
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")



raw_age %>% filter(카테고리명 == "가구_인테리어" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "디지털_가전" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "생활_건강" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "스포츠_레져" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "식품" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "출산_육아" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "패션잡화" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "화장품_미용" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "여가_생활편의" &
                             검색기기 == "mobile") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")
#------------------------------------------------------------------pc
raw_age %>% filter(카테고리명 == "가구_인테리어" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "디지털_가전" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "생활_건강" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "스포츠_레져" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "식품" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "출산_육아" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")


raw_age %>% filter(카테고리명 == "패션잡화" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "화장품_미용" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

raw_age %>% filter(카테고리명 == "여가_생활편의" &
                          검색기기 == "pc") %>% 
  group_by(년도_분기 = paste(년도, 분기), group) %>% 
  summarise(검색빈도 = mean(ratio)) %>% 
  mutate(분기별_검색빈도 = sum(검색빈도)) %>% 
  mutate(분기별_검색빈도비율 = round(검색빈도 / 분기별_검색빈도,3) * 100) %>% 
  ggplot(aes(x = 년도_분기, y = 검색빈도, fill = group)) +
  geom_bar(stat = "identity")+
  geom_text(stat = "sum",aes(label = paste0(분기별_검색빈도비율,"%")),
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Pastel1")

#-----------------merge_Data 컬럼 추가해야겠네... 전월대비 _증가,감소 컬럼
#------------------------------------------------------------------------
