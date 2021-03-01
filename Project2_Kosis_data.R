#2021_03_01
#EDA ~ 모델링 
library(tidyverse)
library(dplyr)
library(xlsx)
library(lubridate)
library(kimisc)
library(readxl)
library(data.table)
library(PerformanceAnalytics)

save.image("project2_Kosis_data.RData")

merge_data <- read.csv("merge_data.csv")
merge_data <- merge_data %>% select(-X)

train_data <- merge_data %>% filter(년도 %in% c("2017","2018","2019"))
train_data2 <- merge_data %>% filter(년도 == "2020" & 월 %in% c("1","2","3","4","5","6","7","8","9","10"))
train_data <- rbind(train_data, train_data2)
write(train_data, "train_data.csv")

test_data <- merge_data %>% filter(년도 == "2020" & 월 %in% c("11","12"))
write.csv(test_data, "test_data.csv")

unique(merge_data$년도)
unique(merge_data$월)

vars <- c('년도','월')
merge_data[vars] <- map_df(.x=merge_data[vars],.f = as.factor)
str(merge_data)
#--------------------------------------------------------------------------
head(train_data)
tail(train_data)
str(train_data)
summary(train_data)
colnames(train_data)

cor_data <- train_data[,c(5,7:15)]
str(cor_data)

cor(cor_data, use = "na.or.complete")
chart.Correlation(cor_data, histogram=TRUE, pch=19)

cor <- map_lgl(.x = cor_data, .f = function(x){
  test<- cor.test(x = x, y = train_data$매출액)
  result <- test$p.value > 0.05
  return(result)
})
cor
# total_case, 경기심리지수_증감률_전월대비, 소비자지수_증감률_전월대비 상관없음!




