### Create Base Variable Script
# rm(list=ls())
## setwd
setwd("C:/future/2학년 2학기 학회/LikeDataMining/PA_Challenge_Data")

## library
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(caret)) install.packages("caret"); library(caret)

## load data
# train_profiles
pro_train <- read_excel("train_profiles.xlsx")
pro_train$CUS_ID <- as.numeric(pro_train$CUS_ID)
pro_train <- pro_train[order(pro_train$CUS_ID),]

# train_clickstreams
click_train <- fread("train_clickstreams.tab")
click_train <- as.data.frame(click_train)

# test_profiles
pro_test <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)
pro_test$CUS_ID <- as.numeric(pro_test$CUS_ID)
pro_test <- pro_test[,1,drop = FALSE]

# test_clickstreams
click_test <- fread("test_clickstreams.tab")
click_test <- as.data.frame(click_test)

## Base Variable 생성 함수
BaseF <- function(profiles, click_stream) {
  ## Variable 01 - DWELLTIME, 총 체류시간
  tmp_01 <- click_stream %>% group_by(CUS_ID) %>% summarise(DWELLTIME = sum(ST_TIME))
  
  ## Variable 02 - PAGEVIEWS, 총 페이지뷰
  tmp_02 <- click_stream %>% group_by(CUS_ID) %>% summarise(PAGEVIEWS = sum(SITE_CNT))
  
  ## Variable 03 - VSITES, 접속한 서로 다른 사이트 수
  tmp_03 <- click_stream %>% group_by(CUS_ID) %>% summarise(VSITES = length(unique(SITE_NM)))
  
  ## Variable 04 - COVERAGE, 대 카테고리(BACT_NM)을 얼마나 다양하게 접속하였는가?
  tmp_04 <- click_stream %>% group_by(CUS_ID) %>% summarise(COVERAGE = length(unique(BACT_NM))/22)
  
  ## Variable 05 - SITECOV, 대 카테고리(BACT_NM)별 체류시간 변동계수
  tmp_df <- click_stream %>% group_by(CUS_ID,BACT_NM) %>% summarise(target = sum(ST_TIME))
  tmp_05 <- tmp_df %>% group_by(CUS_ID) %>% summarise(SITECOV = sd(target)/mean(target))
  
  ## Variable 06 - VDAYS, 총 접속 일 수
  click_stream$FORVDAYS <- substr(click_stream$TIME_ID,1,8)
  tmp_06 <- click_stream %>% group_by(CUS_ID) %>% summarise(VDAYS = length(unique(FORVDAYS)))
  
  ## 중간 통합
  profiles <- inner_join(profiles, tmp_01) %>% inner_join(tmp_02) %>% inner_join(tmp_03) %>%
    inner_join(tmp_04) %>% inner_join(tmp_05) %>% inner_join(tmp_06)
  print("half")
  
  ## Variable 07 - DAYCOV, 일별 변동계수
  tmp_df2 <- click_stream %>% group_by(CUS_ID,FORVDAYS) %>% summarise(target = sum(ST_TIME))
  tmp_07 <- tmp_df2 %>% group_by(CUS_ID) %>% summarise(DAYCOV = sd(target)/mean(target))
  # 위 과정 통합
  # tmp_07_a <- click_stream %>% group_by(CUS_ID,FORVDAYS) %>% summarise(target = sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(DAYCOV = sd(target)/mean(target))
  
  ## Variable 08 - 사이트 카테고리별 체류시간 비율
  tmp_df3 <- click_stream %>% group_by(CUS_ID,BACT_NM) %>% summarise(CT = sum(ST_TIME)) %>% inner_join(profiles[,c('CUS_ID','DWELLTIME')]) %>% mutate(FCT = CT/DWELLTIME)
  tmp_08 <- dcast(CUS_ID ~ BACT_NM, data = tmp_df3, value.var = "FCT") # wide form으로 변환
  tmp_08[is.na(tmp_08)] <- 0 # NA 제거
  
  ## Variable 09 - 요일당 체류시간 비율
  click_stream$DAYS <- factor(weekdays(as.Date(click_stream$FORVDAYS, format = "%Y%m%d")),levels = paste0(c("월","화","수","목","금","토","일"),"요일"))
  tmp_09 <- click_stream %>% group_by(CUS_ID,DAYS) %>% summarise(DF = sum(ST_TIME)) %>% inner_join(profiles[,c('CUS_ID','DWELLTIME')]) %>%
    mutate(FDF = DF/DWELLTIME) %>% dcast(CUS_ID ~ DAYS, value.var = "FDF") # Variable 08의 두 과정을 하나로 통합
  tmp_09[is.na(tmp_09)] <- 0 # NA 제거
  
  ## Variable 10 - 시간대별(5시간) 체류시간 비율
  click_stream$cut_time <- substr(click_stream$TIME_ID,9,10) %>% as.numeric %>% cut(c(-01,05,11,17,23), labels = c("0005","0611","1217","1823"))
  tmp_10 <- click_stream %>% group_by(CUS_ID,cut_time) %>% summarise(HF = sum(ST_TIME)) %>% inner_join(profiles[,c('CUS_ID','DWELLTIME')]) %>%
    mutate(FHF = HF/DWELLTIME) %>% dcast(CUS_ID ~ cut_time, value.var = "FHF")
  tmp_10[is.na(tmp_10)] <- 0 # NA 제거
  
  ## 통합
  profiles <- inner_join(profiles, tmp_07) %>% inner_join(tmp_08) %>% inner_join(tmp_09) %>% inner_join(tmp_10)
  return(profiles)
}

# train, test에 각각 적용
pro_train_01 <- BaseF(pro_train, click_train)
pro_test_01 <- BaseF(pro_test, click_test)

# NA 0으로 대체
pro_train_01[is.na(pro_train_01)] <- 0
pro_test_01[is.na(pro_test_01)] <- 0

# csv 저장
path <- "C:/future/포트폴리오/프로젝트/[D&A] Clickstream data/data/"
write.csv(pro_train_01, paste0(path,"pro_train_01.csv"), row.names = FALSE)
write.csv(pro_test_01, paste0(path,"pro_test_01.csv"), row.names = FALSE)

## Base Modeling




### end