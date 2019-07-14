### EDA & New Variables Script
# rm(list=ls())
## setwd
setwd("C:/future/2학년 2학기 학회/LikeDataMining/PA_Challenge_Data")

## library
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(stringr)) install.packages("stringr"); library(stringr)

## load data
path <- "C:/future/포트폴리오/프로젝트/[D&A] Clickstream data/data/"
# train_profiles
pro_train_01 <- read.csv(paste0(path,"pro_train_01.csv"), stringsAsFactors = FALSE)

# train_clickstreams
click_train <- fread("train_clickstreams.tab")
click_train <- as.data.frame(click_train)

# test_profiles
pro_test_01 <- read.csv(paste0(path,"pro_test_01.csv"), stringsAsFactors = FALSE)

# test_clickstreams
click_test <- fread("test_clickstreams.tab")
click_test <- as.data.frame(click_test)

## function
# cleansing : GROUP의 +,- 제거
cleansing <- function(text) {
  output <- str_remove_all(text, pattern = "[^a-zA-Z0-9]")
  return(output)
}

# min_max scale
min_max_scale <- function(x) {
  return((x - min(x))/ (max(x) - min(x)))
}

# MAE
mae <- function(x,y) {
  return(abs(x-y) %>% sum)
}

# select_f : 특정 변수의 GROUP별 summary 결과를 확인
select_f <- function(x, all = FALSE) {
  summary_table <- matrix(0,nrow = 6,ncol = 6)
  rownames(summary_table) <- unique(pro_train_01$GROUP) %>% sort
  colnames(summary_table) <- summary(1:10) %>% names
  
  idx <- 1
  for (text in sort(unique(pro_train_01$GROUP))) {
    summary_table[idx,] <- get(cleansing(text))[,x] %>% summary
    idx <- idx + 1
    summary_table <- as.data.frame(summary_table)
    } # end of for
  summary_table$Minmax_Mean <- min_max_scale(summary_table$Mean)
  print(paste0("Variable :", x))
  if (all) {
    return(summary_table)
  }
  if (!all) {
    return(summary_table[,c("Mean","Minmax_Mean"),drop = FALSE])
  }
}

# check_MACT_NM : MACT_NM(중 카테고리) 별 GROUP의 분포와 Train data의 GROUP의 분포의 차이를 MAE로 계산
#                 이를 통해 특정 Label에 치우친 MACT_NM을 판별할 수 있음
check_MACT_NM <- function(data, answer_distribution) {
  output <- c()
  idx <- 1
  for (mact_nm in unique(data$MACT_NM)) {
    tmp_data <- click_train[click_train$MACT_NM == mact_nm,c("CUS_ID","GROUP")]
    tmp_dis <- tmp_data$GROUP %>% table %>% prop.table
    output_len[idx] <<- (tmp_data %>% dim)[1]
    names(output_len)[idx] <<- mact_nm
    # 만약 6개의 label이 없는 MACT_NM이면 0으로 채우는 과정
    if (length(tmp_dis) < 6) {
      tmp_dis <- answer_distribution
      tmp_dis[!names(tmp_dis) %in% names(tmp_dis_0)] <- 0
      tmp_dis[names(tmp_dis) %in% names(tmp_dis_0)] <- tmp_010$GROUP %>% table %>% prop.table
      output[idx] <- mae(answer_distribution, tmp_dis)
    } else {
      output[idx] <- mae(answer_distribution, tmp_dis)
      names(output)[idx] <- mact_nm
    }
    idx <- idx + 1
    # 50 단위로 PRINT
    if (idx %% 50 == 0) {
      print(idx)
    }
  }
  return(output)
}

# mact_nm_checkF : MACT_NM 별 행의 개수와 GROUP의 분포를 확인
mact_nm_checkF <- function(x) {
  tmp_data <- click_train[click_train$MACT_NM == x,c("CUS_ID","GROUP")]
  tmp_dis <- tmp_data$GROUP %>% table %>% prop.table
  cat(x); cat(" 행 개수 "); cat(output_len[which(names(output_len) == x)])
  print(tmp_dis)
}


## EDA 01 : 학습 데이터의 LABEL의 분포를 확인한다.
pro_train_01$GROUP %>% table %>% prop.table()

#  F20-    F30   F40+   M20-    M30   M40+  #
# 0.0964 0.1396 0.1296 0.0932 0.2440 0.2972 #
# -> M30,M40의 비율이 높음을 알 수 있다.

# LABEL별 변수 할당
for (text in unique(pro_train_01$GROUP)) {
  assign(cleansing(text), pro_train_01[pro_train_01$GROUP == text,])
}

## EDA 02 : 갖고 있는 변수 중 LABEL 별 변수의 차이를 확인해보자
# GROUP별로 변수의 평균값의 표준편차가 큰 변수 선정하는 과정.
# 표준편차가 크다는 것은 GROUP간의 차이가 큰 변수들임을 뜻한다.
output <- c()
for (i in 3:ncol(pro_train_01)) {
  summary_table <- matrix(0,nrow = 6,ncol = 6)
  rownames(summary_table) <- unique(pro_train_01$GROUP) %>% sort
  colnames(summary_table) <- summary(1:10) %>% names
  
  idx <- 1
  for (text in sort(unique(pro_train_01$GROUP))) {
    summary_table[idx,] <- get(cleansing(text))[,i] %>% summary
    idx <- idx + 1
  }
  summary_table <- as.data.frame(summary_table)
  output[i] <- min_max_scale(summary_table$Mean) %>% sd
}

# 각 변수별 평균의 표준편차를 SUMMARY한 결과는 다음과 같다.
# summary(output)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
# 0.3218  0.3612  0.3828  0.3796  0.3971  0.4314

# 0.40 이상의 표준편차를 갖는 변수를 확인해보자.
over_40 <- colnames(pro_train_01)[which(output > 0.40)]

# [1] "PAGEVIEWS"      "SITECOV"        "게임"           "금융.부동산"    "사회.문화.종교"
# [6] "유통.판매.운송" "일요일"         "X1823"    

select_f(over_40[1], all = FALSE)
# [1] "Variable :PAGEVIEWS"
#         Mean   Minmax_Mean
# F20- 21336.22   0.8987003
# F30  22028.32   1.0000000
# F40+ 15196.12   0.0000000
# M20- 16097.98   0.1320007
# M30  18845.14   0.5340914
# M40+ 17905.03   0.3964904
# -> 20대 30대 여성일 수록 다른 연령대에 비해 다양하게 페이지를 접속한다는 것을 알 수 있다.

select_f(over_40[3])
# [1] "Variable :게임"
#          Mean     Minmax_Mean
# F20- 0.009108863  0.00000000
# F30  0.010590307  0.08108185
# F40+ 0.014599905  0.30053378
# M20- 0.027379825  1.00000000
# M30  0.025361670  0.88954302
# M40+ 0.013053073  0.21587311
# -> 20대 30대 남성일 수록 다른 연령대에 비해서 게임에 관심이 높은 것을 볼 수 있다.

select_f(over_40[4])
# [1] "Variable :금융.부동산"
#        Mean      Minmax_Mean
# F20- 0.02152363   0.0000000
# F30  0.03591739   0.5821314
# F40+ 0.04623978   0.9996029
# M20- 0.02690390   0.2175960
# M30  0.03983658   0.7406363
# M40+ 0.04624960   1.0000000
# -> 금융 부동산은 40대 남성과 40대 여성이 가장 높고 30대 남성도 높은편이다.

select_f(over_40[5])
# [1] "Variable :사회.문화.종교"
#          Mean     Minmax_Mean
# F20- 0.001077368  0.00000000
# F30  0.001195029  0.04273949
# F40+ 0.003830362  1.00000000
# M20- 0.001117082  0.01442573
# M30  0.001346197  0.09764965
# M40+ 0.002873009  0.65225028
# -> 사회.문화.종교는 40대 여성이 가장 관심이 많다

select_f(over_40[6])
# [1] "Variable :유통.판매.운송"
#          Mean    Minmax_Mean
# F20- 0.003499770 0.755616485
# F30  0.003952438 1.000000000
# F40+ 0.002100150 0.000000000
# M20- 0.002293376 0.104317331
# M30  0.003034829 0.504607766
# M40+ 0.002105929 0.003119736
# -> 유통.판매.운송은 30대 여성이 가장 관심이 많다.

select_f(over_40[7])
# [1] "Variable :일요일"
#         Mean    Minmax_Mean
# F20- 0.05962089   0.0000000
# F30  0.06401865   0.1880647
# F40+ 0.06419326   0.1955315
# M20- 0.08300521   1.0000000
# M30  0.07331810   0.5857434
# M40+ 0.08261900   0.9834844
# -> 일요일 접속시간이 가장 많은 연령대는 20대 남성과 40대 남성이다.

select_f(over_40[8])
# [1] "Variable :X1823"
#         Mean    Minmax_Mean
# F20- 0.2571983  0.47208481
# F30  0.2411725  0.02589785
# F40+ 0.2402423  0.00000000
# M20- 0.2761595  1.00000000
# M30  0.2680917  0.77537827
# M40+ 0.2630971  0.63631932
# -> 18~23시 접속시간이 가장 많은 연령대는 20대 남성이고 주로 남성이용시간이 여성 이용시간보다 높다.

# plot 예시
ex_summ <- select_f(over_40[8])
ex_summ$GROUP <- row.names(ex_summ)
ex_summ_m <- melt(ex_summ,id.vars = "GROUP")
ggplot(ex_summ_m, aes(x=GROUP, y=value, group = variable)) + geom_line(aes(colour = variable))

## EDA를 통해 연령, 성별로 접속하는 요일, 시간대와 들어가는 홈페이지의 특성에 차이가 있음을 확인하였다.
## 확인에 사용된 변수들은 BACT_NM(대 카테고리)를 통해 만들어졌다. 
## 조금 더 정밀한 부분인 MACT_NM(중 카테고리), ACT_NM(소 카테고리)를 분석하여 변수를 생성한다면 성별, 연령 구분에 도움이 될 것이라 판단했다.

## EDA 03 : 위에서 얻은 결과를 바탕으로 raw데이터에 접근하는 과정
# data merging
click_train <- inner_join(click_train, pro_train_01[,c("CUS_ID","GROUP")])

# 전체 MACT_NM을 돌면서 기존의 GROUP LABEL 분포와 차이가 많이 나는 MACT_NM(중 카테고리)를 선정한다.
# EX) 전체 라벨의 분포
whole_dis <- click_train$GROUP %>% table %>% prop.table; whole_dis
#       F20-        F30       F40+       M20-        M30       M40+ 
#   0.10797776 0.15204380 0.10755326 0.08241293 0.24909707 0.30091519 

# 어린이 커뮤니티의 분포
kids <- click_train[click_train$MACT_NM == "어린이커뮤니티",c("CUS_ID","GROUP")]
kids_dis <- kids$GROUP %>% table %>% prop.table; kids_dis
#       F20-        F30       F40+       M20-        M30       M40+ 
#   0.01924953 0.24257007 0.08298108 0.01203096 0.23886324 0.40430513

# 두 분포의 MAE = 0.3878324
mae(whole_dis, kids_dis)

# 전체 MACT_NM의 MAE를 확인하여 보자
click_train$MACT_NM %>% unique %>% length
output_len <- c() # 844만 행 중 몇 행이 있는가 확인

# MACT_NM 별 MAE
mae_mact_nm <- check_MACT_NM(click_train, whole_dis)

# 상위 3개를 살펴보자
mae_mact_nm %>% sort %>% tail(3)
# 생활/취미기타     종이/펄프          레저 
#   1.050678        1.117433        1.238777 

# 생활/취미기타의 행개수와 GROUP 분포 확인
mact_nm_checkF("생활/취미기타")
# 생활/취미기타 행 개수 133.
#      F20-         F30        F40+        M20-         M30        M40+ 
#   0.007518797 0.030075188 0.052631579 0.060150376 0.774436090 0.075187970 
# -> 30대 남성의 비율이 높음을 알 수 있다.

# 종이/펄프의 행개수와 GROUP 분포 확인
mact_nm_checkF("종이/펄프")
# 종이/펄프 행 개수 1013.
#      F20-        F30       F40+       M20-        M30       M40+ 
#   0.03356367 0.71076012 0.07008885 0.02369200 0.07601185 0.08588351 
# -> 30대 여성의 비율이 높음을 알 수 있다.

# 레저의 행개수와 GROUP 분포 확인
mact_nm_checkF("레저")
# 레저 행 개수 1054.
#       F20-         F30        F40+        M20-         M30        M40+ 
#   0.004743833 0.039848197 0.005692600 0.017077799 0.012333966 0.920303605 
# -> 40대 남성의 비율이 압도적이다.




## end