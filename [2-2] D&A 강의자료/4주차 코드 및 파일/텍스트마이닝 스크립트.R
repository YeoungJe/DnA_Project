
########Text mining########

## Install & load package
if(!require(rvest)) install.packages("rvest"); library(rvest)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(XML)) install.packages("XML"); library(XML)
if(!require(KoNLP)) install.packages("KoNLP"); library(KoNLP)
if(!require(wordcloud)) install.packages("wordcloud"); library(wordcloud)
if(!require(RColorBrewer)) install.packages("RColorBrewer"); library(RColorBrewer)

#### Simple exercise
### Web crawling movie name from movie site(ex.lalaland)
## 영화 제목 가져오기
url <- "http://movie.daum.net/moviedb/main?movieId=95306" #크롤링 할 페이지.
url_movie <- read_html(url) #r에서 인식할 수 있게 바꾸어 줌
movie_info <- html_nodes(url_movie, css='.tit_movie') #class 에서 이걸 가져옴
movie_info 
str(movie_info)
movie_info[1] %>% html_text() # 라라랜드만 가져오기 위해서 이걸 사용. 텍스트 값만 불러 온다라는 뜻.

### Web crawling movie score from movie site(ex.lalaland)
url <- "http://movie.daum.net/moviedb/grade?movieId=95306"
url_movie <- read_html(url)
movie_score <- html_nodes(url_movie, css='.score_rating') #평점의 class
movie_score #네티즌과 기자 두개가 뜬다.

movie_score[1] %>% html_text() #네티즌 것만 보고 싶어서

### Web crawling movie review from movie site(ex.lalaland)
url <- "http://movie.daum.net/moviedb/grade?movieId=95306&type=netizen&page=1"
url_movie <- read_html(url)
movie_review <- html_nodes(url_movie, css='.desc_review') #평점의 class 가 이것이다.
movie_review

movie_review[1] %>% html_text()

# \t : tab, \n : Enter

## Refine review data /t 와/n 을 삭제하는 과정
review1 <- movie_review[1] %>% html_text()
review1
review1 <- gsub("\n","", review1) #앞에있는것을 뒤에있는것으로 바꾼다
review1 <- gsub("\t","", review1)  
review1

## Refine all review data
review <- movie_review %>% html_text()
review
review <- gsub("\n","", review)
review <- gsub("\t","", review)
review <- gsub("\r","", review) # \r : Cursor to the top
review

### Web crawling movie next page review from movie movie site(ex.lalaland)
url <- "http://movie.daum.net/moviedb/grade?movieId=95306&type=netizen&page=2"
url_movie <- read_html(url)
movie_review <- html_nodes(url_movie, css='.desc_review')
movie_review

## Refine all review data
review <- movie_review %>% html_text()
review
review <- gsub("\n","", review)
review <- gsub("\t","", review)
review <- gsub("\r","", review) # \r : Cursor to the top
review <- str_trim(review) # Remove first & end space
review

### Web crawling movie 1~50page review from review page review(ex.lalaland)
All_review=c() # setting blank vector
for (i in 1:50){
  url <- paste0("http://movie.daum.net/moviedb/grade?movieId=95306&type=netizen&page=",i)
  url_movie <- read_html(url)
  movie_review <- html_nodes(url_movie, css='.desc_review')
  review <- movie_review %>% html_text()
  review <- gsub("\n","", review)
  review <- gsub("\t","", review)
  review <- gsub("\r","", review)
  review <- str_trim(review)
  All_review = c(All_review, review)
}

All_review
write.table(All_review, "All_review(lalaland).txt")

### Web crawling movie 1~50page review from review page review(ex.mammamia)
All_review2=c() # setting blank vector
for (i in 1:50){
  url <- paste0("http://movie.daum.net/moviedb/grade?movieId=43220&type=netizen&page=",i)
  url_movie <- read_html(url)
  movie_review <- html_nodes(url_movie, css='.desc_review')
  review <- movie_review %>% html_text()
  review <- gsub("\n","", review)
  review <- gsub("\t","", review)
  review <- gsub("\r","", review)
  review <- str_trim(review)
  All_review2 = c(All_review2, review)
}

All_review2
write.table(All_review2, "All_review(mammamia).txt")

#### text analysis
useSejongDic() #load & use Sejong dictionary #단어에 품사를 붙혀주는 단계

### extract Nouns
nouns = unlist(sapply(All_review, extractNoun, USE.NAMES=F)) #명사를 추출하는 함수USE.NAMES=F ?
nouns2 = unlist(sapply(All_review2, extractNoun, USE.NAMES=F)) #이거도
head(nouns)
head(nouns2,20)

### Remove one characters
nouns = Filter(function(x){nchar(x)>=2 & 5>=nchar(x)},nouns) #2~5글자 사이의 명사들만 추출
nouns2 = Filter(function(x){nchar(x)>=2 & 5>=nchar(x)},nouns2)
head(nouns)

### count nouns in data
word_count_lalaland <- table(nouns);word_count_lalaland #단어가 몇번 떴는지 카운트 해주는거
word_count_mammamia <- table(nouns2);word_count_mammamia #이거도 마찬가지
word_count_lalaland <- sort(word_count_lalaland, decreasing=T)
word_count_mammamia <- sort(word_count_mammamia, decreasing=T)
head(word_count_lalaland,10)
head(word_count_mammamia,10)



### Word cloud
palette <- brewer.pal(8, "Set2")
wordcloud(names(word_count_lalaland), freq=word_count_lalaland, scale=c(10,4), 
          rot.per=0.25, mirrandom.order=F, colors=palette, random.color=T)


