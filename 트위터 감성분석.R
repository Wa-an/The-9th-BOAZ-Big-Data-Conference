---
  title: "twitter_textmining_2018"
author: "wankim"
date: "2019년 1월 15일"
output: html_document
---
  ```{r}
# Import package
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_191') 
library(KoNLP)
library(stringr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)
library(tidytext)
library(glue)
```

```{r}
# Import Data
saj18 <- read.csv("C:/Users/PC/Desktop/boaz/data/18_sulanju.csv",header=T)
swk18 <- read.csv("C:/Users/PC/Desktop/boaz/data/18_saewookkang.csv",header=T)
hs18 <- read.csv("C:/Users/PC/Desktop/boaz/data/18_honsul.csv",header=T)
saj17 <- read.csv("C:/Users/PC/Desktop/boaz/data/17_sulanju.csv",header=T)
swk17 <- read.csv("C:/Users/PC/Desktop/boaz/data/17_saewookkang.csv",header=T)
hs17 <- read.csv("C:/Users/PC/Desktop/boaz/data/17_honsul.csv",header=T)
```


```{r}
# Preprocess Data
make_easytext <- function(text, removetext) {
  text_prop <- text[,"message"]
  text_prop <- str_to_lower(text_prop) # 영어 소문자로
  text_prop <- gsub(removetext, " ", text_prop) # '혼술' 단어 제거
  text_prop <- gsub("[ㄱ-ㅣ]", "", text_prop) # ㅋㅋㅋ, ㅜㅜ 등 제거
  text_prop <- gsub("[[:punct:]]", "", text_prop) # 구두점 제거
  text_prop <- gsub("\\d+", "", text_prop) # 숫자제거
  text_prop <- str_trim(text_prop, side="right") # 공백제거
  text_prop <- str_trim(text_prop, side="left")
  text_prop
}
```

```{r}
# Preprocess Data
saj18_text <- make_easytext(saj18, "술안주")
saj17_text <- make_easytext(saj17, "술안주")
swk18_text <- make_easytext(swk18, "새우깡")
swk17_text <- make_easytext(swk17, "새우깡")
hs18_text <- make_easytext(hs18, "혼술")
hs17_text <- make_easytext(hs17, "혼술")
```

```{r}
# word freq
word_freq <- function(text, top=1000) {
  text_n <- extractNoun(text) # 명사 추출
  text_n <- table(unlist(text_n)) # 단어별 빈도표 생성
  text_n <- as.data.frame(text_n, stringAsFactors=F)
  ind_upper2 <- which(str_count(text_n$Var1)>=2)
  text_n <- text_n[ind_upper2,]
  #text_n <- filter(text_n, nchar(Var1)>=2) #두글자 이상의 단어만 추출
  text_n <- text_n%>%arrange(desc(Freq))%>%head(top) #top 1000개 단어
  text_n
}
```

```{r}
# Word Freq
saj18_n_c_f_1000 <- word_freq(saj18_text)
saj17_n_c_f_1000 <- word_freq(saj17_text)
swk18_n_c_f_1000 <- word_freq(swk18_text)
swk17_n_c_f_1000 <- word_freq(swk17_text)
hs18_n_c_f_1000 <- word_freq(hs18_text)
hs17_n_c_f_1000 <- word_freq(hs17_text)
```

```{r}
# Save data
write.csv(saj18_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/saj18.csv", row.names =F)
write.csv(saj17_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/saj17.csv", row.names =F)
write.csv(swk18_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/swk18.csv", row.names =F)
write.csv(swk17_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/swk17.csv", row.names =F)
write.csv(hs18_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/hs18.csv", row.names =F)
write.csv(hs17_n_c_f_1000, file ="C:/Users/PC/Desktop/boaz/top1000/hs17.csv", row.names =F)
```

##### 1000개의 단어 중 '음식'관련 단어만 추출 -> 6개 파일 합쳐서 food_m으로 저장

```{r}
# Import Food Data
food_total <- read.csv("C:/Users/PC/Desktop/boaz/top1000/food_m.csv",header=T)
```

```{r}
# Wordcloud -> 치즈/양파/오징어 多
pal <- brewer.pal(8,"Dark2")
windows()
wordcloud(words=food_total$Var1, freq=food_total$total, min.freq =20,random.order=F, color=pal,,scale = c(6,0.5),rot.per = 0.2)
savePlot("C:/Users/PC/Desktop/boaz/top1000/food_total.png",type="png")
```

### 감성분석

```{r}
# Import Food Data
onion <- read.csv("C:/Users/PC/Desktop/boaz/data/onion.csv",header=T)
squid <- read.csv("C:/Users/PC/Desktop/boaz/data/squid.csv",header=T)
cheese <- read.csv("C:/Users/PC/Desktop/boaz/data/cheese.csv",header=T)
dic2 <- read.csv("C:/Users/PC/Desktop/boaz/SentiWord_Dict.csv",header=T) # 감성사전
```

```{r}
# Preprocess Data
onion_text <- make_easytext(onion, "양파")
squid_text <- make_easytext(squid, "오징어")
cheese_text <- make_easytext(cheese, "치즈")
```

```{r}
# 각각 트윗에서 명사 추출
get.nouns <- function(text){ 
  text <- as.character(text) 
  pos <- paste(SimplePos09(text)) 
  nouns <- str_match(pos,'([가-힣]+)/N') 
  nouns[complete.cases(nouns), 2] 
} 
```

```{r}
# Sensitivity Analysis (onion)
for (i in 1:481){
  onion_1_n <- get.nouns(onion$message[i])
  onion_1_n_c <- c(onion_1_n)
  onion_1_n_df <- as.data.frame(onion_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% onion_1_n_df$Var1)
  ri_p <- mean(ri$point)
  print(ri_p)
}
# Sensitivity Analysis (squid)
for (i in 1:561){
  squid_1_n <- get.nouns(squid$message[i])
  squid_1_n_c <- c(squid_1_n)
  squid_1_n_df <- as.data.frame(squid_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% squid_1_n_df$squid_1_n)
  ri_p <- mean(ri$point)
  print(ri_p)
}
# Sensitivity Analysis (cheese)
for (i in 1:513){
  cheese_1_n <- get.nouns(cheese$message[i])
  cheese_1_n_c <- c(cheese_1_n)
  cheese_1_n_df <- as.data.frame(cheese_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% cheese_1_n_df$cheese_1_n)
  ri_p <- mean(ri$point)
  print(ri_p)
}
```

### 맛 + 새우깡 감성 분석

```{r}
# import Data
s_c <- read.csv("C:/Users/PC/Desktop/boaz/data/plus/s_c.csv",header=T)
s_o <- read.csv("C:/Users/PC/Desktop/boaz/data/plus/s_o.csv",header=T)
s_s <- read.csv("C:/Users/PC/Desktop/boaz/data/plus/s_s.csv",header=T)
dic2 <- read.csv("C:/Users/PC/Desktop/boaz/SentiWord_Dict.csv",header=T) # 감성사전
```

```{r}
# 감성점수 추출 텀 (새우깡 + 치즈)
for (i in 1:86){
  s_c_1_n <- get.nouns(s_c$message[i])
  s_c_1_n_c <- c(s_c_1_n)
  s_c_1_n_df <- as.data.frame(s_c_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% s_c_1_n_df$s_c_1_n)
  ri_p <- mean(ri$point)
  print(ri_p)
}
# 감성점수 추출 텀 (새우깡 + 양파)
for (i in 1:405){
  s_o_1_n <- get.nouns(s_o$message[i])
  s_o_1_n_c <- c(s_o_1_n)
  s_o_1_n_df <- as.data.frame(s_o_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% s_o_1_n_df$s_o_1_n)
  ri_p <- mean(ri$point)
  print(ri_p)
}
# 감성점수 추출 텀 (새우깡 + 오징어)
for (i in 1:260){
  s_s_1_n <- get.nouns(s_s$message[i])
  s_s_1_n_c <- c(s_s_1_n)
  s_s_1_n_df <- as.data.frame(s_s_1_n,stringsAsFactors = F)
  ri <- filter(dic2, dic2$text %in% s_s_1_n_df$s_s_1_n)
  ri_p <- mean(ri$point)
  print(ri_p)
}
```