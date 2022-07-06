##textmining
install.packages("corpus")
install.packages("qdap")
install.packages("tm")
install.packages("tidyverse")
install.packages("wordcloud2")
install.packages("topicmodels")
install.packages("ldatuning")
install.packages("dplyr")
install.packages("tidytext")
install.packages("stringr")
install.packages("plyr")
install.packages("rJava")
library(tidyverse)
library(ggplot2)
library(tm)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)

start_time <- Sys.time()

### wp
wp_txt <- readLines("D:/대학원/선형모형론/2학기/term/wp.txt")
wp_txt <- gsub("support@wsj.com","",wp_txt)
wp_txt <- gsub("['’]s\\b|[^[:alnum:][:blank:]@_]","",wp_txt) #따옴표 제거 정규식

wp_int <- c()

for (i in 2:length(wp_txt)){
  wp_int <- paste0(wp_int,wp_txt[i],"."," ")
}

wp_docs <- Corpus(VectorSource(wp_int))

wp_docs <- tm_map(wp_docs, content_transformer(tolower))
wp_docs <- tm_map(wp_docs, removeNumbers)
wp_docs <- tm_map(wp_docs, removePunctuation)
wp_docs <- tm_map(wp_docs, removeWords, stopwords("english"))
wp_docs <- tm_map(wp_docs, removeWords, c("the", "and","'s","biden","joe","can","cant","will","that","weve","dint","wont","youll","youre","wp","wsj","says","new"))
wp_docs <- tm_map(wp_docs, stripWhitespace)

wp_dtm <- DocumentTermMatrix(wp_docs)
wp_dtm %>% dim()

wp_freq <- colSums(as.matrix(wp_dtm))

wp_freq_order <- sort(wp_freq, decreasing = TRUE)
wp_freq_order20 <- wp_freq_order[1:20]
wp_freq_order20 <- wp_freq_order20 %>% as.table()

wp_freq_order20 %>% class()

##워드크라우드
wp_freq_order20_워드크라우드 <- wp_freq_order20 %>% wordcloud2(minRotation = 0, maxRotation = 0, shape ="circle")

##빈도그래프
wp_freq_order20df <- wp_freq_order20 %>% as.data.frame()
wp_freq_order20_빈도그래프 <- ggplot(wp_freq_order20df, aes(x=Freq, y=reorder(Var1,Freq), fill = Freq)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#0000FF")


## 특정 단어와의 상관관계 (sparsity가 0인 경우 상관관계가 계산되지 않음)
findAssocs(wp_dtm, "biden", corlimit = 0.85)



### wsj
wsj_txt <- readLines("D:/대학원/선형모형론/2학기/term/wsj.txt")
wsj_txt <- gsub("['’]s\\b|[^[:alnum:][:blank:]@_]","",wsj_txt) #따옴표 제거 정규식
wsj_int <- c()

for (i in 2:length(wsj_txt)){
  wsj_int <- paste0(wsj_int,wsj_txt[i],"."," ")
}

wsj_docs <- Corpus(VectorSource(wsj_int))

wsj_docs <- tm_map(wsj_docs, content_transformer(tolower))
wsj_docs <- tm_map(wsj_docs, removeNumbers)
wsj_docs <- tm_map(wsj_docs, removePunctuation)
wsj_docs <- tm_map(wsj_docs, removeWords, stopwords("english"))
wsj_docs <- tm_map(wsj_docs, removeWords, c("the", "and","'s","biden","joe","can","cant","will","that","weve","dint","wont","youll","youre","wp","wsj","says","new"))
wsj_docs <- tm_map(wsj_docs, stripWhitespace)

wsj_dtm <- DocumentTermMatrix(wsj_docs)
wsj_dtm %>% dim()

wsj_freq <- colSums(as.matrix(wsj_dtm))

wsj_freq_order <- sort(wsj_freq, decreasing = TRUE)

wsj_freq_order20 <- wsj_freq_order[1:20]
wsj_freq_order20 <- wsj_freq_order20 %>% as.table()

wsj_freq_order20 %>% class()

##워드크라우드
wsj_freq_order20_워드크라우드 <- wsj_freq_order20 %>% wordcloud2(minRotation = 0, maxRotation = 0, shape ="circle")

##빈도그래프
wsj_freq_order20df <- wsj_freq_order20 %>% as.data.frame()
wsj_freq_order20_빈도그래프 <- ggplot(wsj_freq_order20df, aes(x=Freq, y=reorder(Var1,Freq), fill = Freq)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#000000", high = "#FF0000")


## 특정 단어와의 상관관계 (sparsity가 0인 경우 상관관계가 계산되지 않음)
findAssocs(wsj_dtm, "trump", corlimit = 0.5)



####주제모형
library(topicmodels)
library(qdap)

### wp
## 주제개수 지정
library(ldatuning)

wp_lda_n <- FindTopicsNumber(
  wp_dtm,
  topics = 1:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
  mc.cores = 2L,
  verbose = TRUE
)

wp_lda_n %>% FindTopicsNumber_plot()

## 주제모형 산출
library(dplyr)
library(tidytext)
wp_lda <- LDA(wp_dtm, k=3, method = "Gibbs", control=list(alpha=1, delta=0.1, seed=12))

terms(wp_lda,20) %>% head(10)

wp_topics <- wp_lda %>% tidy(matrix = "beta")

wp_top_terms <- wp_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wp_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


### wsj
## 주제개수 지정
wsj_lda_n <- FindTopicsNumber(
  wsj_dtm,
  topics = 1:10,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 12),
  mc.cores = 2L,
  verbose = TRUE
)

wsj_lda_n %>% FindTopicsNumber_plot()

## 주제모형 산출
wsj_lda <- LDA(wsj_dtm, k=4, method = "Gibbs", control=list(alpha=1, delta=0.1, seed=12))

terms(wsj_lda ,20) %>% head(10)

wsj_topics <- wsj_lda %>% tidy(matrix = "beta")

wsj_top_terms <- wsj_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wsj_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


## wp vs wsj 비교 양적분석
# wp
wp_int <- iconv(wp_int, "latin1", "ASCII","")
wp_prep <- wp_int %>% qprep()
wp_prep <- wp_prep %>% replace_contraction()
wp_prep <- wp_prep %>% rm_stopwords(Top100Words, separate = FALSE)
wp_prep <- wp_prep %>% strip(char.keep = c("?","."))

wp_df <- data.frame(press = wp_prep)
wp_df <- sentSplit(wp_df, "press")
wp_df$searchword <- "wp"


# wsj
wsj_prep <- iconv(wsj_int, "latin1", "ASCII","")
wsj_prep <- wsj_prep %>% qprep()
wsj_prep <- wsj_prep %>% replace_contraction()
wsj_prep <- wsj_prep %>% rm_stopwords(Top100Words, separate = FALSE)
wsj_prep <- wsj_prep %>% strip(char.keep = c("?","."))

wsj_df <- data.frame(press = wsj_prep)
wsj_df <- sentSplit(wsj_df, "press")
wsj_df$searchword <- "wsj"


## wp, wsj 데이터프레임 합치기
wp_wsj <- rbind(wp_df,wsj_df)
wp_wsj <- wp_wsj %>% data.frame()

wp_wsj_press <- wp_wsj$press
wp_wsj_searchword <- wp_wsj$searchword
wp_wsj_terms <- wp_wsj_press %>% freq_terms()
wp_wsj_plot <- wp_wsj_terms %>% plot()

# 종합 단어 통계
wp_wsj_ws <- word_stats(wp_wsj_press,wp_wsj_searchword, rm.incomplete = TRUE)
plot(wp_wsj_ws, label = TRUE, lab.digit = 2)
wp_wsj_ws %>% View()


# 정서점수-??와 충돌하는듯
wp_wsj_pol <- polarity(wp_wsj_press, wp_wsj_searchword)

wp_wsj_pol_plot <- wp_wsj_pol %>% plot()

# wp 정서점수
wp_press <- wp_df$press
wp_searchword <- wp_df$searchword
wp_terms <- wp_press %>% freq_terms()

wp_pol <- polarity(wp_press, wp_searchword)
wp_pol_plot <- wp_pol %>% plot()
wp_pol_plot$p1

# wsj 정서점수
wsj_press <- wsj_df$press
wsj_searchword <- wsj_df$searchword
wsj_terms <- wsj_press %>% freq_terms()

wsj_pol <- polarity(wsj_press, wsj_searchword)
wsj_pol_plot <- wsj_pol %>% plot()
wsj_pol_plot$p1



## 가장 부정적-긍정적 문장 찾기
# wp
wp_press <- wp_df$press
wp_searchword <- wp_df$searchword

wp_pol <- polarity(wp_press, wp_searchword)

wp_pol.all <- wp_pol$all

wp.most.neg <- which.min(wp_pol.all$polarity)
wp.most.pos <- which.max(wp_pol.all$polarity)

wp_pol.all$text.var[wp.most.neg]
wp_pol.all$text.var[wp.most.pos]

# wsj
wsj_press <- wsj_df$press
wsj_searchword <- wsj_df$searchword

wsj_pol <- polarity(wsj_press, wsj_searchword)

wsj_pol.all <- wsj_pol$all

wsj.most.neg <- which.min(wsj_pol.all$polarity)
wsj.most.pos <- which.max(wsj_pol.all$polarity)

wsj_pol.all$text.var[wsj.most.neg]
wsj_pol.all$text.var[wsj.most.pos]

## 가독성
wp_wsj_ari <- automated_readability_index(wp_wsj_press, wp_wsj_searchword)
wp_wsj_ari %>% plot()

## 형식성(품사)
wp_wsj_form <- formality(wp_wsj_press, wp_wsj_searchword)
wp_wsj_form$form.prop.by

## 다양성
wp_wsj_div <- diversity(wp_wsj_press, wp_wsj_searchword)

## 분산성
dispersion_plot(wp_wsj_press,
                rm.vars = wp_wsj_searchword,
                c("democrats", "election", "president","coronavirus"),
                color = "blue", bg.color = "white")






# 데이터 전처리
names(df_ytb_com) <- c("글쓴이","댓글","날짜","제목")

TextDoc <- Corpus(VectorSource(df_ytb_com$제목))

TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, removeWords, c("the", "and","'s","can","cant","will","that","weve","dint","wont","youll","youre","says","new"))
TextDoc <- tm_map(TextDoc, stripWhitespace)

TextDoc_dtm <- TermDocumentMatrix(TextDoc) # Sparsity : 0이 아닌 숫자가 모여있는 정도
dtm_mat <- TextDoc_dtm %>% as.matrix()
dtm_mat <- sort(rowSums(dtm_mat), decreasing=TRUE)
dtm_tb <- tibble(word = names(dtm_mat),freq = dtm_mat)

ggplot(dtm_tb, aes(x = freq, y = reorder(word, freq), fill = freq)) + 
  geom_col() + 
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  labs(x="", y="") + 
  theme(legend.position = "none") +
  scale_fill_gradient(low = "#5D5D5D", high = "#003300") +
  geom_text(aes(label = freq),hjust = -0.1,size=5) 

## 워드클라우드 작성
dtm$freq <- dtm$freq %>% as.integer()
wordcloud2(dtm, minRotation=0, maxRotation=0, color = "#003300")

# 연관분석
findAssocs(TextDoc_dtm, terms = c("asia","hate","covid"), corlimit = 0.25)
