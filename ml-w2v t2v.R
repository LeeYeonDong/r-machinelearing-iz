# install.packages("tokenizers")
library(tokenizers)
# install.packages("text2vec")
# install.packages("uwot")
# install.packages("word2vec")
library(word2vec)
library(uwot)
library(text2vec)
library(Matrix)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(broom)
library(stringr)
library(tidyr)
library(readr)
library(tm)
library(RTextTools)
library(tidyverse)
library(NLP)
library(qdap)
library(corpus)
library(wordcloud2)
library(stringr)
library(rJava)
library(base64enc)
library(RmecabKo)
library(reshape2)
library(tidytext)
library(umap)
library(gridExtra)

# data 준비
뉴스_sports.news_NC <- read_csv(file = "D:/대학원/논문/소논문/뉴스_sports.news_NC.csv", col_names = TRUE, locale=locale('ko',encoding='euc-kr'))

names(뉴스_sports.news_NC) <- c("언론사_NC","제목_NC","날짜_NC","링크_NC","좋아_NC","훈훈_NC","슬퍼_NC","화나_NC","후속_NC")

뉴스_NC <- 뉴스_sports.news_NC[!is.na(뉴스_sports.news_NC$날짜_NC),]

뉴스_NC$ID_NC <- c(1:nrow(뉴스_NC))

뉴스_NC %>% head()

뉴스_NC$제목_NC <- gsub("포토","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("오늘","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("경기","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("사진","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("스포츠","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("종합","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("다시","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("치어리더","",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("\u7f8e","미국",뉴스_NC$제목_NC)
뉴스_NC$제목_NC <- gsub("\u65e5","일본",뉴스_NC$제목_NC)

## divide reviewtext into separate words
뉴스_NC %>% str()
ID제목_NC <- 뉴스_NC %>% dplyr::select("ID_NC","제목_NC") ## MASS와 충돌

# 데이터 분할
ID제목_NC_dim <- ID제목_NC %>% dim()

ID제목_NC_dim_int <- ID제목_NC_dim[1] / 10000
ID제목_NC_dim_int <- ID제목_NC_dim_int %>% ceiling()
n <- ID제목_NC_dim_int

ID제목_NC_sp <- split(ID제목_NC,rep(1:n, each = 10000))

# 데이터 셋 만들기
ID제목_NC_tb <- list()
ID제목_NC_data_set <- list()
ID제목_NC_token <- c()
ID제목_NC_한글 <- list()
ID제목_NC_영어 <- list()
ID제목_NC <- tibble()

## 단어기준 토큰화
for (i in 1:n){
  
  cat(i, '번째 데이터 리스트 tokenizer', '중 입니다.\n') 
  
  ID제목_NC_tb[[i]] <- ID제목_NC_sp[[i]] %>% 
    tibble() %>% 
    unnest_tokens(input = 제목_NC, output = word_NC, token = "words", drop = FALSE)
  
  names(ID제목_NC_tb[[i]]) <- c("ID_NC","제목_NC","word_NC")
  
  ID제목_NC_한글[[i]] <- ID제목_NC_tb[[i]] %>%  
    mutate(한글_NC = str_match(word_NC,'([가-힣]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## ([가-힣]+)/P') 한글만을 선택하는 정규표현식
    mutate(글자수_NC = str_length(한글_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(한글_NC) >= 2) 
  
  ID제목_NC_영어[[i]] <- ID제목_NC_tb[[i]] %>%  
    mutate(영어_NC = str_match(word_NC,'([a-zA-Z]+)')[,2]) %>% ## "한글" variable을 만들고 한글만 저장         
    na.omit() %>% ## 
    mutate(글자수_NC = str_length(영어_NC)) %>%   ## "글자수" variable을 만듭니다 
    filter(str_length(영어_NC) >= 2) 
  
  ID제목_NC <- bind_rows(ID제목_NC,ID제목_NC_한글[[i]])
  ID제목_NC <- bind_rows(ID제목_NC,ID제목_NC_영어[[i]])
}

NC_tokens <- ID제목_NC %>% dplyr::select("ID_NC","제목_NC","word_NC","글자수_NC")

## count the number of words per article and plot results
NC_tokens %>% 
  group_by(ID_NC) %>% 
  summarise(n_tokens = n()) %>%
  mutate(n_tokens_binned = cut(n_tokens, breaks = c(1,seq(0,40,3),Inf))) %>% 
  group_by(n_tokens_binned) %>% 
  summarise(n_article = n()) %>% 
  ggplot(aes(x = n_tokens_binned, y = n_article)) + 
  geom_bar(stat = 'identity',fill = 'blue') + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 5, size = 10)) + 
  geom_text(size = 5, aes(label = n_article), position = position_dodge(width = 1), vjust = -0.5)

# 신경망을 구축 할 때 총 5 개 이상의 빈도를 가진 단어 만 사용. 현재 컬렉션은 60,016개의 고유 한 단어로 내려갑니다.
NC_tokens %>% 
  group_by(word_NC) %>% 
  summarize(word_freq = n()) %>% 
  mutate(min_5_freq = case_when(word_freq < 5 ~'token frequency : < 5', TRUE ~ 'token frequency : >= 5')) %>% 
  group_by(min_5_freq) %>% 
  summarise(n_tokens = n()) %>% 
  mutate(ratio_tokens = n_tokens / sum(n_tokens)) 

NC_tokens_min5 <- NC_tokens %>% 
  group_by(word_NC) %>% 
  mutate(token_freq = n()) %>%  
  filter(token_freq >= 5) %>% 
  group_by(ID_NC) %>% 
  summarise(NCTextClean = str_c(word_NC, collapse = " "))


# 신경망이 데이터를 입력으로 취할 수 있도록 토큰을 정수로 변환 필요. 신경망에 직접 텍스트를 input할 수 없다. 신경망 용어로 기사 (샘플) 및 단어 벡터 (특징)가있는 2 차원 행렬이 필요. 이를 위해서는 입력 기능의 길이가 같아야합니다. 아래에서는 텍스트를 벡터화하고 인덱스를 만들고 패딩 (0 추가)을 사용하여 동일한 크기를 만듭니다.

#label 붙이기 긍정 or 부정
NC_tokens_min5$label <- rbinom(n = length(NC_tokens_min5$ID_NC), size = 1, prob = 0.1)

TextClean_NC <- NC_tokens_min5 %>% select(NCTextClean) %>% pull()
label_NC <- NC_tokens_min5 %>% select(label) %>% pull() %>% as.array()


#### word2vec
# 2013년 구글에서 개발한 Word2Vec이라는 방법론이 있습니다. 이름 그대로 단어(Word)를 벡터(Vector)로 바꿔주는 방법입니다. 이를 임베딩(Embedding). GloVe, Fasttext 같은 다른 방법론과의 비교 단어를 벡터화할 때 단어의 문맥적 의미를 보존

model_w2v <- word2vec(x = TextClean_NC, type = "skip-gram", dim = 15, iter = 20) # type : cbow or skip_gram
embedding_w2 <- model_w2v %>% as.matrix()
embedding_w2v_prd <- predict(model_w2v, c("nc"), type = "embedding")

predict(model_w2v, c("nc"), type = "nearest", top_n = 10)


# Visualise semanƟc similarity - Perform dimensionality reducƟon using UMAP
embedding_w2v_vis <- umap(embedding_w2v, n_neighbors = 15, n_threads = 2, metric = "cosine") # n_threads = dimension
embedding_w2v_vis %>% str()

rownames(embedding_w2v_vis) <- embedding_w2v %>% rownames()
head(embedding_w2v_vis,n =10)

embedding_w2v_df <- data.frame(word = rownames(embedding_w2v_vis),
                               x = embedding_w2v_vis[,1],
                               y = embedding_w2v_vis[,2],
                               stringsAsFactors = FALSE)
rownames(embedding_w2v_df) <- NULL

# nc 기준 50개 단어만 시각화
near100 <- predict(model_w2v, c("nc"), type = "nearest", top_n = 100)
near101 <- append(near100$nc$term2,"nc")

embedding_w2v_df %>% 
  filter(word %in% near101) %>% 
  ggplot(aes(x = x, y = y, label = word)) +
  geom_text_repel()


#### text2vec-GloVe
# Word to Vector(word2vec) 방법은 2013년 구글 엔지니어에 의해 개발된 방법이며 Glove는 스텐포드 NLP 연구실에서 개발된 방법이다. 두 방법 모두 단어 뒤에 있는 의미를 학습하는 비지도 학습 방법이다. 이들은 문맥 속의 단어들을 재구조화하기 위해 two-layer neural network를 이용한다.
TextClean_NC
label_NC

it_NC <- itoken(TextClean_NC, 
                ids = label_NC,
                progressbar = TRUE) # itoken(iterablr) : iterator을 생성하는 함수이다. craete_dtm, create_tcm, create_vocabulary 등을 사용하기 위해서는 반드시 itoken 함수를 인자로 넣어주어야 한다.

# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in NC_tokens_min5(최소 5회 이상 언급 단어만)
vocab_NC <- it_NC %>% create_vocabulary() # n-gram 방법으로 단어 사전을 생성해주는 함수이다. (1,1)은 unigram을, (2,2)는 bigram을 (1,2)는 mixing unigram and bigram을 의미한다. 

vocab_NC %>% head()

# text2vec has the option to prune the vocabulary of low-frequent words / prune : 가지치기하다
vocab_NC <- vocab_NC %>% prune_vocabulary(term_count_min = 5)
# 너무 자주 등장하는 단어 혹은 너무 적게 등장하는 단어들을 배제하여 단어 사전을 pruinng 시켜주는 함수이다.

# 여기서 확률론적 언어 모형에 대한 기본적인 개념이 필요하다. 확률론적 언어 모형은 m개의 단어 w1,w2,...,wm이 주어졌을 때 문장으로써 성립될 확률 P(w1,w2,...,wm)을 출력함으로써 이 단어 열이 실제로 현실에서 사용될 수 있는 문장인지를 판별하는 모형이다.
# 이 확률은 각 단어의 확률과 단어들의 조건부 확률을 이용하여 다음과 같이 계산할 수 있다.
# P(w1,w2,...,wm)=P(w1,w2,...,wm-1) x P(wm|w1,w2,...,wm-1)
# =P(w1,w2,...,wm-2) x P(wm-1|w1,w2,...,wm-2) x P(wm|w1,w2,...,wm-1)
# 여기서 P(wm|w1,w2,...,wm-1)은 지금까지 w1,w2,...,wm-1이라는 단어 열이 나왔을 때 그 다음 단어로 wm이 나올 조건부 확률을 말한다. 즉, 조건절에 속한 단어들을 우리는 문맥 정보라고 부른다.
# 이 때 조건부 확률을 어떻게 모형화하느냐에 따라 유니그램모델(Unigram Model), 바이그램모형(Bigram Model), N-그램 모형(N-gram Model) 
# 유니그램 모형 : 만약 모든 단어의 활용이 완전히 서로 독립이라고 한다면 단어 열의 확률은 다음과 같이 각 단어의 확률의 곱이 된다. 이러한 모형을 유니그램 모형이라고 한다.
# 바이그램모형 : 만약 단어의 활용이 바로 전 단어에만 의존한다면 단어 열의 확률은 다음과 같다. 이러한 모형을 Bigram 모형 또는 마코프 모형이라고 한다.
# N-그램 모형 : 만약 단어의 활용이 바로 전 n-1개의 단어에만 의존한다면 단어 열의 확률은 다음과 같다. 이러한 모형을 N-gram 모형이라고 한다.


## Create TCM(co-occurance matrix)
# Vectorize word to integers
vectorizer_NC <- vocab_NC %>% vocab_vectorizer()

# Create a Term-Count-Matrix, by default it will use a skipgram window of 5 (symmetrical)
tcm_NC <- create_tcm(it_NC, vectorizer_NC, skip_grams_window = 5)
# it : iterator(itoken 함수 이용)
#- vectorizer : vectorized object(vocab_vectorizer 함수 이용)
#- grow_dtm : DTM(document term matrix)를 만들건지에 대한 여부
#- skip_grams_windonw : vector word embedding을 계산할 때 타겟 단어 주변에 몇 개의 단어를 n-gram으로 정의할 것인지에 대한 여부
# Skip-gram 모델은 CBOW와는 반대 방향의 모델이다. 현재 주어진 단어 하나를 가지고 주위에 등장하는 나머지 몇 가지의 단어들의 등장 여부를 유추하는 것이다. 이 때 예측하는 단어들의 경우 현재 보고 있는 단어 주위에서 샘플링하는데, '가까운 위치에 있는 단어일수록 현재 단어와 관련이 더 많은 단어일 것이다'라는 생각을 적용하기 위해 멀리 떨어져있는 단어일수록 낮은 확률로 택하는 방법을 사용한다. 

tcm_NC %>% str()

# @Dimnames : 안에 있는 값들은 한 단어가 다른 단어와 같이 일어날 확률을 의미한다.
# 단어들이 같이 등장하는 일이 빈번하게 일어난다면 우리는 이 단어들이 유사한 의미를 지닌다고 유추할 수 있을 것이다


## Glove 모델
# 앞에서는 text2vec 모델을 이용하여 계산을 하였다면 GloVe모델을 만드는 방법에 대해서 다뤄볼 것이다. Glove 모델은 text2vec와 같이 word embedding을 효과적으로 구현해주는 모델이다. 

# Word2Vec는 중심단어로 주변단어를 혹은 주변단어로 중심단어를 예측하는 과정에서 단어를 벡터로 임베딩하는 기법이지만 사용자가 지정한 주변 단어의 갯수 내에서만 학습과 분석이 이루어지기 때문에 말뭉치 전체의 co-occruence를 반영하기 어렵다는 단점이 있다.

# GloVe 연구팀은 특정 단어 k가 주어졌을 때 임베딩된 두 단어벡터의 내적이 두 단어의 동시등장확률 간 비율이 되도록 임베딩을 하려고 했다. 
# maximum number of co-occurrences to use in the weighting function, we choose the entire token set divided by 100
x_max_NC <- length(vocab_NC$doc_count)/100

glove_vector_model_NC <- GloVe$new(rank = 32, x_max = x_max_NC) # glove vector model을 만들어주는 함수다.
# rank : desired dimension for the latent vectors
# x_max : integer maximum number of co-occurrences to use in the weighting function
word_vectors_NC <- glove_vector_model_NC$fit_transform(tcm_NC, n_iter = 20, convergence_tol = 0.01, n_threads = 8) # 한 프로세스내에서도 여러 갈래의 작업들이 동시에 진행될 필요가 있다. 이 갈래를 스레드라고 부른다.

# 근접 단어 뽑기 1
word_승리 <- word_vectors_NC["승리", , drop = FALSE]

cos_sim_승리 <- sim2(x = word_vectors_NC, y = word_승리, method = "cosine", norm = "l2")
sort(cos_sim_승리[,1], decreasing = TRUE) %>% head(10)

#
word_우승 <- word_vectors_NC['nc', ,drop=FALSE]-
  word_vectors_NC['패배', ,drop=FALSE]+
  word_vectors_NC['우승', ,drop=FALSE]

cos_sim_우승 <- sim2(x = word_vectors_NC, y = word_우승, method = "cosine", norm = "l2")
sort(cos_sim_우승[,1], decreasing = TRUE) %>% head(10)


# Visualise semanƟc similarity - Perform dimensionality reducƟon using UMAP
embedding_Glove_vis <- umap(word_vectors_NC, n_neighbors = 15, n_threads = 2, metric = "cosine")

embedding_Glove_vis %>% str()

rownames(embedding_Glove_vis) <- word_vectors_NC %>% rownames()
head(embedding_Glove_vis,n =10)

embedding_Glove_df <- data.frame(word = rownames(embedding_Glove_vis),
                               x = embedding_Glove_vis[,1],
                               y = embedding_Glove_vis[,2],
                               stringsAsFactors = FALSE)
rownames(embedding_Glove_df) <- NULL

# nc 기준 50개 단어만 시각화
near100 <- predict(word_vectors_NC, c("nc"), type = "nearest", top_n = 100)
near101 <- append(near100$nc$term2,"nc")

embedding_Glove_df %>% 
  filter(word %in% near101) %>% 
  ggplot(aes(x = x, y = y, label = word)) +
  geom_text_repel()
