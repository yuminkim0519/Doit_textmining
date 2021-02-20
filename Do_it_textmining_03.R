### 03-1 단어 빈도 비교하기

## 텍스트 합치기
# 데이터 불러오기
library(dplyr)

raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>% 
  as_tibble() %>% 
  mutate(president = "moon")

raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>% 
  as_tibble() %>% 
  mutate(president = "park")

# 데이터 합치기 - bind_rows()
bind_speeches <- bind_rows(moon, park) %>% 
  select(president, value)


## 집단별 단어 빈도 구하기
# 기본적인 전처리 및 토큰화
library(stringr)

speeches <- bind_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

speeches

# 토큰화
library(tidytext)
library(KoNLP)
useNIADic()

speeches <- speeches %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

speeches

# 하위 집단별 단어 빈도 구하기 - count()
frequency <- speeches %>% 
  count(president, word) %>%   # 연설문 및 단어별 빈도
  filter(str_count(word) > 1)  # 두 글자 이상 추출

head(frequency)

## 자주 사용된 단어 추출하기 - slice_max()
top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10)

top10
top10 %>% 
  filter(president == "park") %>% 
  print(n = Inf)

## 빈도 동점 단어 제외하고 추출하기 - slice_max(with_ties = F)
top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F)

top10

## 막대 그래프 만들기
#변수의 항목별로 그래프 만들기 - facet_wrap()
library(ggplot2)
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president)

# 그래프별로 y축 설정하기 - facet_wrap(scales = "free_y")
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president,
             scales = "free_y")

# 특정 단어 제거하고 막대 그래프 만들기
top10 <- frequency %>% 
  filter(word != "국민") %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F)

ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president,
             scales = "free_y")

# 축 정렬하기
# 그래프별로 축 정렬하기 - reorder_within()
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")

# 변수 항목 제거하기 - sclae_x_reordered()
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) 

### 03-2 오즈비 - 상대적으로 중요한 단어 비교하기
## long form을 wide form으로 변환하기 - pivot_wider()
df_long <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10) %>% 
  filter(word %in% c("국민", "우리", "정치", "행복"))

install.packages("tidyr")
library(tidyr)

df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n)

# NA를 0으로 바꾸기 - pivot_wider(values_fill = list(n = ))
df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

# 연설문 단어 빈도를 wide form으로 변환하기
frequency_wide <- frequency %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

## 오즈비(odds ratio) 구하기
# 단어의 비중을 나타낸 변수 추가하기
frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon+1) / (sum(moon+1))),  # moon에서 단어의 비중
         ratio_park = ((park+1) / (sum(park+1))))  # park에서 단어의 비중

frequency_wide

# 오즈비 변수 추가하기
frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_moon / ratio_park)


frequency_wide %>% 
  arrange(odds_ratio)

# 오즈비 간단히 구하기
frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ((moon) / (sum(moon))),
         ((park) / (sum(park))))

## 상대적으로 중요한 단어 추출하기
# 오즈비가 가장 높거나 가장 낮은 단어 추출하기 - filter(rank())
top10 <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

top10 %>% 
  arrange(-odds_ratio) %>% 
  print(n = Inf)

## 막대 그래프 만들기
# 비중이 큰 연설문을 나타낸 변수 추가하기
top10 <- top10 %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))

# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()

# 그래프별로 축 설정하기(각 데이터의 빈도 기준으로 축 설정)
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~president, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL)


## 주요 단어가 사용된 문장 살펴보기
# 원문을 문장 기준으로 토큰화하기
speeche_sentence <- bind_speeches %>% 
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

# 주요 단어가 사용된 문장 추출하기 - str_detect()
speeche_sentence %>% 
  filter(president == "moon" & str_detect(sentence, "복지국가"))

speeche_sentence %>% 
  filter(president == "park" & str_detect(sentence, "행복"))

## 중요도가 비슷한 단어 살펴보기
frequency_wide %>% 
  filter(moon >= 5 & park >= 5) %>% 
  arrange(abs(1 - odds_ratio)) %>% 
  head(10)
