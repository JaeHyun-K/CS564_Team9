

# 2017 ~ 2020 수학 가: 이과, 나: 문과 
# 2012 ~ 2016 수학 가(B): 이과, 나(A): 문과 

library(dplyr)

setwd("C:/Users/youn/Desktop/R_5/3D_visualizing")
getwd()





############ 2015 ############
score.2015 = read.csv("./data/수능2015_ANSI.csv", stringsAsFactors = F, header =T)
str(score.2015)
names(score.2015) = c("region", "kor_A_score", "kor_A_dev", "kor_B_score", "kor_B_dev",
                       "math_A_score", "math_A_dev", "math_B_score", "math_B_dev", "eng_A_score", "eng_A_dev")
score.2015 = score.2015[3:20,]
score.2015 = score.2015 %>% 
  mutate(kor_A_score = as.numeric(kor_A_score)) %>%
  mutate(kor_A_dev = as.numeric(kor_A_dev)) %>%
  mutate(kor_B_score = as.numeric(kor_B_score)) %>%
  mutate(kor_B_dev = as.numeric(kor_B_dev)) %>%
  mutate(math_A_score = as.numeric(math_A_score)) %>%
  mutate(math_A_dev = as.numeric(math_A_dev)) %>%
  mutate(math_B_score = as.numeric(math_B_score)) %>%
  mutate(math_B_dev = as.numeric(math_B_dev)) %>%
  mutate(eng_A_score = as.numeric(eng_A_score)) %>%
  mutate(eng_A_dev = as.numeric(eng_A_dev))
###### 국어 평균
score.2015 = score.2015 %>% 
  mutate(kor_score = (kor_A_score+kor_B_score)/2) %>%
  mutate(kor_dev = (kor_A_dev+kor_B_dev)/2) %>%
  select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2015)
summary(score.2015)

############ 2016 ############
score.2016 = read.csv("./data/수능2016_ANSI.csv", stringsAsFactors = F, header =T)
str(score.2016)
names(score.2016) = c("region", "kor_A_score", "kor_A_dev", "kor_B_score", "kor_B_dev",
                      "math_A_score", "math_A_dev", "math_B_score", "math_B_dev", "eng_A_score", "eng_A_dev")
score.2016 = score.2016[2:19,]
score.2016 = score.2016 %>% 
  mutate(kor_A_score = as.numeric(kor_A_score)) %>%
  mutate(kor_A_dev = as.numeric(kor_A_dev)) %>%
  mutate(kor_B_score = as.numeric(kor_B_score)) %>%
  mutate(kor_B_dev = as.numeric(kor_B_dev)) %>%
  mutate(math_A_score = as.numeric(math_A_score)) %>%
  mutate(math_A_dev = as.numeric(math_A_dev)) %>%
  mutate(math_B_score = as.numeric(math_B_score)) %>%
  mutate(math_B_dev = as.numeric(math_B_dev)) %>%
  mutate(eng_A_score = as.numeric(eng_A_score)) %>%
  mutate(eng_A_dev = as.numeric(eng_A_dev))
###### 국어 평균
score.2016 = score.2016 %>% 
  mutate(kor_score = (kor_A_score+kor_B_score)/2) %>%
  mutate(kor_dev = (kor_A_dev+kor_B_dev)/2) %>%
  select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2016)
summary(score.2016)

############ 2017 ############
score.2017 = read.csv("./data/수능2017_ANSI.csv", stringsAsFactors = F, header =T)
str(score.2017)
names(score.2017) = c("region", "kor_score", "kor_dev",
                      "math_A_score", "math_A_dev", "math_B_score", "math_B_dev", "eng_A_score", "eng_A_dev")
score.2017 = score.2017[2:19,]
score.2017 = score.2017 %>% 
  mutate(kor_score = as.numeric(kor_score)) %>%
  mutate(kor_dev = as.numeric(kor_dev)) %>%
  mutate(math_A_score = as.numeric(math_A_score)) %>%
  mutate(math_A_dev = as.numeric(math_A_dev)) %>%
  mutate(math_B_score = as.numeric(math_B_score)) %>%
  mutate(math_B_dev = as.numeric(math_B_dev)) %>%
  mutate(eng_A_score = as.numeric(eng_A_score)) %>%
  mutate(eng_A_dev = as.numeric(eng_A_dev))
###### 
score.2017 = score.2017 %>% 
  select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2017)
summary(score.2017)

############ 2018 ############
score.2018 = read.csv("./data/수능2018_ANSI.csv", stringsAsFactors = F, header =T)
str(score.2018)
names(score.2018) = c("region", "kor_score", "kor_dev",
                      "math_A_score", "math_A_dev", "math_B_score", "math_B_dev")
score.2018 = score.2018[2:19,]
score.2018 = score.2018 %>% 
  mutate(kor_score = as.numeric(kor_score)) %>%
  mutate(kor_dev = as.numeric(kor_dev)) %>%
  mutate(math_A_score = as.numeric(math_A_score)) %>%
  mutate(math_A_dev = as.numeric(math_A_dev)) %>%
  mutate(math_B_score = as.numeric(math_B_score)) %>%
  mutate(math_B_dev = as.numeric(math_B_dev))

###### 
score.2018 = score.2018 %>% 
  select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2018)
summary(score.2018)

############ 2019 ############
score.2019 = read.csv("./data/수능2019_ANSI.csv", stringsAsFactors = F, header =T)
str(score.2019)
names(score.2019) = c("region", "kor_score", "kor_dev",
                      "math_A_score", "math_A_dev", "math_B_score", "math_B_dev")
score.2019 = score.2019[2:19,]
score.2019 = score.2019 %>% 
  mutate(kor_score = as.numeric(kor_score)) %>%
  mutate(kor_dev = as.numeric(kor_dev)) %>%
  mutate(math_A_score = as.numeric(math_A_score)) %>%
  mutate(math_A_dev = as.numeric(math_A_dev)) %>%
  mutate(math_B_score = as.numeric(math_B_score)) %>%
  mutate(math_B_dev = as.numeric(math_B_dev))

###### 
score.2019 = score.2019 %>% 
  select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2019)
summary(score.2019)

