setwd("C:/Users/youn/Desktop/R_5/3D_visualizing")
getwd()

library(ggplot2)
library(dplyr)

mean_data = data.frame(year = c(2015,2016,2017,2018,2019))

# kor data
tmp = score.2015 %>% arrange(desc(kor_score)) %>% head(5) %>% summarise(mean_kor_score_H = mean(kor_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(kor_score)) %>% head(5) %>% summarise(mean_kor_score_H = mean(kor_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(kor_score)) %>% head(5) %>% summarise(mean_kor_score_H = mean(kor_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(kor_score)) %>% head(5) %>% summarise(mean_kor_score_H = mean(kor_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(kor_score)) %>% head(5) %>% summarise(mean_kor_score_H = mean(kor_score)))
mean_data = cbind(tmp, mean_data)

tmp = score.2015 %>% arrange(desc(kor_score)) %>% tail(5) %>% summarise(mean_kor_score_L = mean(kor_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(kor_score)) %>% tail(5) %>% summarise(mean_kor_score_L = mean(kor_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(kor_score)) %>% tail(5) %>% summarise(mean_kor_score_L = mean(kor_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(kor_score)) %>% tail(5) %>% summarise(mean_kor_score_L = mean(kor_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(kor_score)) %>% tail(5) %>% summarise(mean_kor_score_L = mean(kor_score)))
mean_data = cbind(tmp, mean_data)

# math A data
tmp = score.2015 %>% arrange(desc(math_A_score)) %>% head(5) %>% summarise(mean_math_A_score_H = mean(math_A_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(math_A_score)) %>% head(5) %>% summarise(mean_math_A_score_H = mean(math_A_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(math_A_score)) %>% head(5) %>% summarise(mean_math_A_score_H = mean(math_A_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(math_A_score)) %>% head(5) %>% summarise(mean_math_A_score_H = mean(math_A_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(math_A_score)) %>% head(5) %>% summarise(mean_math_A_score_H = mean(math_A_score)))
mean_data = cbind(tmp, mean_data)

tmp = score.2015 %>% arrange(desc(math_A_score)) %>% tail(5) %>% summarise(mean_math_A_score_L = mean(math_A_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(math_A_score)) %>% tail(5) %>% summarise(mean_math_A_score_L = mean(math_A_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(math_A_score)) %>% tail(5) %>% summarise(mean_math_A_score_L = mean(math_A_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(math_A_score)) %>% tail(5) %>% summarise(mean_math_A_score_L = mean(math_A_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(math_A_score)) %>% tail(5) %>% summarise(mean_math_A_score_L = mean(math_A_score)))
mean_data = cbind(tmp, mean_data)

# math B data
tmp = score.2015 %>% arrange(desc(math_B_score)) %>% head(5) %>% summarise(mean_math_B_score_H = mean(math_B_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(math_B_score)) %>% head(5) %>% summarise(mean_math_B_score_H = mean(math_B_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(math_B_score)) %>% head(5) %>% summarise(mean_math_B_score_H = mean(math_B_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(math_B_score)) %>% head(5) %>% summarise(mean_math_B_score_H = mean(math_B_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(math_B_score)) %>% head(5) %>% summarise(mean_math_B_score_H = mean(math_B_score)))
mean_data = cbind(tmp, mean_data)

tmp = score.2015 %>% arrange(desc(math_B_score)) %>% tail(5) %>% summarise(mean_math_B_score_L = mean(math_B_score))
tmp = rbind(tmp, score.2016 %>% arrange(desc(math_B_score)) %>% tail(5) %>% summarise(mean_math_B_score_L = mean(math_B_score)))
tmp = rbind(tmp, score.2017 %>% arrange(desc(math_B_score)) %>% tail(5) %>% summarise(mean_math_B_score_L = mean(math_B_score)))
tmp = rbind(tmp, score.2018 %>% arrange(desc(math_B_score)) %>% tail(5) %>% summarise(mean_math_B_score_L = mean(math_B_score)))
tmp = rbind(tmp, score.2019 %>% arrange(desc(math_B_score)) %>% tail(5) %>% summarise(mean_math_B_score_L = mean(math_B_score)))
mean_data = cbind(tmp, mean_data)
mean_data


# check total and each dev of math data

dev_mean_math_A_dev = score.2015 %>% summarise(mean_math_A_dev = mean(math_A_dev)) +
  score.2016 %>% summarise(mean_math_A_dev = mean(math_A_dev)) +
  score.2017 %>% summarise(mean_math_A_dev = mean(math_A_dev)) +
  score.2018 %>% summarise(mean_math_A_dev = mean(math_A_dev)) +
  score.2019 %>% summarise(mean_math_A_dev = mean(math_A_dev))
dev_mean_math_A_dev = dev_mean_math_A_dev/5; dev_mean_math_A_dev

dev_mean_math_B_dev = score.2015 %>% summarise(mean_math_B_dev = mean(math_B_dev)) +
  score.2016 %>% summarise(mean_math_B_dev = mean(math_B_dev)) +
  score.2017 %>% summarise(mean_math_B_dev = mean(math_B_dev)) +
  score.2018 %>% summarise(mean_math_B_dev = mean(math_B_dev)) +
  score.2019 %>% summarise(mean_math_B_dev = mean(math_B_dev))
dev_mean_math_B_dev = dev_mean_math_B_dev/5; dev_mean_math_B_dev

score.2017 %>% summarise(mean_math_A_dev = mean(math_A_dev))
score.2018 %>% summarise(mean_math_A_dev = mean(math_A_dev))
score.2019 %>% summarise(mean_math_A_dev = mean(math_A_dev))

score.2017 %>% summarise(mean_math_B_dev = mean(math_B_dev))
score.2018 %>% summarise(mean_math_B_dev = mean(math_B_dev))
score.2019 %>% summarise(mean_math_B_dev = mean(math_B_dev))
