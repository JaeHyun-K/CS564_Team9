library(cluster)
library(coefplot)
library(dplyr)
library(factoextra)
library(gclus)
library(ggmap)
library(ggplot2)
library(gifski)
library(gpclib)
library(grid)
library(gridExtra)
library(magick)
library(maptools)
library(NbClust)
library(png)
library(raster)
library(rayshader)
library(readxl)
library(reshape)
library(rgdal)
library(rgeos)
library(rgl)
library(rlang)
library(rpsychi)
library(sp)

##### ANOVA on college entrance exam(수능) score by regions in 2019 #####
score <- read.csv('ANOVA/Data/entrance_exam_score(2019).csv', header = T)

## We can conduct ANOVA test with only these information thanks to "rpsychi" package!
score_kor_anova <- with(score, ind.oneway.second(score$mean_k, score$sd_k, score$n))
score_kor_anova$anova.table

score_mathB_anova <- with(score, ind.oneway.second(score$mean_mB, score$sd_mB, score$n))
score_mathB_anova$anova.table



##### Clustering #####
academy <- read.csv('Clustering/Data/student_class_academy_info(2019).csv')
interrupt <- read.csv('Clustering/Data/interruption_info(2015-2019).csv')
score <- read.csv('Clustering/Data/entrance_exam_score(2015-2019).csv')

inter <- dplyr::select(filter(interrupt, year == 2019), c("city", "interruptedRatio"))
sco <- dplyr::select(filter(score, year == 2019), c("city", "korean"))
aca <- dplyr::select(academy, c("city","academyNumRate"))

area <- merge(aca, sco, by="city")
area <- merge(area, inter, by="city")
area <- area[c(9,8,6,12,5,7,11,2,1,17,16,14,13,4,3,15,10),]
rownames(area) <- NULL
area <- area[,-1]
scaled_area <- scale(area)

clust <- NbClust(scaled_area, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index = "all")
kc_3 <- kmeans(scaled_area, centers = 3)
sobj3 <- silhouette(kc_3$cluster, dist(scaled_area))
clusplot(scaled_area, kc_3$cluster, color = TRUE, labels = 3)
plot(sobj3, col = c(3, 5, 7))

## Another clustering method
NbClust(scaled_area, method = "complete", index = "hartigan")
NbClust(scaled_area, method = "complete", index = "kl")

pamx3 <- pam(scaled_area,3)
plot(pamx3)

## Visualizing the clustering result
## We saved clustering results to another file for simplicity
korea <- read.csv('Map_visualization-cluster/Data/KR_NO_ISLE_euc_kr.csv')
map <- shapefile('Map_visualization-cluster/Data/TL_SCCO_CTPRVN.shp')
cluster <- read.csv('Map_visualization-cluster/Data/cluster.csv')

korea_map <- fortify(map, region = "CTPRVN_CD")
mapping <- merge(korea_map, cluster, by = "id")
mapped <- merge(korea, cluster, by = "id")

## Version with island: Thanks to http://www.gisdeveloper.co.kr/?p=2332 !!
ggplot() + geom_polygon(data = mapping, aes(x = long, y = lat, group=group, fill = Group)) +
  scale_fill_gradientn(colours = c(7,3,5), space = "Lab", guide = "legend", breaks = c(1,2,3)) +
  theme_void() + coord_fixed()

## Version without island: Thanks to Tony!!
ggplot() + geom_polygon(data = mapped, aes(x=long, y = lat, group = id, fill = Group)) +
  scale_fill_gradientn(colours = c(7,3,5), space = "Lab", guide = "legend", breaks = c(1,2,3)) +
  theme_void() + coord_fixed()



##### Linear Regression #####
learning_data = read.csv('Linear_regression/Data/student_class_academy_info(2019).csv')
score_data = read.csv('Linear_regression/Data/entrance_exam_score(2015-2019).csv')

score2019_data = score_data[score_data$year==2019,] %>% dplyr::select(1,3,6,7)
score2019_data = score2019_data[c(1,2,3,4,5,6,7,17,8,9,10,11,12,13,14,15,16),]
score2019_data

learning_data[,"korean_score"] = score2019_data[1:17,2]
learning_data[,"mathA_score"] = score2019_data[1:17,3]
learning_data[,"mathB_score"] = score2019_data[1:17,4]
learning_data[,"total_score"] = score2019_data[1:17,2] + score2019_data[1:17,3] + score2019_data[1:17,4]
learning_data
learning_data[,3:10] = scale(learning_data[,3:10])
learning_data

public_fit = lm(korean_score ~ classroomNumRate + teacherNumRate, data = learning_data)
summary(public_fit)

private_fit = lm(korean_score ~ academyNumRate + academyPayRate, data = learning_data)
summary(private_fit)

academy_pay_fit = lm(korean_score ~ academyPayRate, data = learning_data)
with(learning_data, plot(academyPayRate, korean_score, pch = 21, bg = "cyan"))
lines(learning_data$academyPayRate, academy_pay_fit$fitted.values, col = "red")



##### Visualization of differences of college entrance exam score #####
## inside Seoul
SEscore <- read_excel('Map_visualization/Data/exam_score_in_Seoul.xlsx')
map <- shapefile('Map_visualization/Data/SIG_201703/TL_SCCO_SIG.shp')

map <- fortify(map, region = "SIG_CD")
map$id <- as.numeric(map$id)
seoul <- map[map$id <= 11740,]
seoul_map <- merge(seoul, SEscore, by = "id")

plot1 <- ggplot() + geom_polygon(data = seoul_map, aes(x = long, y = lat, group = group.x, fill = score))

plot1 + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_void() + coord_fixed() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) # Result 1

ggplot() + geom_polygon(data = seoul_map, aes(x = long, y = lat, group = group.x, fill = group.y)) +
  theme_void() + coord_fixed() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) # Result 2

SEscore %>% group_by(group) %>% summarise(mean_score = mean(score))

## among Korea
examScore <- read_excel('Map_visualization/Data/exam_score_by_regions.xlsx')
map <- shapefile('Map_visualization/Data/CTPRVN_201703/TL_SCCO_CTPRVN.shp')

examScore2019 <- cbind(examScore[1], examScore[13])
examScore2019 <- cbind(examScore2019, examScore[11]-examScore[12])
colnames(examScore2019) <- list("region", "id", "value")
examScore2019

newmap <- fortify(map, region = "CTPRVN_CD")
korea <- merge(newmap, examScore2019, by = "id")

ggplot() + geom_polygon(data = korea, aes(x = long, y = lat, group = group, fill = value))+
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_void() + coord_fixed() # Result 3



##### 3D Visualization #####
interruption = read.csv('3D_visualizing/Data/interruption_ANSI.csv', stringsAsFactors = F, header = T)
private.ed.per.month <- read.csv('3D_visualizing/Data/월평균_사교육비_ANSI.csv', stringsAsFactors = F, header = T)

#### Need to preprocess data first ####
## 2015
score.2015 = read.csv('3D_visualizing/Data/수능2015_ANSI.csv', stringsAsFactors = F, header = T)
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
## 국어 평균
score.2015 = score.2015 %>% 
  mutate(kor_score = (kor_A_score+kor_B_score)/2) %>%
  mutate(kor_dev = (kor_A_dev+kor_B_dev)/2) %>%
  dplyr::select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2015)
summary(score.2015)

## 2016
score.2016 = read.csv('3D_visualizing/Data/수능2016_ANSI.csv', stringsAsFactors = F, header = T)
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
## 국어 평균
score.2016 = score.2016 %>% 
  mutate(kor_score = (kor_A_score+kor_B_score)/2) %>%
  mutate(kor_dev = (kor_A_dev+kor_B_dev)/2) %>%
  dplyr::select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2016)
summary(score.2016)

## 2017
score.2017 = read.csv('3D_visualizing/Data/수능2017_ANSI.csv', stringsAsFactors = F, header = T)
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

score.2017 = score.2017 %>% 
  dplyr::select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2017)
summary(score.2017)

## 2018
score.2018 = read.csv('3D_visualizing/Data/수능2018_ANSI.csv', stringsAsFactors = F, header = T)
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

score.2018 = score.2018 %>% 
  dplyr::select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2018)
summary(score.2018)

## 2019
score.2019 = read.csv('3D_visualizing/Data/수능2019_ANSI.csv', stringsAsFactors = F, header = T)
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

score.2019 = score.2019 %>% 
  dplyr::select(region, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev)
str(score.2019)
summary(score.2019)

#### Analyzing mean and standard deviations ####
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

#### Now we are prepared to draw maps ####
memory.limit(size = 32000)

## draw empty map
kormap <- readRDS('3D_visualizing/Data/geodata/gadm36_KOR_1_sp.rds')
kormapfd = fortify(kormap)

## preprocessing for dot plot
names.eng.kor = data.frame(Eng_name = c("Busan","Chungcheongbuk-do","Chungcheongnam-do", "Daegu","Daejeon",
                                        "Gangwon-do", "Gwangju", "Gyeonggi-do", "Gyeongsangbuk-do",
                                        "Gyeongsangnam-do","Incheon","Jeju","Jeollabuk-do",
                                        "Jeollanam-do","Sejong","Seoul","Ulsan"),
                           Kor_name = c("부산", "충북", "충남", "대구", "대전",
                                        "강원", "광주", "경기", "경북", "경남",
                                        "인천", "제주", "전북", "전남", "세종",
                                        "서울", "울산"),
                           id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9))
## Id of names.eng.kor is same as kormapfd

interruption = interruption %>%
  mutate(Kor_name = city) %>% 
  dplyr::select(year, highschoolStudentNum, interruptedNum ,interruptedRatio ,Kor_name)
interruption = merge(interruption, names.eng.kor, by="Kor_name")

names(private.ed.per.month) = c("Kor_name", "y.2018", "y.2019", "difference" , "elementary", "middle", "high")
private.ed.per.month = private.ed.per.month[1:17,] %>%
  dplyr::select(Kor_name, y.2019, elementary, middle, high)
private.ed.per.month = merge(private.ed.per.month, names.eng.kor, by="Kor_name")


## get long and lat
kor.long.lat = data.frame()
for(i in names.eng.kor$id) {
  #print(i)
  kor.long.lat = rbind(kor.long.lat, kormapfd %>% filter(id == i) %>% summarise(long = mean(long), lat = mean(lat)))
}
kor.long.lat = cbind(kor.long.lat, id= names.eng.kor$id)

## little change for food visualization
kor.long.lat[1,]$long = 129.2029
kor.long.lat[1,]$lat  = 35.15279
kor.long.lat[3,]$long = 126.9672
kor.long.lat[3,]$lat  = 36.42187
kor.long.lat[4,]$long = 128.6075 
kor.long.lat[4,]$lat  = 35.73772
kor.long.lat[5,]$long = 127.4657   
kor.long.lat[5,]$lat  = 36.28762
kor.long.lat[6,]$long = 128.4957     
kor.long.lat[6,]$lat  = 37.67871
kor.long.lat[7,]$long = 126.9552      
kor.long.lat[7,]$lat  = 35.10388    
kor.long.lat[8,]$long = 127.3098       
kor.long.lat[8,]$lat  = 37.15929  
kor.long.lat[9,]$long = 128.7886        
kor.long.lat[9,]$lat  = 36.20868   
kor.long.lat[10,]$long = 128.3478         
kor.long.lat[10,]$lat  = 35.24451    
kor.long.lat[11,]$long = 126.5672          
kor.long.lat[11,]$lat  = 37.46187   
kor.long.lat[12,]$long = 126.5944           
kor.long.lat[12,]$lat  = 33.36291   
kor.long.lat[13,]$long = 127.2419            
kor.long.lat[13,]$lat  = 35.64812    
kor.long.lat[14,]$long = 126.9320             
kor.long.lat[14,]$lat  = 34.7842      
kor.long.lat[15,]$long = 127.2871              
kor.long.lat[15,]$lat  = 36.50711
kor.long.lat[16,]$long = 127.0717               
kor.long.lat[16,]$lat  = 37.47785         
kor.long.lat[17,]$long = 129.3102                
kor.long.lat[17,]$lat  = 35.48490           

## Set map as 0 height, change "kor_score" for other subjects
kormapfd_score_helper1 = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                   interruptedRatio = rep(0,17)) 
kormapfd1 = merge(kormapfd, kormapfd_score_helper1, by = "id")


kormapfd_score_helper2 = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                    kor_score = rep(0,17))
kormapfd2 = merge(kormapfd, kormapfd_score_helper2, by = "id")


kormapfd_score_helper3 = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                    math_A_score = rep(0,17)) 
kormapfd3 = merge(kormapfd, kormapfd_score_helper3, by = "id")


kormapfd_score_helper4 = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                    math_B_score = rep(0,17)) 
kormapfd4 = merge(kormapfd, kormapfd_score_helper4, by = "id")


kormapfd_score_helper5 = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                    y.2019 = rep(0,17)) 
kormapfd5 = merge(kormapfd, kormapfd_score_helper5, by = "id")


kor.long.lat.wname = merge(kor.long.lat, names.eng.kor, by="id")

## processing interruption
interruption.2015.wid = interruption %>% filter(year == 2015) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, interruptedRatio, long, lat, id = id.x)
interruption.2016.wid = interruption %>% filter(year == 2016) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, interruptedRatio, long, lat, id = id.x)
interruption.2016.wid$long = interruption.2016.wid$long + 0.05
interruption.2016.wid$lat = interruption.2016.wid$lat + 0.05 * 1.3
interruption.2017.wid = interruption %>% filter(year == 2017) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, interruptedRatio, long, lat, id = id.x)
interruption.2017.wid$long = interruption.2017.wid$long - 0.04
interruption.2017.wid$lat = interruption.2017.wid$lat + 0.04 * 2 * 1.5
interruption.2018.wid = interruption %>% filter(year == 2018) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, interruptedRatio, long, lat, id = id.x)
interruption.2018.wid$long = interruption.2018.wid$long - 0.05 -0.08
interruption.2018.wid$lat = interruption.2018.wid$lat + 0.05 * 1.3
interruption.2019.wid = interruption %>% filter(year == 2019) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, interruptedRatio, long, lat, id = id.x)
interruption.2019.wid$long = interruption.2019.wid$long - 0.08
interruption.2019.wid$lat = interruption.2019.wid$lat

interruption_limit = c(0.005938, 0.020053)

## processing score
score.2015.wid = score.2015 %>%
  mutate(Kor_name = region) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev, id,long,lat)
score.2016.wid = score.2016 %>%
  mutate(Kor_name = region) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev, id,long,lat)
score.2016.wid$long = score.2016.wid$long + 0.05
score.2016.wid$lat = score.2016.wid$lat + 0.05 * 1.3
score.2017.wid = score.2017 %>%
  mutate(Kor_name = region) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev, id,long,lat)
score.2017.wid$long = score.2017.wid$long - 0.04
score.2017.wid$lat = score.2017.wid$lat + 0.04 * 2 * 1.5
score.2018.wid = score.2018 %>%
  mutate(Kor_name = region) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev, id,long,lat)
score.2018.wid$long = score.2018.wid$long - 0.05 -0.08
score.2018.wid$lat = score.2018.wid$lat + 0.05 * 1.3
score.2019.wid = score.2019 %>%
  mutate(Kor_name = region) %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, kor_score, kor_dev, math_A_score, math_A_dev, math_B_score, math_B_dev, id,long,lat)
score.2019.wid$long = score.2019.wid$long - 0.08
score.2019.wid$lat = score.2019.wid$lat

kor_score_limit = c(93.6, 104.5)
math_A_score_limit = c(88.0, 106.1)
math_B_score_limit = c(86.9, 107.3)

## processing private.ed.per.month
private.ed.per.month.wid = private.ed.per.month %>%
  merge(kor.long.lat.wname, by = "Kor_name") %>%
  dplyr::select(Kor_name, y.2019, long, lat, id = id.x)
private.ed.per.month.wid$long = private.ed.per.month.wid$long - 0.04
private.ed.per.month.wid$lat = private.ed.per.month.wid$lat + 0.04 * 2 * 1.5

private_ed_limit = c(18.10, 45.10)

## save map size
empty_map = ggplot() + 
  geom_polygon(data = kormapfd, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())
xlim = ggplot_build(empty_map)$layout$panel_scales_x[[1]]$range$range
ylim = ggplot_build(empty_map)$layout$panel_scales_y[[1]]$range$range

## plot it as 3D ##
## https://www.rayshader.com/reference/plot_3d.html
## https://github.com/cydalytics/HK_Properties_Price_Distribution/blob/master/HK_Properties_Price_Distribution.R

## interruption
map_2d = ggplot() + 
  geom_polygon(data = kormapfd1, aes(x = long, y = lat, group = group, color = interruptedRatio), fill = "white") +
  xlim(xlim[1], xlim[2]) + # x-axis Mapping
  ylim(ylim[1], ylim[2]) + # y-axis Mapping
  geom_point(data = interruption.2015.wid, aes(x = long, y = lat, color = interruptedRatio), size = 1.4) + # Points
  geom_point(data = interruption.2016.wid, aes(x = long, y = lat, color = interruptedRatio), size = 1.4) + # Points
  geom_point(data = interruption.2017.wid, aes(x = long, y = lat, color = interruptedRatio), size = 1.4) + # Points
  geom_point(data = interruption.2018.wid, aes(x = long, y = lat, color = interruptedRatio), size = 1.4) + # Points
  geom_point(data = interruption.2019.wid, aes(x = long, y = lat, color = interruptedRatio), size = 1.4) + # Points
  scale_colour_gradient(name = "Interruption Ratio", 
                        limits = interruption_limit, 
                        low = "#FCB9B2", high = "#820404") + # Price Density Color
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank()) + # Clean Everything
        coord_fixed()
map_2d
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)

## kor_score
map_2d = ggplot() + 
  geom_polygon(data = kormapfd2, aes(x = long, y = lat, group = group, color = kor_score), fill = "white") +
  xlim(xlim[1], xlim[2]) + # x-axis Mapping
  ylim(ylim[1], ylim[2]) + # y-axis Mapping
  geom_point(data = score.2015.wid, aes(x = long, y = lat, color = kor_score), size = 1.4) + # Points
  geom_point(data = score.2016.wid, aes(x = long, y = lat, color = kor_score), size = 1.4) + # Points
  geom_point(data = score.2017.wid, aes(x = long, y = lat, color = kor_score), size = 1.4) + # Points
  geom_point(data = score.2018.wid, aes(x = long, y = lat, color = kor_score), size = 1.4) + # Points
  geom_point(data = score.2019.wid, aes(x = long, y = lat, color = kor_score), size = 1.4) + # Points
  scale_colour_gradient(name = "Score of Korean", 
                        limits = kor_score_limit, 
                        low = "#FCB9B2", high = "#820404") + # Price Density Color
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank()) + # Clean Everything
        coord_fixed()
map_2d
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)

## mathA_score
map_2d = ggplot() + 
  geom_polygon(data = kormapfd3, aes(x = long, y = lat, group = group, color = math_A_score), fill = "white") +
  xlim(xlim[1], xlim[2]) + # x-axis Mapping
  ylim(ylim[1], ylim[2]) + # y-axis Mapping
  geom_point(data = score.2015.wid, aes(x = long, y = lat, color = math_A_score), size = 1.4) + # Points
  geom_point(data = score.2016.wid, aes(x = long, y = lat, color = math_A_score), size = 1.4) + # Points
  geom_point(data = score.2017.wid, aes(x = long, y = lat, color = math_A_score), size = 1.4) + # Points
  geom_point(data = score.2018.wid, aes(x = long, y = lat, color = math_A_score), size = 1.4) + # Points
  geom_point(data = score.2019.wid, aes(x = long, y = lat, color = math_A_score), size = 1.4) + # Points
  scale_colour_gradient(name = "Score of Math A", 
                        limits = math_A_score_limit, 
                        low = "#FCB9B2", high = "#820404") + # Price Density Color
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank()) + # Clean Everything
        coord_fixed()
map_2d
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)

## mathB_score
map_2d = ggplot() + 
  geom_polygon(data = kormapfd4, aes(x = long, y = lat, group = group, color = math_B_score), fill = "white") +
  xlim(xlim[1], xlim[2]) + # x-axis Mapping
  ylim(ylim[1], ylim[2]) + # y-axis Mapping
  geom_point(data = score.2015.wid, aes(x = long, y = lat, color = math_B_score), size = 1.4) + # Points
  geom_point(data = score.2016.wid, aes(x = long, y = lat, color = math_B_score), size = 1.4) + # Points
  geom_point(data = score.2017.wid, aes(x = long, y = lat, color = math_B_score), size = 1.4) + # Points
  geom_point(data = score.2018.wid, aes(x = long, y = lat, color = math_B_score), size = 1.4) + # Points
  geom_point(data = score.2019.wid, aes(x = long, y = lat, color = math_B_score), size = 1.4) + # Points
  scale_colour_gradient(name = "Score of Math B", 
                        limits = math_B_score_limit, 
                        low = "#FCB9B2", high = "#820404") + # Price Density Color
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank()) + # Clean Everything
        coord_fixed()
map_2d
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)

## private education
map_2d = ggplot() + 
  geom_polygon(data = kormapfd5, aes(x = long, y = lat, group = group, color = y.2019), fill = "white") +
  xlim(xlim[1], xlim[2]) + # x-axis Mapping
  ylim(ylim[1], ylim[2]) + # y-axis Mapping
  geom_point(data = private.ed.per.month.wid, aes(x = long, y = lat, color = y.2019), size = 1.4) + # Points
  scale_colour_gradient(name = "private education\n fee per month", 
                        limits = private_ed_limit, 
                        low = "#FCB9B2", high = "#820404") + # Price Density Color
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank()) + # Clean Everything
        coord_fixed()
map_2d
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)
