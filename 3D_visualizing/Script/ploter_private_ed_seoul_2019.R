setwd("C:/Users/youn/Desktop/R_5/3D_visualizing")
getwd()

library(ggplot2)
library(ggmap)
library(rayshader)
library(dplyr)
library(sp)
library(grid)
library(magick)
library(png)
library(gifski)
library(rlang)
library(rgl)

############ change memory size ############ 

memory.limit(size = 32000) 
memory.limit()

############ draw empty map ############ 
kormap <- readRDS("./data/geodata/gadm36_KOR_1_sp.rds")
kormapfd = fortify(kormap)

############ get data ############ 
private.ed.per.month <- read.csv("./data/월평균_사교육비_ANSI.csv", stringsAsFactors = F, header =T)
private.ed.per.month

############ preprocessing data ############ 

names.eng.kor = data.frame(Eng_name = c("Busan","Chungcheongbuk-do","Chungcheongnam-do", "Daegu","Daejeon",
                                        "Gangwon-do", "Gwangju", "Gyeonggi-do", "Gyeongsangbuk-do",
                                        "Gyeongsangnam-do","Incheon","Jeju","Jeollabuk-do",
                                        "Jeollanam-do","Sejong","Seoul","Ulsan"),
                           Kor_name = c("부산", "충북", "충남", "대구", "대전",
                                        "강원", "광주", "경기", "경북", "경남",
                                        "인천", "제주", "전북", "전남", "세종",
                                        "서울", "울산"),
                           id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9))
# change name
names(private.ed.per.month) = c("Kor_name", "y.2018", "y.2019", "difference" , "elementary", "middle", "high")
private.ed.per.month = private.ed.per.month[1:17,] %>%
  select(Kor_name, y.2019, elementary, middle, high)
#names.eng.kor = names.eng.kor[c(order(names.eng.kor$Kor_name)),]
#private.ed.per.month = private.ed.per.month[c(order(private.ed.per.month$Kor_name)),]
private.ed.per.month = merge(private.ed.per.month, names.eng.kor, by="Kor_name")

############ get long and lat ############ 
kor.long.lat = data.frame()
for(i in names.eng.kor$id){
  #print(i)
  kor.long.lat = rbind(kor.long.lat, kormapfd %>% filter(id == i) %>% summarise(long = mean(long),lat = mean(lat)))
}
kor.long.lat = cbind(kor.long.lat, id= names.eng.kor$id)

############ little change for food visualization ############ 
kor.long.lat[1,]$long =  129.2029
kor.long.lat[1,]$lat =  35.15279
kor.long.lat[3,]$long =  126.9672
kor.long.lat[3,]$lat =  36.42187
kor.long.lat[4,]$long =  128.6075 
kor.long.lat[4,]$lat =  35.73772
kor.long.lat[5,]$long =  127.4657   
kor.long.lat[5,]$lat =  36.28762
kor.long.lat[6,]$long =  128.4957     
kor.long.lat[6,]$lat =  37.67871
kor.long.lat[7,]$long =  126.9552      
kor.long.lat[7,]$lat =  35.10388    
kor.long.lat[8,]$long =  127.3098       
kor.long.lat[8,]$lat =  37.15929  
kor.long.lat[9,]$long =  128.7886        
kor.long.lat[9,]$lat =  36.20868   
kor.long.lat[10,]$long =  128.3478         
kor.long.lat[10,]$lat =  35.24451    
kor.long.lat[11,]$long =  126.5672          
kor.long.lat[11,]$lat =  37.46187   
kor.long.lat[12,]$long =  126.5944           
kor.long.lat[12,]$lat =  33.36291   
kor.long.lat[13,]$long =  127.2419            
kor.long.lat[13,]$lat =  35.64812    
kor.long.lat[14,]$long =  126.9320             
kor.long.lat[14,]$lat =  34.7842      
kor.long.lat[15,]$long =  127.2871              
kor.long.lat[15,]$lat =  36.50711
kor.long.lat[16,]$long =  127.0717               
kor.long.lat[16,]$lat =  37.47785         
kor.long.lat[17,]$long =  129.3102                
kor.long.lat[17,]$lat =  35.48490           


############ Set map as 0 height, Change "kor_score" for other subjects ############
kormapfd_score_helper = data.frame(id = c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9),
                                   y.2019 = rep(0,17)) 
kormapfd = merge(kormapfd,kormapfd_score_helper,by="id")

kor.long.lat.wname = merge(kor.long.lat, names.eng.kor, by="id")
#score_map


############ processing private.ed.per.month ############

# private.ed.per.month
private.ed.per.month.wid = private.ed.per.month %>%
  merge(kor.long.lat.wname, by="Kor_name") %>%
  select(Kor_name, y.2019, long, lat, id=id.x)
private.ed.per.month.wid$long = private.ed.per.month.wid$long - 0.04
private.ed.per.month.wid$lat = private.ed.per.month.wid$lat + 0.04 * 2 * 1.5


private_ed_limit = c(18.10, 45.10)

############ save map size ############ 
empty_map = ggplot() + 
  geom_polygon(data = kormapfd, aes(x=long, y=lat, group=group), fill="white", color="black") +
  theme(legend.position = "none", 
                             axis.line=element_blank(), 
                             axis.text.x=element_blank(), axis.title.x=element_blank(),
                             axis.text.y=element_blank(), axis.title.y=element_blank(),
                             axis.ticks=element_blank(), 
                             panel.background = element_blank())
xlim = ggplot_build(empty_map)$layout$panel_scales_x[[1]]$range$range
ylim = ggplot_build(empty_map)$layout$panel_scales_y[[1]]$range$range

############ plot it as 3D ############
## https://www.rayshader.com/reference/plot_3d.html
## https://github.com/cydalytics/HK_Properties_Price_Distribution/blob/master/HK_Properties_Price_Distribution.R

map_2d = ggplot() + 
  geom_polygon(data = kormapfd, aes(x=long, y=lat, group=group, color=y.2019), fill="white") +
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(data=private.ed.per.month.wid, aes(x=long, y=lat, color=y.2019), size=1.4) + # Points
  scale_colour_gradient(name = 'private education\n fee per month', 
                        limits=private_ed_limit, 
                        low="#FCB9B2", high="#820404") + # Price Density Color
  theme(axis.line=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(), 
        panel.background = element_blank()) # Clean Everything
map_2d
#plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 100)
plot_gg(map_2d, multicore = TRUE, fov = 70, scale = 250, shadow = T,
        zoom = 0.85, phi = 45, theta = 290, sunangle = 135)



############ simple plot with 2d ############ 
ggplot(data = private.ed.per.month.wid, aes(x = Kor_name, y = y.2019, fill=y.2019)) +
  geom_bar(stat='identity') +
  ylab("private educational fee per month") +
  scale_colour_gradient(limits=private_ed_limit, 
                        low="#FCB9B2", high="#820404") 


