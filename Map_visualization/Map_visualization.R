
#To generate the codes, you should have shp(shape) file.
#We downloaded them here: http://www.gisdeveloper.co.kr/?p=2332
#We used shp file of 2017, because uploaded data after 2018 cause error in R.

#All of these data, including exam scores, and
#results(plot) are also can be downloaded from our github repository.

############### Visualization of entrance exam score ###############
###############             in Seoul map             ###############

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(gpclib)
library(readxl)
library(dplyr)

SEscore <- read_excel("exam_score_in_Seoul.xlsx")

map <- shapefile("SIG_201703/TL_SCCO_SIG.shp")
map <- fortify(map, region='SIG_CD')
map$id <- as.numeric(map$id)
seoul <- map[map$id <= 11740,]
seoul_map <- merge(seoul, SEscore, by='id')

plot1 <- ggplot() + geom_polygon(data=seoul_map, aes(x=long, y=lat, group=group.x, fill=score))

plot1 + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) # Result 1

ggplot() + geom_polygon(data=seoul_map, aes(x=long, y=lat, group=group.x, fill=group.y)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) # Result 2

SEscore %>% group_by(group) %>% summarise(mean_score = mean(score))


############### Visualization of entrance exam score ###############
###############             in Korea map             ###############

examScore <- read_excel("exam_score_by_regions.xlsx")
map <- shapefile("CS564_Team9/Map_visualization/CTPRVN_201703/TL_SCCO_CTPRVN.shp")
examScore2019 <- cbind(examScore[1], examScore[13])
examScore2019 <- cbind(examScore2019, examScore[11]-examScore[12])
colnames(examScore2019) <- list("region", "id", "value")
View(examScore2019)

newmap <- fortify(map, region='CTPRVN_CD')
korea <- merge(newmap, examScore2019, by='id')

ggplot()+geom_polygon(data=korea, aes(x=long, y=lat, group=group, fill=value))+
  scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") +
  theme_bw() # Result 3
