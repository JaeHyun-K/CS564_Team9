library(cluster)
library(gclus)
library(dplyr)
library(factoextra)
library(NbClust)

academy<-read.csv('student_class_academy_info(2019).csv')
interrupt<-read.csv('interruption_info(2015-2019).csv')
score<-read.csv('entrance_exam_score(2015-2019).csv')

inter<-select(filter(interrupt, year==2019), c("city", "interruptedRatio"))
sco<-select(filter(score, year==2019), c("city", "korean"))
aca<-select(academy, c("city","academyNumRate"))

area<-merge(aca, sco, by="city")
area<-merge(area, inter, by="city")
area<-area[c(9,8,6,12,5,7,11,2,1,17,16,14,13,4,3,15,10),]
rownames(area)<-NULL
area<-area[,-1]
scaled_area<-scale(area)
clust<-NbClust(scaled_area, distance="euclidean", min.nc=2, max.nc = 10, method="complete", index="all")
kc_3<-kmeans(scaled_area, centers =3)
sobj3<-silhouette(kc_3$cluster, dist(scaled_area))
clusplot(scaled_area, kc_3$cluster, color=TRUE, labels=3)
plot(sobj3, col=c(3,5,7))


#Another clustering method
NbClust(scaled_area, method="complete", index="hartigan")
NbClust(scaled_area, method="complete", index="kl")

pamx3<-pam(scaled_area,3)
plot(pamx3)
