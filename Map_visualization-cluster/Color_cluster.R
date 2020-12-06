library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

korea<-read.csv('KR_NO_ISLE_euc_kr.csv')
map <- shapefile("TL_SCCO_CTPRVN.shp")
cluster<-read.csv('cluster.csv')
korea_map<-fortify(map, region='CTPRVN_CD')
mapping<-merge(korea_map, cluster, by="id")
mapped<-merge(korea, cluster, by="id")

#Version with island: Thanks to http://www.gisdeveloper.co.kr/?p=2332 !!
ggplot()+geom_polygon(data=mapping, aes(x=long, y=lat, group=group, fill=Group))+
  scale_fill_gradientn(colours = c(7,3,5), space="Lab", guide = "legend" ,breaks=c(1,2,3))+
  theme_bw()

#Version with out island: Thanks to Tony!!
ggplot()+geom_polygon(data=mapped, aes(x=long, y=lat, group=id, fill=Group))+
  scale_fill_gradientn(colours = c(7,3,5), space="Lab", guide = "legend" ,breaks=c(1,2,3))+
  theme_bw()

