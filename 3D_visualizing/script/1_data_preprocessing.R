setwd("C:/Users/youn/Desktop/R_5/3D_visualizing")
getwd()

#install.packages("reshape")
library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)

rm(list=ls())

# data loading

academic_interrupt = read.csv("./data/연도별_시도별 학업중단률(1999년_2019년)_ANSI.csv", header=F)
academic_interrupt
str(academic_interrupt)
academic_interrupt = academic_interrupt %>% select(V1, V2, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28,V29) # 怨좊벑?븰?깮, ?옄?눜
names(academic_interrupt) = c("year", "location", "illness_2011", "maladjustment_2011", "behaviour_2011", "housework_2011",
                              "etc_2011", "illness_2012","housework_2012", "maladjustment_2012", "abroad_2012", "etc_2012", "dropout", "expelled")
academic_interrupt = academic_interrupt[4:374,]
academic_interrupt = academic_interrupt %>% filter(!is.na(year))
academic_interrupt = academic_interrupt %>% mutate(year = as.numeric(year)) %>% 
  mutate(illness_2011 = as.numeric(illness_2011)) %>%
  mutate(maladjustment_2011 = as.numeric(maladjustment_2011)) %>%
  mutate(behaviour_2011 = as.numeric(behaviour_2011)) %>%
  mutate(housework_2011 = as.numeric(housework_2011)) %>%
  mutate(etc_2011 = as.numeric(etc_2011)) %>%
  mutate(illness_2012 = as.numeric(illness_2012)) %>%
  mutate(housework_2012 = as.numeric(housework_2012)) %>%
  mutate(maladjustment_2012 = as.numeric(maladjustment_2012)) %>%
  mutate(abroad_2012 = as.numeric(abroad_2012)) %>%
  mutate(etc_2012 = as.numeric(etc_2012)) %>%
  mutate(dropout = as.numeric(dropout)) %>%
  mutate(expelled = as.numeric(expelled))

academic_interrupt = academic_interrupt %>% mutate(sum = (maladjustment_2011+behaviour_2011+housework_2011+
                                                     etc_2011+housework_2012+maladjustment_2012+etc_2012+
                                                     dropout+expelled))
#write.csv(academic_interrupt, file="./data/high_school_academic_interruption.csv")


###############################################

#a_i_tmp = read.csv("./data/사유별 학업 중단자_ANSI.csv", header=F)
a_i_tmp = read.csv("./data/연도별_시도별 학업중단률(1999년_2019년)_ANSI.csv", header=F)
str(a_i_tmp)
a_i_tmp = a_i_tmp %>% select(V1, V2, V26)
a_i_tmp = a_i_tmp[4:368,]
names(a_i_tmp) = c("year", "location", "num_of_highschool_students")

a_i_tmp = a_i_tmp %>% mutate(year = as.numeric(year)) %>% mutate(num_of_highschool_students = as.numeric(num_of_highschool_students))

tmp = left_join(academic_interrupt, a_i_tmp, by=c("year", "location"))
tmp = mutate(tmp, "interrupted_ratio" = sum/num_of_highschool_students)
tmp
write.csv(tmp, file="./data/high_school_academic_interruption.csv")
