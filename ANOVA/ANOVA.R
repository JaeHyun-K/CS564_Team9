## ANOVA on ¼ö´É score by regions in 2019
score <- read.csv('entrance_exam_score(2019).csv', header = T)

# We can conduct ANOVA test with only these information thanks to "rpsychi" package!
# install.packages('rpsychi')
library(rpsychi)
with(score, ind.oneway.second(score$mean, score$sd, score$n))