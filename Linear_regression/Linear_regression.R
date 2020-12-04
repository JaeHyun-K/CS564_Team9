learning_data = read.csv("Data/student_class_academy_info(2019).csv")
score_data = read.csv("Data/entrance_exam_score(2015-2019).csv")

library(dplyr)
score2019_data = score_data[score_data$year==2019,] %>% select(1,3,6,7)
score2019_data = score2019_data[c(1,2,3,4,5,6,7,17,8,9,10,11,12,13,14,15,16),]
score2019_data

learning_data[,"korean_score"] = score2019_data[1:17,2]
learning_data[,"mathA_score"] = score2019_data[1:17,3]
learning_data[,"mathB_score"] = score2019_data[1:17,4]
learning_data[,"total_score"] =
  score2019_data[1:17,2]+score2019_data[1:17,3]+score2019_data[1:17,4]
learning_data
learning_data[,3:10] = scale(learning_data[,3:10])
learning_data

library(coefplot)
public_fit = lm(korean_score ~ classroomNumRate + teacherNumRate,
                data=learning_data)
summary(public_fit)

private_fit = lm(korean_score ~ academyNumRate + academyPayRate,
                data=learning_data)
summary(private_fit)

academy_pay_fit = lm(korean_score ~ academyPayRate, data=learning_data)
with(learning_data, plot(academyPayRate,korean_score, pch=21, bg='cyan'))
lines(learning_data$academyPayRate, academy_pay_fit$fitted.values, col='red')
