# randomForest

library(randomForest)
library(tidyvers)
library(data.table)

final_car <- fread("final_data.csv")
final_car1 <- final_car %>% 
  dplyr::select(gu,sex, not_workingday, humi, meantemp, age_group, sum_tel, rain_group, diff) 

table(final_car$sum_tel) # sum_tel 개수 센 것

final_car_a <- final_car1 %>% # 범주 나눔, -Inf ~ 9 : a / 10 ~ 29 : b / 30 ~ INF : c
  dplyr::mutate(call_group = cut(sum_tel, c(-Inf, 9, 29, Inf), c("a", "b", "c")))

table(final_car_a$call_group)

str(final_car_a)
final_car_a$gu <- as.factor(final_car_a$gu)
final_car_a$sex <- as.factor(final_car_a$sex)
final_car_a$not_workingday <- as.factor(final_car_a$not_workingday)
final_car_a$age_group <- as.factor(final_car_a$age_group)
final_car_a$rain_group <- as.factor(final_car_a$rain_group)

ind = sample(2, nrow(final_car_a), replace = TRUE, prob = c(0.7, 0.3))
train_car = final_car_a[ind == 1,]
test_car = final_car_a[ind == 2,]

test_car <- test_car %>% 
  dplyr::select(-sum_tel)

set.seed(123)
train_x <- train_car[, c(1:6, 8:9)] # 설명변수들
train_y <- train_car[, 10] # call_group(a, b, c)로 이루어져 있는 애들
fmodel <- randomForest(x = train_x, y = train_y, ntree = 100, nodesize = 3, importance = T)

irisPred <- predict(fmodel, newdata = test_car) 
CM <- table(irisPred, test_car$call_group) # 행 : 실제 test set의 group / 열 : prediction한 애들의 그룹
table(test_car$call_group) # prediction 했을 때, a : 9321 / b : 10446 / c : 611
accuracy <- sum(diag(CM)) / sum(CM) # 72% 나옴

# LDA, QDA

library(MASS)
lda <- lda(formula = call_group ~ age_group + gu + sex + meantemp + humi + not_workingday + diff , data = train_car)
pre <- predict(lda, data = train_car, newdata = test_car)
pre$class
CM <- table(pre$class, test_car$call_group)
table(test_car$call_group) # prediction 했을 때, a : 9280 / b : 10475 / c : 567
accuracy <- sum(diag(CM)) / sum(CM) # 68% 나옴


qda <- qda(formula = call_group ~ age_group + sex + meantemp + humi + not_workingday + diff, data = train_car)
pre <- predict(qda, data = train_car, newdata = test_car)
pre$class
CM <- table(pre$class, test_car$call_group)
table(test_car$call_group) # prediction 했을 때, a : 9280 / b : 10475 / c : 567
accuracy <- sum(diag(CM)) / sum(CM) # 58% 나옴


