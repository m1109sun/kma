library(dplyr)
library(lubridate)
car_acci <- read.csv('C:/kma/kma/car_acci_2017.csv', header=TRUE)
car_acci <- car_acci[,c(2, 4:12, 26:27)]

car_acci <- car_acci %>%
  group_by(발생년월일시, 주야, 요일, 발생지시도, 발생지시군구, 경도, 위도) %>%
  summarise(occur = sum(사망자수, 사상자수, 중상자수, 경상자수, 부상신고자수))

colnames(car_acci) <- c("ymdt", "day_night", "day_week", "sido", "gungu", "long", "lat", "occur")
car_acci$ymdt <- lubridate::ymd_h(car_acci$ymdt)
car_acci$date <- format(lubridate::date(car_acci$ymdt), "%Y%m%d")
car_acci <- car_acci[,c(2:9)]
car_acci <- car_acci %>% filter(sido %in% c("서울", "경기", "제주"))
car_acci <- data.frame(car_acci)

