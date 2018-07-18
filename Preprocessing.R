library(dplyr)
library(lubridate)

# 방재기상관측 2012 ~ 2017 데이터 불러오기 (종로구가 따로 없어서 북악산(422)를 종로구로 함, 동작구가 따로 없어서 현충원(889)를 동작구로 함)

bangjae <- read.csv("C:/kma/kma/bangjae_data.csv", header = TRUE)

bangjae_2017 <- bangjae %>%
  filter(lubridate::year(date) == "2017") %>%
  arrange(date)
bangjae_2017$date <- as.Date(bangjae_2017$date)

# bangjae_2017에 비어있는 날짜 채우기

bangjae_2017 <- bangjae_2017 %>%
  arrange(gungu, date) %>%
  group_by(gungu) %>%
  tidyr::complete(date = seq.Date(lubridate::ymd("2017-01-01"), lubridate::ymd("2017-12-31"), by = "day"))
bangjae_2017 <- data.frame(bangjae_2017)
bangjae_2017 <- bangjae_2017[,-3]

# 우선 NA가 얼마나 있는지 확인해보자

table(is.na(bangjae_2017$mean_temp)) # FALSE : 9077, TRUE : 48
table(is.na(bangjae_2017$low_temp)) # FALSE : 9080, TRUE : 45
table(is.na(bangjae_2017$high_temp)) # FALSE : 9080, TRUE : 45
table(is.na(bangjae_2017$rain)) # FALSE : 9119, TRUE : 6
table(is.na(bangjae_2017$wind)) # FALSE : 9083, TRUE : 42

# 방재 데이터 2012 ~ 2016년 뽑아오기. 그래서 일별로 다 평균낸 것
bangjae_other <- bangjae %>%
  filter(! (lubridate::year(date) %in% 2017)) %>%
  arrange(date) %>%
  select(date, mean_temp, low_temp, high_temp, rain, wind, gungu)

bangjae_other_mean <- bangjae_other %>%
  group_by(gungu, strftime(date, "%m-%d")) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), low_temp = mean(low_temp, na.rm = TRUE), high_temp = mean(high_temp, na.rm = TRUE), rain = mean(rain, na.rm = TRUE), wind = mean(wind, na.rm = TRUE))
bangjae_other_mean <- data.frame(bangjae_other_mean)
colnames(bangjae_other_mean) <- c("gungu", "day", "mean_temp", "low_temp", "high_temp", "rain", "wind")

# 내가 선택한 방법은 일단, missing이 있는 data와 없는 data로 나눠서, missing을 채우고 다시 합치는 것
# mean_temp 

bangjae_2017_miss_mean_temp <- bangjae_2017 %>%
  filter(is.na(bangjae_2017$mean_temp)) %>%
  select(gungu, date, mean_temp)
bangjae_2017_miss_mean_temp$day <- strftime(bangjae_2017_miss_mean_temp$date, "%m-%d")

bangjae_2017_obs_mean_temp <- bangjae_2017 %>%
  filter(!is.na(bangjae_2017$mean_temp)) %>%
  select(gungu, date, mean_temp)

mean_temp_fill <- merge(bangjae_2017_miss_mean_temp, bangjae_other_mean, by = c("gungu", "day"))
mean_temp_fill <- mean_temp_fill[,c(1,3,5)]
colnames(mean_temp_fill) <- c("gungu", "date", "mean_temp")

mean_temp <- plyr::join(bangjae_2017_obs_mean_temp, mean_temp_fill, type = "full")

# low_temp 

bangjae_2017_miss_low_temp <- bangjae_2017 %>%
  filter(is.na(bangjae_2017$low_temp)) %>%
  select(gungu, date, low_temp)
bangjae_2017_miss_low_temp$day <- strftime(bangjae_2017_miss_low_temp$date, "%m-%d")

bangjae_2017_obs_low_temp <- bangjae_2017 %>%
  filter(!is.na(bangjae_2017$low_temp)) %>%
  select(gungu, date, low_temp)

low_temp_fill <- merge(bangjae_2017_miss_low_temp, bangjae_other_mean, by = c("gungu", "day"))
low_temp_fill <- low_temp_fill[,c(1,3,6)]
colnames(low_temp_fill) <- c("gungu", "date", "low_temp")

low_temp <- plyr::join(bangjae_2017_obs_low_temp, low_temp_fill, type = "full")

# high_temp

bangjae_2017_miss_high_temp <- bangjae_2017 %>%
  filter(is.na(bangjae_2017$high_temp)) %>%
  select(gungu, date, high_temp)
bangjae_2017_miss_high_temp$day <- strftime(bangjae_2017_miss_high_temp$date, "%m-%d")

bangjae_2017_obs_high_temp <- bangjae_2017 %>%
  filter(!is.na(bangjae_2017$high_temp)) %>%
  select(gungu, date, high_temp)

high_temp_fill <- merge(bangjae_2017_miss_high_temp, bangjae_other_mean, by = c("gungu", "day"))
high_temp_fill <- high_temp_fill[,c(1,3,7)]
colnames(high_temp_fill) <- c("gungu", "date", "high_temp")

high_temp <- plyr::join(bangjae_2017_obs_high_temp, high_temp_fill, type = "full")

# wind

bangjae_2017_miss_wind <- bangjae_2017 %>%
  filter(is.na(bangjae_2017$wind)) %>%
  select(gungu, date, wind)
bangjae_2017_miss_wind$day <- strftime(bangjae_2017_miss_wind$date, "%m-%d")

bangjae_2017_obs_wind <- bangjae_2017 %>%
  filter(!is.na(bangjae_2017$wind)) %>%
  select(gungu, date, wind)

wind_fill <- merge(bangjae_2017_miss_wind, bangjae_other_mean, by = c("gungu", "day"))
wind_fill <- wind_fill[,c(1,3,9)]
colnames(wind_fill) <- c("gungu", "date", "wind")

wind <- plyr::join(bangjae_2017_obs_wind, wind_fill, type = "full")

# rain

bangjae_2017_miss_rain <- bangjae_2017 %>%
  filter(is.na(bangjae_2017$rain)) %>%
  select(gungu, date, rain)
bangjae_2017_miss_rain$day <- strftime(bangjae_2017_miss_rain$date, "%m-%d")

bangjae_2017_obs_rain <- bangjae_2017 %>%
  filter(!is.na(bangjae_2017$rain)) %>%
  select(gungu, date, rain)

rain_fill <- merge(bangjae_2017_miss_rain, bangjae_other_mean, by = c("gungu", "day"))
rain_fill <- rain_fill[,c(1,3,8)]
colnames(rain_fill) <- c("gungu", "date", "rain")

rain <- plyr::join(bangjae_2017_obs_rain, rain_fill, type = "full")

# 합치기

a <- merge(mean_temp, low_temp, by = c("gungu", "date"))
b <- merge(a, high_temp, by = c("gungu", "date"))
c <- merge(b, wind, by = c("gungu", "date"))
d <- merge(c, rain, by = c("gungu", "date"))
seoul_weather <- d %>% arrange(date, gungu)

