library(dplyr)
library(lubridate)

# get_daily_seoul function을 가져옴

get_daily_seoul <- function(start_d, end_d, personal_key){
  
  # 시간을 일별로 묶기
  date_dat <- data.frame(date = seq.Date(lubridate::ymd(start_d), lubridate::ymd(end_d), by = "day"))
  date_dat$date <- format(date_dat$date, "%Y%m%d")
  weather_info <- list()
  
  # for문
  for(i in 1:nrow(date_dat)){
    # url_sub
    url_head <- "http://openAPI.seoul.go.kr:8088/"
    url_back <- "/json/DailyWeatherStation/1/999/"
    
    # 뽑아올 날짜
    date_dat_sub <- date_dat[i,1]
    
    # url
    url <- paste0(url_head, personal_key, url_back, date_dat_sub)
    
    # R로 불러오기
    result <- RCurl::getURL(url, encoding = "gzip")
    processed_json <- jsonlite::fromJSON(result)
    weather_info[[i]] <- processed_json$DailyWeatherStation$row
  }
  weather_information <- data.table::rbindlist(weather_info, fill = TRUE)
  weather_information
}

seoul_daily <- get_daily_seoul("20170101", "20171231", "426175456b6d313134387865684266")
seoul_daily <- seoul_daily %>%
  select(SAWS_OBS_TM, STN_NM, SAWS_TA_AVG, SAWS_HD_AVG, SAWS_WS_AVG, SAWS_RN_SUM)

# seoul_daily의 sido와 car_acci의 sido 차이점 비교
seoul <- unique(seoul_daily$STN_NM) # 남산이 있음 # 중구는 같음 # ex) 동대문
car <- car_acci %>% filter(sido == "서울")
car <- unique(car$gungu) # 남산이 없음 # 중구는 같음 # ex) 동대문구

# seoul 시군구 car_acci와 통일 시키기(car_acci에는 남산구가 없으므로 seoul_daily에서 남산구를 삭제)
# car_acci에는 서울, 경기, 제주가 다 있는데 서울만 가져오기
seoul_daily$STN_NM <- ifelse(seoul_daily$STN_NM == "중구", "중구", paste0(seoul_daily$STN_NM, "구"))
colnames(seoul_daily) <- c("date", "gungu", "temp", "humi", "wind", "rain")
seoul_daily <- seoul_daily %>%
  filter(gungu != "남산구")
seoul_daily$date <- lubridate::ymd(seoul_daily$date)
car_acci <- car_acci %>%
  filter(sido == "서울")

# seoul_daily에 비어있는 날짜 채우기

seoul_daily <- seoul_daily %>%
  arrange(gungu, date) %>%
  group_by(gungu) %>%
  tidyr::complete(date = seq.Date(lubridate::ymd("2017-01-01"), lubridate::ymd("2017-12-31"), by = "day"))
seoul_daily <- data.frame(seoul_daily)


### 일별 평균으로 대체하기
## 내가 선택한 방법은 일단, missing이 있는 data와 없는 data로 나눠서, missing을 채우고 다시 합치는 것
seoul_daily_miss <- seoul_daily %>%
  filter(is.na(seoul_daily$temp))

seoul_daily_obs <- seoul_daily %>%
  filter(!is.na(seoul_daily$temp))

kma_mean <- seoul_daily %>% # 4개 11-03, 11-04, 11-05, 11-06는 25개구 전체 결측값
  group_by(date) %>%
  summarise(m_temp = mean(temp, na.rm = TRUE), m_humi = mean(humi, na.rm = TRUE), m_wind = mean(wind, na.rm = TRUE), m_rain = mean(rain, na.rm = TRUE))
kma_mean <- data.frame(kma_mean)

seoul_daily_miss <- merge(seoul_daily_miss, kma_mean, by = "date")
seoul_daily_miss <- seoul_daily_miss[,-c(3:6)]
colnames(seoul_daily_miss) <- c("date", "gungu", "temp", "humi", "wind", "rain")

seoul <- plyr::join(seoul_daily_miss, seoul_daily_obs, type = "full")

## 날짜가 11-03, 11-04, 11-05, 11-06인 25개구가 모두 결측치를 갖고 있으므로 11-03, 11-04에는 11-02의 데이터를, 11-05, 11-06에는 11-07의 데이터로 대체함
date_obs <- seoul %>% filter(!is.na(temp))

date_1102 <- seoul %>% filter(date == "2017-11-02")
date_1107 <- seoul %>% filter(date == "2017-11-07")

date_03_04 <- seoul %>% filter(date == "2017-11-03" | date == "2017-11-04")
date_05_06 <- seoul %>% filter(date == "2017-11-05" | date == "2017-11-06")

date_02_03_04 <- merge(date_03_04, date_1102, by = "gungu")
date_07_05_06 <- merge(date_05_06, date_1102, by = "gungu")
date_02_03_04 <- date_02_03_04[,c(1:2, 8:11)]
date_07_05_06 <- date_07_05_06[,c(1:2, 8:11)]

date_miss <- plyr::join(date_02_03_04, date_07_05_06, type = "full")
colnames(date_miss) <- c("gungu", "date", "temp", "humi", "wind", "rain")

seoul_final <- plyr::join(date_obs, date_miss, type = "full")
seoul_final <- seoul_final %>%
  arrange(date, gungu)


## car_acci_2017 
car_acci <- read.csv('C:/kma/kma/car_acci_2017.csv', header=TRUE)

car_acci <- car_acci %>%
  group_by(발생년월일시, 주야, 요일, 발생지시도, 발생지시군구, 경도, 위도) %>%
  summarise(occur = sum(사망자수, 사상자수, 중상자수, 경상자수, 부상신고자수))

colnames(car_acci) <- c("ymdt", "day_night", "day_week", "sido", "gungu", "long", "lat", "occur")
car_acci$ymdt <- lubridate::ymd_h(car_acci$ymdt)
car_acci$date <- format(lubridate::date(car_acci$ymdt), "%Y-%m-%d")
car_acci <- car_acci[,c(2:9)]
car_acci<- car_acci %>% filter(sido %in% "서울")
car_acci$date <- lubridate::date(car_acci$date)
car_acci <- data.frame(car_acci)


### car_acci와 seoul 합치기

acci_seoul <- merge(car_acci, seoul, by = c("date", "gungu"))

