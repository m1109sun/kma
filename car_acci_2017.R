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
car_acci <- car_acci %>%
  filter(sido == "서울")

# car_acci에서 sido가 서울인 것만 뽑아서 seoul_daily와 합치기
car_kma <- merge(car_acci, seoul_daily, by = c("date", "gungu"))








