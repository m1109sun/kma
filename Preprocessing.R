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
  dplyr::select(SAWS_OBS_TM, STN_NM, SAWS_TA_AVG, SAWS_TA_MIN, SAWS_TA_MAX, SAWS_WS_AVG, SAWS_RN_SUM)

# seoul_daily의 sido와 car_acci의 sido 차이점 비교

seoul <- unique(seoul_daily$STN_NM) # 남산이 있음 # 중구는 같음 # ex) 동대문
car <- car_acci %>% filter(sido == "서울")
car <- unique(car$gungu) # 남산이 없음 # 중구는 같음 # ex) 동대문구

# seoul 시군구 car_acci와 통일 시키기(car_acci에는 남산구가 없으므로 seoul_daily에서 남산구를 삭제)
# car_acci에는 서울, 경기, 제주가 다 있는데 서울만 가져오기

seoul_daily$STN_NM <- ifelse(seoul_daily$STN_NM == "중구", "중구", paste0(seoul_daily$STN_NM, "구"))
colnames(seoul_daily) <- c("date", "gungu", "mean_temp", "low_temp", "high_temp", "wind", "rain")
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

# 방재기상관측 2012 ~ 2017 데이터 불러오기 (종로구가 따로 없어서 북악산(422)를 종로구로 함, 동작구가 따로 없어서 현충원(889)를 동작구로 함)

bangjae <- read.csv("C:/kma/kma/bangjae_data.csv", header = TRUE)

# 내가 선택한 방법은 일단, missing이 있는 data와 없는 data로 나눠서, missing을 채우고 다시 합치는 것

seoul_daily_miss <- seoul_daily %>%
  filter(is.na(seoul_daily$mean_temp))
seoul_daily_miss <- seoul_daily_miss[,c(1:2)]
seoul_daily_obs <- seoul_daily %>%
  filter(!is.na(seoul_daily$mean_temp))

# missing이 있는 data를 기상청 방재 data와 합침

seoul_daily_miss <- merge(seoul_daily_miss, bangjae, by = c("gungu", "date"))
table(is.na(seoul_daily_miss$mean_temp)) # FALSE : 882, TRUE : 5
table(is.na(seoul_daily_miss$low_temp)) # FALSE : 882, TRUE : 5
table(is.na(seoul_daily_miss$high_temp)) # FALSE : 882, TRUE : 5
table(is.na(seoul_daily_miss$rain)) # FALSE : 887
table(is.na(seoul_daily_miss$wind)) # FALSE : 882, TRUE : 5

# 방재 data로 missing을 채우고 난 후의 data에서 또 missing 값과 아닌 값 뽑아내기

seoul_daily_missfill <- seoul_daily_miss %>%
  filter(!is.na(seoul_daily_miss$mean_temp))
seoul_daily_missfill <- seoul_daily_missfill[,c(1:2, 4:8)]

seoul_daily_miss_miss <- seoul_daily_miss %>% # missing이 5갠데, 강서구이고 date가 09-28, 09-29, 09-30, 10-01, 12-05임
  filter(is.na(seoul_daily_miss$mean_temp))
seoul_daily_miss_miss <- seoul_daily_miss_miss[,c(1:3, 7)]
seoul_daily_miss_miss$mon_day <- substr(seoul_daily_miss_miss$date, 6, 10)

# 우리가 불러들인 방재 데이터가 2012 ~ 2017이니까 저 날짜에 해당하는 다른 년도 데이터들을 불러오자

bangjae$month <- month(bangjae$date)
bangjae$day <- day(bangjae$date)
  
miss <- bangjae %>%
  filter(gungu == "강서구") %>%
  filter((month == "9" & (day == "28" | day == "29" | day == "30")) | (month == "10" & day == "1") | (month == "12" & day == "5"))
miss$mon_day <- substr(miss$date, 6, 10)

miss_avg <- miss %>%
  group_by(mon_day) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), low_temp = mean(low_temp, na.rm = TRUE), high_temp = mean(high_temp, na.rm = TRUE), wind = mean(wind, na.rm = TRUE))
miss_avg <- data.frame(miss_avg)

# 5개의 남은 missing 값을 채워보자

seoul_daily_miss_miss <- merge(seoul_daily_miss_miss, miss_avg, by = "mon_day")
seoul_daily_miss_miss <- seoul_daily_miss_miss[,c(2:3, 5:9)]

# 다 합쳐보자, 최종 seoul 기온 data

seoul <- plyr::join(seoul_daily_miss_miss, seoul_daily_missfill, type = "full")
seoul_daily <- plyr::join(seoul, seoul_daily_obs, type = "full")
seoul_daily <- seoul_final %>%
  arrange(gungu, date)

# car_acci_2017 

car_acci <- read.csv('C:/kma/kma/car_acci_2017.csv', header = TRUE)

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

acci_seoul <- merge(car_acci, seoul_daily, by = c("date", "gungu"))

# plot 그리기 위해서 factor level을 월, 화, 수, 목, 금, 토, 일 순서로 변경

acci_seoul$day_week <- factor(acci_seoul$day_week, levels = c("월", "화", "수", "목", "금", "토", "일"))


