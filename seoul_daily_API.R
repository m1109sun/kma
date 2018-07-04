library(RCurl)
library(jsonlite)

# start_d : 불러올 시작일 ex) 20100101
# end_d : 불러올 종료일 ex) 20100102
# personal_key : 개인 API key
# sample url : http://openAPI.seoul.go.kr:8088/(인증키)/xml/DailyWeatherStation/1/5/20130315

# 일간 자료 불러오기

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

Example <- get_daily_seoul("20160101", "20160110", "426175456b6d313134387865684266")
