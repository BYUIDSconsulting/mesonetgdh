#' @title Calculate hourly temperature
#' @param data is the dataframe that contains the temp data. Cols should be named 'date_time', 'air_temp_set_1'
#' @example data 
#' p <- calc_hourly_temp(data)
#' @export
#' 
calc_hourly_temp <- function(data){
  # calculate the avg hourly temperature of a field
  require(lubridate)
  data %>% 
    mutate(date_time = ymd_hms(date_time),
           Hour = paste0(hour(date_time), ":00:00"),
           Date = date(date_time)) %>%
    group_by(station_id, Date, Hour) %>%
    summarize(temp_avg = mean(air_temp_set_1))
}