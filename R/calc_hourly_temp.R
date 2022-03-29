#' @title Calculate hourly temperature
#' @param data is the dataframe that contains the temp data. Cols should be named 'date_time', 'air_temp_set_1'
#' @example data 
#' p <- calc_hourly_temp(data)
#' @export
#' 
calc_hourly_temp <- function(data){
  # calculate the avg hourly temperature of a field
  dat <- data %>% 
    group_by(FIELD_ID, CROP_SEASON) %>%
    filter(date_time >= seeding_date & date_time <= harvest_date) %>%
    ungroup() %>%
    mutate(date_time = lubridate::ymd_hms(date_time),
           Hour = lubridate::hour(date_time),
           Date = lubridate::date(date_time)) %>%
    group_by(FIELD_ID, CROP_SEASON, station_id, Date, Hour) %>%
    mutate(temp_avg = mean(air_temp_set_1, na.rm = TRUE)) %>% 
    ungroup() %>%
    distinct(across(c(station_id, Date, Hour)), .keep_all = TRUE) %>%
    group_by(FIELD_ID, CROP_SEASON) %>%
    complete(station_id, Date, Hour) %>%
    as.data.frame() %>% ungroup() %>%
    select(-c(air_temp_set_1, date_time))
  
  dat$temp_avg <- zoo::na.approx(dat$temp_avg, na.rm = FALSE)
  return(dat)

}