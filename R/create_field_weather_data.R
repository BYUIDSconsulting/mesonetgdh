#' @title Summary of temperature aggregations
#' @param data is the dataframe that contains the temperatures from each station. Temp column should be named "air_temp_set_1"
#' @example data 
#' p <- elev_change(data)
#' @export

create_field_weather_data <- function(data){
  # dat <- field_station %>% left_join(station_temp_data)
  data %>% 
    group_by(FIELD_ID, Date, Hour) %>%
    mutate(temp_min = min(temp_avg),
           temp_max = max(temp_avg),
           temp_avg = mean(temp_avg)) %>%
    #temp_wt_avg = weighted(temp)) %>%
    select(FIELD_ID, Date, Hour, temp_min, temp_max, temp_avg) %>%
    distinct()
}


