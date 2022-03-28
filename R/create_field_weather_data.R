#' @title Summary of temperature aggregations
#' @param data is the dataframe that contains the temperatures from each station. Temp column should be named "air_temp_set_1"
#' @example data 
#' p <- elev_change(data)
#' @export

create_field_weather_data <- function(data){
  # dat <- field_station %>% left_join(station_temp_data)
  data %>% 
    group_by(field_id) %>%
    mutate(temp_min = min(air_temp_set_1),
           temp_max = max(air_temp_set_1),
           temp_avg = mean(air_temp_set_1)) %>%
    #temp_wt_avg = weighted(temp)) %>%
    select(field_id, Date, Hour, temp_min, temp_max, temp_avg) %>%
    distinct()
}
