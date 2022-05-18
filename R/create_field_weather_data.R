#' @title Summary of temperature aggregations
#' @param data is the dataframe that contains the temperatures from each station. Temp column should be named "temp_avg
#' @examples data 
#' p <- elev_change(data)
#' @export
create_field_weather_data <- function(data){
  dat1 <- weighted(data)
  
  data %>% 
    left_join(dat1, by = c("FIELD_ID", "station_id", "Date", "Hour")) %>%
    group_by(FIELD_ID, Date, Hour) %>%
    summarize(temp_min = min(temp_avg, na.rm = TRUE),
              temp_max = max(temp_avg, na.rm = TRUE),
              temp_combined_avg = mean(temp_avg, na.rm = TRUE),
              temp_wt_avg = mean(wt_temp, na.rm = TRUE)) %>%
    select(FIELD_ID, Date, Hour, temp_min, temp_max, temp_combined_avg, temp_wt_avg) %>%
    distinct()
}


