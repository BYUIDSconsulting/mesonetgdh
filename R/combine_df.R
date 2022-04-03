#' @title Create Temporary Dataframe to Calculate GDHs 
#' @param temp_data is field_weather_data from 'create_field_weather_data'
#' @param database_data is the field information, including crop and seeding date
#' @param gdh_data is the information needed to calculate GDHs, including base and upper temps
#' @export

combine_df <- function(temp_data, database_data, gdh_data) {
  new_data <- temp_data %>% 
    left_join(database_data) %>%
    left_join(gdh_data, by = c("CROP_NAME" = "CROP")) %>%
    select(FIELD_ID, CROP_NAME, Base_Fahrenheit, Upper_Fahrenheit, 
           temp_combined_avg, temp_wt_avg, Date, Hour, seeding_date,
           harvest_date, CROP_SEASON) %>%
    filter(CROP_SEASON == lubridate::year(Date))
  
  
  return(new_data)
}