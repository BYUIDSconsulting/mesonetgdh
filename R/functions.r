#' @title Get Elevations
#' @param df is the dataframe as output from the 'all_fields' function
#' @example df = makeaneexample
#' get_elev(df)
#' @export

get_elev <-  function(df){
  
  # define coordinate columns
  coords <- data.frame(x=df$lon, y=df$lat)
  # get elevation from source
  elev <- elevatr::get_elev_point(coords, unit = 'feet', src='epqs', prj="EPSG:4326")
  # extract elevation column
  elevation <- elev[[1]]
  
  return(elevation)
}
#' @title Create Temporary Dataframe to Calculate GDHs 
#' @param temp_data is field_weather_data from 'create_field_weather_data'
#' @param database_data is the field information, including crop and seeding date
#' @param gdh_data is the information needed to calculate GDHs, including base and upper temps
#' @export

combine_df <- function(temp_data, database_data, gdh_data) {
  new_data <- temp_data %>% 
    left_join(database_data) %>%
    left_join(gdh_data, by = c("CROP_NAME" = "CROP")) %>%
    select(FIELD_ID, CROP_NAME, Base_Fahrenheit, 
           Base_Celsius, Upper_Fahrenheit, 
           Upper_Celsius, air_temp_c, date_time, START_DATE, CROP_SEASON) %>%
    mutate(date_time = ymd_hms(date_time),
           START_DATE = ymd_hms(START_DATE)) %>%
    filter(CROP_SEASON == year(date_time))
  
  
  return(new_data)
}

#' @title Calculate Growing Degree Hours
#' @param data is the dataframe that is output from 'combine_df' function
#' @example data = temp_df
#' growing_degrees <- calc_gdh(data)
#' print(growing_degrees)
#' @export

calc_gdh <- function(data, field = NULL){
  
  if (!is.null(field)) {
    data %<>% filter(field_id == field) 
  }
  
  temp_data <- data %>%
    mutate(gdh = air_temp_f - Base_Fahrenheit,
           gdh = case_when(gdh < 0 ~ 0,
                           air_temp_f > Upper_Fahrenheit ~ Upper_Fahrenheit - Base_Fahrenheit, 
                           TRUE ~ gdh),
           total_gdh = cumsum(ifelse(is.na(gdh), 0, gdh)))
  
  if (!is.null(field)) {
    
    total_gdhs <- temp_data %>% 
      rbind(temp_data[nrow(data),]) %>% 
      select(FIELD_ID, CROP, seeding_date, date_time, gdh, total_gdh)
    
  } else{
    
    total_gdhs <- temp_data %>% 
      group_by(field_id) %>% 
      slice_max(total_gdh)
  }
  return(total_gdhs)
}


#' @title Get state abbreviations
#' @param df is the dataframe that includes the coordinate columns. Make sure to rename columns to 'lat', 'lon'
#' @example df 
#' df$state <- get_state(df)
#' @export
get_state <- function(df){
  if ('lat' %in% colnames(df) | 'lon' %in% colnames(df)) {
    suppressMessages(df$state <- maps::map.where('state', df$lon, df$lat))
    df$state <- state.abb[match(stringr::str_to_title(df$state), state.name)]
    return(df$state)
  } else {
    message('Warning message:\nMissing lat, lon columns. \nPlease rename coordinate columns to "lat", "lon"')
  }
}