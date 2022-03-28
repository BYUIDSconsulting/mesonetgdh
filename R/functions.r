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

#' @title Calculate latitudinal distance
#' @param data is the dataframe that contains the latitude. Column should be named "lat"
#' @example data 
#' p <- lat_dist(data)
#' @export

lat_dist <- function(data){ 
  require(geosphere)
  
  data$dummy <- 0
  
  data %>%
    group_by(FIELD_ID, station_id) %>%
    summarize(lat_dist = distHaversine(matrix(c(dummy, field_lat), ncol = 2),
                                       matrix(c(dummy, station_lat), ncol = 2))) %>%
    unique()
}

#' @title Calculate longitudinal distance
#' @param data is the dataframe that contains the latitude. Column should be named "lon"
#' @example data 
#' p <- lon_dist(data)
#' @export

lon_dist <- function(data){
  require(geosphere)
  
  data$dummy <- 0
  
  data %>% 
    group_by(FIELD_ID, station_id) %>%
    summarize(long_dist = distHaversine(matrix(c(field_lon, dummy), ncol = 2), 
                                        matrix(c(station_lon, dummy), ncol = 2))) %>%
    unique()
}

#' @title Calculate distance
#' @param data is the dataframe that contains the latitude and longitude. Columns should be named "lat" and "lon"
#' @example data 
#' p <- total_dist(data)
#' @export

total_dist <- function(data){
  require(geosphere)
  
  data %>% 
    group_by(FIELD_ID, station_id) %>%
    summarize(total_dist = distHaversine(matrix(c(field_lon, field_lat), ncol = 2),
                                         matrix(c(station_lon, station_lat), ncol = 2))) %>%
    unique()
}

#' @title Calculate elevation difference
#' @param data is the dataframe that contains the elevations. Columns should be named "field_elev" and "station_elev"
#' @example data 
#' p <- elev_change(data)
#' @export

elev_change <- function(data){
  
  data %>% group_by(FIELD_ID, station_id) %>%
    summarize(elev_dif = abs(field_elev - station_elev)) %>%
    unique()
}

#' @title Summary of temperature aggregations
#' @param data is the dataframe that contains the temperatures from each station. Temp column should be named "air_temp_f"
#' @example data 
#' p <- elev_change(data)
#' @export

create_field_weather_data <- function(data){
  # dat <- field_station %>% left_join(station_temp_data)
  data %>% 
    group_by(field_id) %>%
    mutate(temp_min = min(air_temp_f),
           temp_max = max(air_temp_f),
           temp_avg = mean(air_temp_set_1)) %>%
    #temp_wt_avg = weighted(temp)) %>%
    select(field_id, Date, Hour, temp_min, temp_max, temp_avg) %>%
    distinct()
}