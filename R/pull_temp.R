#' @title Request the temperature data from MesoWest
#' @param df is the fields_stations dataset derived from the find_closest_station() function. 
#' @examples p <- pull_temp(df)
#' @export
pull_temp <- function(df) {
  
  stations_date <- df %>% 
    select(station_id, harvest_date, seeding_date)
  
  print(paste0("Number of rows to get temp: ", nrow(stations_date)))
  print(paste0("Number of distinct stations to get temp: ", nrow(stations_date %>% distinct(station_id))))
  
  base_url <- 'https://api.synopticdata.com/v2'

  df_request <- NULL
  for (i in 1:nrow(stations_date)) {
    start_date <- paste0(gsub('-', '', stations_date$seeding_date[i]), '0000')
    end_date <- paste0(gsub('-', '', stations_date$harvest_date[i]), '0000')
    station <- stations_date$station_id[i]

    req <- paste0('/stations/timeseries?stid=', station
                  ,'&vars=air_temp&fields=stid,longitude,latitude,elevation'
                  ,'&units=english,temp|F'
                  ,'&start=', start_date, '&end=', end_date)
    req_url <- paste0(base_url, req, '&token=', token)
    response <- jsonlite::fromJSON(req_url, simplifyMatrix = TRUE)
    id <- response$STATION$STID

    resp <- data.frame(lapply(response$STATION$OBSERVATIONS, unlist)) %>%
      mutate(station_id = id)
    df_request <- rbind(df_request, resp) 
  }
  
  df_request <- df_request %>% 
    mutate(year=lubridate::year(date_time))
  
  return(df_request)
  
}

#' @title Request the hourly temperature data from MesoWest and process it
#' @param fields_stations is the fields_stations dataset derived from the find_closest_station() function. 
#' @examples station_temp_data <- pull_temp_full(fields_stations)
#' @export
pull_temp_full <- function(fields_stations){
  # Do not use the pull_temp_all as it is buggy
  fs_yearly <- fields_stations %>%
    group_by(station_id, CROP_SEASON) %>%
    summarise(seeding_date=min(seeding_date), harvest_date=max(harvest_date))
  temperature <- pull_temp(fs_yearly)
  print("Combining stations")
  print(paste0(nrow(fields_stations), " rows in fields_stations"))
  print(paste0(nrow(temperature), " rows in temperature"))
  df_comb <- fields_stations %>%
    left_join(temperature, by=c('station_id'='station_id', 'CROP_SEASON'='year'))
  print("Calculating station temp")
  station_temp_data <- calc_hourly_temp(df_comb)
  return(station_temp_data)
}