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
