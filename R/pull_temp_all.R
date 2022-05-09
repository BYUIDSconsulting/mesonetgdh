#' @title Request the temperature data from MesoWest, but fast
#' @param df is the fields_stations dataset derived from the find_closest_station() function. 
#' @export
global_resp <- NULL
pull_temp_all <- function(df) {
  
  stations_date <- df %>% 
    select(station_id, harvest_date, seeding_date)

  base_url <- 'https://api.synopticdata.com/v2'

  df_request <- NULL
  
  # Download data for all stations
  start_date <- paste0(gsub('-', '', min(stations_date$seeding_date)), '0000')
  end_date <- paste0(gsub('-', '', min(stations_date$harvest_date)), '0000')
  uniq_stations <- df$station_id %>% unique()
  stations_str <- stringr::str_c(uniq_stations,collapse=",")
  req <- paste0('/stations/timeseries?stid=', stations_str
                ,'&vars=air_temp&fields=stid,longitude,latitude,elevation'
                ,'&units=english,temp|F'
                ,'&start=', start_date, '&end=', end_date)
  req_url <- paste0(base_url, req, '&token=', token)
  response <- jsonlite::fromJSON(req_url, simplifyMatrix = TRUE)
  
  # Loop through each station and add its info to the data frame
  for(i in 1:length(uniq_stations)){
    id <- uniq_stations[i]
    resp <- data.frame(lapply(response$STATION[i,]$OBSERVATIONS, unlist)) %>%
      mutate(station_id = id)
    df_request <- rbind(df_request, resp)
  }

  df_request <- df_request %>% 
    mutate(year=lubridate::year(date_time))  
  return(df_request)
  
  
  
}
