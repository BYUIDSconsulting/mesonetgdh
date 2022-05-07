#' @title Request the temperature data from MesoWest, but fast
#' @param df is the fields_stations dataset derived from the find_closest_station() function. 
#' @examples p <- pull_temp_all(df)
global_resp <- NULL
pull_temp_all <- function(df) {
  
  stations_date <- df %>% 
    select(station_id, harvest_date, seeding_date)

  base_url <- 'https://api.synopticdata.com/v2'

  df_request <- NULL
  
  start_date <- paste0(gsub('-', '', min(stations_date$seeding_date)), '0000')
  end_date <- paste0(gsub('-', '', min(stations_date$harvest_date)), '0000')
  stations_str <- stringr::str_c((df$station_id %>% unique()),collapse=",")
  req <- paste0('/stations/timeseries?stid=', stations_str
                ,'&vars=air_temp&fields=stid,longitude,latitude,elevation'
                ,'&units=english,temp|F'
                ,'&start=', start_date, '&end=', end_date)
  req_url <- paste0(base_url, req, '&token=', token)
  response <- jsonlite::fromJSON(req_url, simplifyMatrix = TRUE)
  
  id <- response$STATION$STID
  
  resp <- data.frame(lapply(response$STATION$OBSERVATIONS, unlist)) 
  print(tibble(resp) %>% head(5))
  df_request <- rbind(df_request, resp) 
  df_request <- df_request %>% 
    mutate(year=lubridate::year(date_time))
  
  return(df_request)
  
}
