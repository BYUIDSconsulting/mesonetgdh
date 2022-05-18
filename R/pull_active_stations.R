#' @title Pull active stations from MesoWest API
#' @param df is 'fields' dataset, including FIELD id,lat,lon,elevation,state, CROP name,season, seeding and harvest dates
#' @param key is the API key for MesoWest
#' @examples p <- pull_active_stations(df, key)
#' @export
pull_active_stations <- function(df, key) {
  
  ################################################
  # Authentication token
  base_url <- 'https://api.synopticdata.com/v2'
    
  # request token using API key
  auth <- paste0(base_url, '/auth?apikey=', key)
  token <<- jsonlite::fromJSON(auth)$TOKEN
  ################################################
  # Use unique states to pull relevant temperature data
  unique_states <- paste((df$state %>% unique()), collapse=',')

  # Combine the URL
  req <- paste0('/stations/metadata?state=', unique_states,
                '&vars=air_temp&fields=stid,longitude,latitude,elevation,shortname,period_of_record&status=active')
  # GET request to obtain data
  req_url <- paste0(base_url, req, '&token=', token)
  response <<- jsonlite::fromJSON(req_url, simplifyMatrix = TRUE)

  
  response_df <-  response$STATION %>%
    select(-RESTRICTED) %>%
    rename(lat = LATITUDE,
           lon = LONGITUDE) %>%
    mutate_at(c('ELEVATION', 'lat', 'lon'), as.numeric) %>%
    filter(!grepl('COOP', SHORTNAME)) %>% 
    mutate(record_start=as.Date(PERIOD_OF_RECORD$start),
           record_end=as.Date(PERIOD_OF_RECORD$end)) %>%
    select(-c(SHORTNAME, PERIOD_OF_RECORD))

  return(response_df)
    
}