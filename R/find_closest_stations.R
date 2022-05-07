#' @title Find the closest stations 
#' @param field_df is the fields dataset. 
#' @param stations_df is the dataset that includes all the active stations that was derived from the pull_active_stations() function.
#' @param radius is set to the maximum distance a station could be away from a field.  
#' @examples p <- find_closest_stations(field_df, stations_df, radius = ###)

find_closest_stations <- function(field_df, stations_df, radius) {
  
  # create vector to add closest station to corresponding farm
  station_id <- c()
  station_elev <- c()
  station_lat <- c()
  station_lon <- c()
  
  for (i in 1:nrow(field_df)){
    
    f_id <- field_df$FIELD_ID[i]
    # get lat, lon coordinates 
    lat <- field_df$field_lat[i]
    lon <- field_df$field_lon[i]
    # filter activity using seeding date
    start_date <- field_df$seeding_date[i]
    
    # find the closest stations within radius
    closest <- spatialrisk::points_in_circle(stations_df,
                                lat_center = lat,
                                lon_center = lon,
                                radius = measurements::conv_unit(radius, 'mile', 'm')) %>% 
      filter(record_start < start_date)
    
    # select top 5 closest stations
    if (nrow(closest) >= 5) {
      closest_5 <- closest %>%
        slice(1:5) 
    } else {
      closest_5 <- closest
    }
    
    #append to vector to later add as column 
    station_id <- c(station_id, list(closest_5$STID))
    station_elev <- c(station_elev, list(closest_5$ELEVATION))
    station_lat <- c(station_lat, list(closest_5$lat))
    station_lon <- c(station_lon, list(closest_5$lon))
    
  }
  
  field_station <- field_df %>%
    mutate(station_id = station_id,
           station_elev = station_elev,
           station_lat = station_lat,
           station_lon = station_lon) %>% 
    unnest(c(station_id, station_elev,station_lat, station_lon)) %>%
    select(-state)
  

  return(field_station)

}
