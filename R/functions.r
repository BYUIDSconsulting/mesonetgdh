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

#' @title Calculate weights according to elevation
#' @param data is the dataframe that contains the elevations. Columns should be named "FIELD_ID" and "station_id"
#' @example data 
#' p <- weighted(data)
#' @export
weighted <- function(data){
  elev <- elev_change(data) %>% 
    mutate(adjusted_wt = -elev_dif/1000 * 3.5) %>% 
    distinct(across(c("FIELD_ID", "station_id")), .keep_all = TRUE)
  
  data %>% left_join(elev) %>%
    group_by(FIELD_ID, station_id, Date, Hour) %>%
    summarize(wt_temp = temp_avg + adjusted_wt)
}


