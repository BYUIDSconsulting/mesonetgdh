# library(pacman)
# pacman::p_load(dplyr, DBI)

#' @title Get Elevations
#' @param df is the dataframe as output from the 'all_fields' function
#' @examples  df = makeaneexample
#' get_elev(df)
#'
#' @export
get_elev <-  function(df, conn=NULL){
  # CREATE TABLE FIELD_ELEV_BYUI_DEV (
  #   FIELD_ID VARCHAR,
  #   ELEVATION NUMBER
  # );
  
  # Set an index so we can sort at the end
  df$row_num = seq.int(nrow(df))
  # Query from the DB if possible
  missing_elevs <- df
  joined_elevs <- df
  if(!is.null(conn)){
    print("Pulling from cache...")
    elev_query <- "SELECT FIELD_ID, ELEVATION FROM FIELD_ELEV_BYUI_DEV;"
    elevations <- DBI::dbGetQuery(conn, elev_query)
    joined_elevs <- df %>% left_join(elevations,  by="FIELD_ID")
    print(sprintf("%d rows returned", length(rownames(elevations))))
    missing_elevs <- joined_elevs %>% filter(is.na(ELEVATION))
  }

  if(length(rownames(missing_elevs)) > 0){
    print("Pulling remaining elevations from API...")
    # Get everything from the Elevation API that isn't cached
    # define coordinate columns
    coords <- data.frame(x=missing_elevs$lon, y=missing_elevs$lat)
    # get elevation from source
    elev <- elevatr::get_elev_point(coords, unit = 'feet', src='epqs', prj="EPSG:4326")
    # extract elevation column
    elevation <- elev[[1]]
    # Add elevations back to the data frame
    missing_elevs$ELEVATION = elevation
    # Cache elevations to the database
    if(!is.null(conn)){
      print("Starting caching process...")
      res <- DBI::dbAppendTable(conn,"FIELD_ELEV_BYUI_DEV",
                                missing_elevs %>% select(FIELD_ID, ELEVATION))
      print(res)
    }
  }
  else{
    print("All elevations cached, skipping API...")
  }
  
  # Join the two data frames back together
  # TODO figure out row numbers
  total_df <- joined_elevs %>%
    full_join(missing_elevs, by="FIELD_ID") %>%
    mutate(ELEVATION=coalesce(ELEVATION.x, ELEVATION.y)) %>%
    #distinct(row_num.x) %>%
    arrange(row_num.x) %>%
    distinct(FIELD_ID, .keep_all=TRUE)
  # Make sure it is in the right order
  print(total_df)
  return(elevation)
}


#' @title Get state abbreviations
#' @param df is the dataframe that includes the coordinate columns. Make sure to rename columns to 'lat', 'lon'
#' @examples  df 
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
#' @examples  data 
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
#' @examples data 
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
#' @examples data 
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
#' @examples data 
#' p <- elev_change(data)
#' @export
elev_change <- function(data){
  
  data %>% group_by(FIELD_ID, station_id) %>%
    summarize(elev_dif = field_elev - station_elev) %>%
    unique()
}

#' @title Calculate weights according to elevation
#' @param data is the dataframe that contains the elevations. Columns should be named "FIELD_ID" and "station_id"
#' @examples data 
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


