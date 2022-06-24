make_ranges_day <- function(from, to = Sys.Date(), n = 5){
  from <- as.Date(from)
  to <- if(to == Sys.Date()) Sys.Date() - 1 else as.Date(to)
  by = paste(n, "days")
  starts <- seq(from, to - n + 1, by = by)
  ends <- seq(from + n - 1, to, by = by)
  last <- length(starts)
  remaining <- to - ends[last] + 1
  if(remaining != 0){
    starts <- c(starts, ends[last] + 1)
    ends <- c(ends, to)
  }
  return(data.frame(starts, ends))
}

pull_temp_block <- function(df, start_date, end_date, uniq_stations){
  if(start_date > end_date){
    temp <- start_date
    start_date <- end_date
    end_date <- start_date
  }
  start_date <- paste0(gsub('-', '', start_date), '0000')
  end_date <- paste0(gsub('-', '', end_date), '0000')
  print(start_date)
  print(end_date)
  base_url <- 'https://api.synopticdata.com/v2'
  stations_str <- stringr::str_c(uniq_stations,collapse=",")
  req <- paste0('/stations/timeseries?stid=', stations_str
                ,'&vars=air_temp&fields=stid,longitude,latitude,elevation'
                ,'&units=english,temp|F'
                ,'&start=', start_date, '&end=', end_date)
  req_url <- paste0(base_url, req, '&token=', token)
  response <- jsonlite::fromJSON(req_url, simplifyMatrix = TRUE)
  resp_json <- jsonlite::toJSON(response)
  print("Number of stations requested:")
  print(length(uniq_stations))
  print("Number of stations returned:")
  print(length(response$STATION))
  print(req_url)
  df_request <- as.data.frame(NULL)
  # Loop through each station and add its info to the data frame
  for(i in 1:length(uniq_stations)){
    id <- uniq_stations[i]
    print(response$SUMMARY)
    resp <- data.frame(lapply(response$STATION[i,]$OBSERVATIONS, unlist)) %>%
      mutate(station_id = id)
    df_request <- rbind(df_request, resp)
  }
  global_dfr <<- df_request
  df_request <- df_request %>% 
    mutate(year=lubridate::year(date_time))  
  return(df_request)
}

#' @title Request the temperature data from MesoWest, but fast
#' @param df is the fields_stations dataset derived from the find_closest_station() function. 
#' @examples p <- pull_temp_all(df)
#' @export
pull_temp_all <- function(df, min_date=NA, max_date=NA) {
  pt_df <<- df
  
  stations_date <- df %>% 
    select(station_id, harvest_date, seeding_date)

  df_request <- as.data.frame(NULL)
  
  # Get begining and end dates
  start_date <- paste0(gsub('-', '', min(stations_date$seeding_date)), '0000')
  if(!is.na(min_date)){
    start_date <- paste0(gsub('-', '', min_date), '0000')
  }
  end_date <- paste0(gsub('-', '', max(stations_date$harvest_date)), '0000')
  if(!is.na(max_date)){
    end_date <- paste0(gsub('-', '', max_date), '0000')
  }
  print(start_date)
  print(end_date)
  date_range_df <- make_ranges_day(min_date, max_date, n=1)
  
  # Get data for each date block (the api only allows a certain number of days)
  uniq_stations <- df$station_id %>% unique()
  for(i in 1:nrow(date_range_df)){
    row <- date_range_df[i,]
    df_request <- rbind(
      df_request,
      pull_temp_block(df, row$starts, row$ends, uniq_stations)
    )
  }
  
  return(df_request)
  
}
