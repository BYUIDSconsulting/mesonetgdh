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
