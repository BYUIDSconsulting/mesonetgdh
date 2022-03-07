#' @title Get Elevations
#' @param df is the dataframe as output from the 'all_fields' function
#' @example df = makeaneexample
#' get_elev(df)
#' @export
get_elev <-  function(df){
  
  elevation <- c()
  
  for (i in 1:nrow(df)) {
    coords <- data.frame(x=df$lon[i], y=df$lat[i])
    elev <- elevatr::get_elev_point(coords, unit = 'feet', src='epqs', prj="EPSG:4326")
    elevation <- c(elevation, elev[[1]])
  }
  
  return(elevation)
}
