# library(pacman)
# pacman::p_load(DBI, odbc, tidyverse)

#' @title Request fields data (all_fields1) from Snowflake
#' @param conn The connection to the Snowflake database
#' @examples all_fields1 <- download_crops(myconn) %>% filter(state %in% c('AZ','ID','UT'))
#' @returns The all_fields1 data frame.
#' @export
download_fields <- function(conn, min_year=NA, max_year=NA) {
  DBI::dbGetQuery(conn, "USE FARM_PROD.FARM__PRODUCTION;")
  fields_query <- "
  SELECT fi.ID as FIELD_ID,
      CAST(
        PARSE_JSON(b.MULTIPOLYGONS)['multipolygons'][0]['rings'][0]['points'][0]['lat']
        AS DECIMAL(12,8))
        AS LAT,
      CAST(PARSE_JSON(b.MULTIPOLYGONS)['multipolygons'][0]['rings'][0]['points'][0]['lon']
        AS DECIMAL(12,8))
        AS LON
  FROM FIELDS fi
  INNER JOIN BOUNDARIES b
      ON fi.ID = b.FIELD_ID
  ORDER BY FIELD_ID;
  "
  # Get all fields coordinates from the database
  all_fields <- DBI::dbGetQuery(myconn, fields_query) %>%
    rename(lat = LAT, lon = LON)
  all_fields1 <- all_fields %>%
    # Get state names and elevations
    mutate(state = mesonetgdh::get_state(.),
         field_elev = mesonetgdh::get_elev(.)) %>% 
    drop_na() %>% 
    rename(field_lat=lat,
           field_lon=lon)
  return(all_fields1)
}
