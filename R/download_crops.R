library(pacman)
pacman::p_load(DBI, odbc, tidyverse)

#' @title Request crop data from Snowflake within a certain season range
#' @param conn The connection to the Snowflake database
#' @param min_year Minimum crop season year to pull from. Defaults to NA (pull all years)
#' @param max_year Maximum crop season year to pull from. Defaults to NA (pull all years)
#' @examples p <- pull_temp_all(df)
#' @returns The seed_harvest_df data frame.
#' @export
download_crops <- function(conn, min_year=NA, max_year=NA) {
  DBI::dbGetQuery(conn, "USE FARM_PROD.FARM__PRODUCTION;")
  if(is.na(min_year)){
    min_year = 0
  }
  if(is.na(max_year)){
    max_year = 9999
  }
  # Query the DB
  crops_query <- sprintf("
  SELECT FIELD_ID, FIELD_OPERATION_TYPE, CROP_NAME,
    CAST(CROP_SEASON AS INT) AS CROP_SEASON, START_DATE, END_DATE
    FROM FIELD_OPERATIONS
    WHERE CAST(CROP_SEASON AS INT) > %d
    AND CAST(CROP_SEASON AS INT) < %d
    ORDER BY FIELD_ID;
  ", min_year, max_year)
  field_crops <- DBI::dbGetQuery(conn, crops_query)
  # Reformat data
  seed_harvest_df <- field_crops %>%
    # Using the START_DATE column for dates
    rename(DATE = START_DATE) %>% 
    select(-END_DATE) %>% 
    # remove duplicates
    distinct() %>% 
    # select fields with one seeding or harvest entry for a unique crop and season 
    group_by(FIELD_ID, CROP_SEASON, CROP_NAME, FIELD_OPERATION_TYPE) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n==1,
           !is.na(CROP_NAME)) %>% # remove unidentified crop name
    select(-n)
  return(seed_harvest_df)
}
