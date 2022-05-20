library(pacman)
pacman::p_load(DBI, odbc, tidyverse, gtools)

#' @title Request crop data (seed_harvest_wide) from Snowflake within a certain season range
#' @param conn The connection to the Snowflake database
#' @param min_year Minimum crop season year to pull from. Defaults to NA (pull all years)
#' @param max_year Maximum crop season year to pull from. Defaults to NA (pull all years)
#' @examples seed_harvest_wide <- download_crops(myconn, min_year=2020, max_year=2022)
#' @returns The seed_harvest_df data frame.
#' @export
download_crops <- function(conn, gdd_info_formatted, min_year=NA, max_year=NA) {
  usethis::use_data()
  if(gtools::invalid(conn)){
    stop("Invalid connection provided")
  }
  if(gtools::invalid(gdd_info_formatted)){
    stop("Invalid gdd_info_formatted provided")
  }
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
  # Restructure dataset
  seed_harvest_wide <- seed_harvest_df  %>% 
    pivot_wider(names_from = FIELD_OPERATION_TYPE,
                values_from = DATE) %>% 
    drop_na() %>% 
    mutate(harvest = as.Date(harvest),
           seeding = as.Date(seeding)) %>% 
    rename(harvest_date = harvest,
           seeding_date = seeding) %>% 
    filter(CROP_NAME %in% gdd_info_formatted$CROP, # FROM AVAILABLE CROP THRESHOLD DATA
           CROP_SEASON > min_year) # This originally said 2019
  return(seed_harvest_df)
}
