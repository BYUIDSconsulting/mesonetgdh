# library(pacman)
# pacman::p_load(dplyr, DBI)

#' @title Get all fields from the Snowflake database
#' @param myconn The connection to the Snowflake database
#' @examples
#' all_fields1 <- get_all_fields(myconn)
#' @export
get_all_fields <- function(myconn){
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
    rename(lat = LAT, lon = LON) %>%
    # Let's make sure we don't repeat fields
    distinct(FIELD_ID, .keep_all=TRUE)
  all_fields1 <- all_fields %>% 
    # We don't want to repeat fields
    # add the states and elevation using the weather package
    mutate(state = mesonetgdh::get_state(.)) %>% 
    drop_na() %>% 
    # Using only fields in Arizona, Idaho, Utah
    filter(state %in% c('AZ', 'ID', 'UT')) %>% 
    # Get elevation after we have the states
    # The DB connection isn't working
    mutate(field_elev = mesonetgdh::get_elev(.,conn=NULL)) %>%
    drop_na() %>%
    rename(field_lat=lat,
           field_lon=lon)
  return(all_fields1)
}


#' @title Get latest date with GDHs from the Snowflake database
#' @param myconn The connection to the Snowflake database
#' @examples
#' latest_date <- get_latest_date(myconn)
#' @export
get_latest_date <- function(myconn){
  # Latest GDH date in the database, get temperatures for everything after this
  latest_date <- DBI::dbGetQuery(
    myconn,
    "SELECT * FROM GDH_BYUI_DEV ORDER BY GDH_DATE DESC LIMIT 1;"
    ) %>%
    pull(GDH_DATE)
  return(latest_date)
}

#' @title Get the wide seed harvest dataframe (based on field crops data) from the Snowflake database
#' @param myconn The connection to the Snowflake database
#' @param gdd_info_formatted Growing Degree Day temperature threshold data
#' @description Reformat date into wide format data
#' @examples
#' seed_harvest_df <- get_seed_harvest(myconn)
#' @export
get_seed_harvest_wide <- function(myconn, gdd_info_formatted){
  # crop information, seeding and harvest dates from the database
  crops_query <- "
  SELECT o.FIELD_ID, o.FIELD_OPERATION_TYPE, o.CROP_NAME,
    CAST(o.CROP_SEASON AS INT) AS CROP_SEASON, o.START_DATE, o.END_DATE,
    g.GDH_MAX_DATE
    FROM FIELD_OPERATIONS o
    LEFT JOIN (
      SELECT MAX(GDH_DATE) AS GDH_MAX_DATE, FIELD_ID, CROP_SEASON, MIN(GDH_DATE) AS GDH_MIN_DATE
      FROM GDH_BYUI_DEV
      GROUP BY FIELD_ID, CROP_SEASON
    ) g
        ON o.FIELD_ID = g.FIELD_ID
        AND o.CROP_SEASON = g.CROP_SEASON
    ORDER BY FIELD_ID; 
  "
  field_crops <- DBI::dbGetQuery(myconn, crops_query)
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
    #drop_na() %>% 
    mutate(harvest = as.Date(harvest),
           seeding = as.Date(seeding)) %>% 
    rename(harvest_date = harvest,
           seeding_date = seeding) %>% 
    mutate(harvest_date = coalesce(harvest_date, GDH_MAX_DATE)) %>%
    mutate(end_of_year = as.Date(sprintf("%d-12-31",CROP_SEASON))) %>%
    mutate(harvest_date = coalesce(harvest_date, end_of_year)) %>%
    mutate(start_of_year = as.Date(sprintf("%d-01-01",CROP_SEASON))) %>%
    mutate(seeding_date = coalesce(seeding_date, start_of_year)) %>%
    filter(CROP_NAME %in% gdd_info_formatted$CROP, # FROM AVAILABLE CROP THRESHOLD DATA
           CROP_SEASON > 2018)
  return(seed_harvest_wide)
}

#' @title Push GDH data to Snowflake.
#' @param myconn A connection to the Snowflake database.
#' @param table The table to push to.
#' @param growing_degree_units The GDH data to push.
#' @export
push_gdh <- function(myconn, table="GDH_BYUI_DEV", growing_degree_units){
  growing_degree_units <- growing_degree_units %>%
    mutate(ID = paste(Hour,FIELD_ID,GDH_DATE,sep="_")) %>%
    ungroup()
  colnames(growing_degree_units) <- toupper(colnames(growing_degree_units))
  # Append to the table
  # dbCreateTable(myconn, table, growing_degree_units)
  rows_written <- DBI::dbAppendTable(myconn, table, growing_degree_units)
  print(sprintf("%d rows in growing_degree_units",nrow(growing_degree_units)))
}

#' @title Remove duplicate rows from a Snowflake table 
#' @param myconn A connection to the Snowflake database
#' @param table The table to remove duplicates from
#' @examples
#' remove_duplicates(table)
#' @export
remove_duplicates <- function(myconn, table){
  cmds <- c(
    sprintf("CREATE TABLE new_table LIKE %s COPY GRANTS;",table),
    sprintf("INSERT INTO new_table SELECT DISTINCT * FROM %s;",table),
    sprintf("ALTER TABLE %s SWAP WITH new_table;",table),
    "DROP TABLE new_table;"
  )
  lapply(cmds, function(cmd){DBI::dbGetQuery(myconn, cmd)})
}
