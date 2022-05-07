#' @title Filter/restructure seeding and harvest data 
#' @param seed_harvest_data is a dataset containing the crop seeding and harvest dates for all the fields pulled from the database.
#' @param method choose between 'current' or 'historic'. 'current' will only include the current year for crop season. 'historic' will exclude the current year from crop season. 
#' @examples p <- seed_harvest_clean(seed_harvest_data, method = current)
#' @examples p <- seed_harvest_clean(seed_harvest_data, method = historic)

seed_harvest_clean <- function(seed_harvest_data, method=NULL) {
  
  df <- seed_harvest_data %>% 
    # remove duplicates
    distinct() %>% 
    # Using the START_DATE column for date reference
    rename(DATE = START_DATE) %>% 
    select(-END_DATE)
  
  # Raise error for missing method argument
  if (is.null(method)) {
    message("Warning message: \nPlease specify 'method' argument \nApplicable types: method=('historic', 'current')\n")
  }

  # For historical data
  else if (method == 'historic') {
    df1 <- df %>% 
      filter(CROP_SEASON <= lubridate::year(Sys.Date()) - 1) %>% 
      # select fields with one seeding or harvest entry for a unique crop and season 
      group_by(FIELD_ID, CROP_SEASON, CROP_NAME,FIELD_OPERATION_TYPE) %>% 
      mutate(row = row_number()) %>% 
      pivot_wider(names_from = FIELD_OPERATION_TYPE,
                  values_from = DATE) %>% 
      select(1:3, 6,5) %>% 
      drop_na()
    
  }
  
  # For current data
  else if (method == 'current') {
    df1 <- df %>% 
      filter(CROP_SEASON == lubridate::year(Sys.Date())) %>% 
      group_by(FIELD_ID, CROP_SEASON, CROP_NAME) %>%
      mutate(row = row_number()) %>% 
      pivot_wider(names_from = FIELD_OPERATION_TYPE,
                  values_from = DATE) %>% 
      select(-row) %>% 
      drop_na(seeding) %>% 
      mutate(harvest = replace_na(Sys.Date()))
  }
  
  df2 <- df1 %>% 
    mutate(harvest = as.Date(harvest),
           seeding = as.Date(seeding)) %>% 
    rename(harvest_date = harvest,
           seeding_date = seeding)

  return(df2)
  
}
