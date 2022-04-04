#' @title Calculate Growing Degree Hours
#' @param data is the dataframe that is output from 'combine_df' function
#' @example data = temp_df
#' growing_degrees <- calc_gdh(data)
#' print(growing_degrees)
#' @export

calc_gdh <- function(data, field = NULL){
  
  if (!is.null(field)) {
    data %<>% filter(FIELD_ID == field) 
  }
  
  temp_data <- data %>%
    group_by(FIELD_ID, CROP_NAME, CROP_SEASON) %>%
    filter(Date >= seeding_date & Date <= harvest_date) %>%
    mutate(gdh = temp_combined_avg - Base_Fahrenheit,
           gdh = case_when(gdh < 0 ~ 0,
                           temp_combined_avg > Upper_Fahrenheit ~ Upper_Fahrenheit - Base_Fahrenheit, 
                           TRUE ~ gdh),
           total_gdh = cumsum(ifelse(is.na(gdh), 0, gdh))) 
  
  
  
  if (!is.null(field)) {
    
    total_gdhs <- temp_data %>% 
      rbind(temp_data[nrow(data),]) %>% 
      select(FIELD_ID, CROP_NAME, seeding_date, date_time, gdh, total_gdh)
    
  } else{
    
    total_gdhs <- temp_data %>% 
      group_by(FIELD_ID, CROP_NAME, CROP_SEASON) %>% 
      filter(total_gdh == max(total_gdh)) %>%
      select(FIELD_ID, CROP_NAME, seeding_date, harvest_date, total_gdh)

  }
  
  write_csv(total_gdhs, paste0("GDH-", Sys.Date(), ".csv"))
  return(total_gdhs)
}
