#' @title Calculate Growing Degree Hours
#' @param data is the dataframe that is output from 'combine_df' function
#' @example data = temp_df
#' growing_degrees <- calc_gdh(data)
#' print(growing_degrees)
#' @export

calc_gdh <- function(data, field = NULL){
  
  if (!is.null(field)) {
    data %<>% filter(field_id == field) 
  }
  
  temp_data <- data %>%
    mutate(gdh = air_temp_f - Base_Fahrenheit,
           gdh = case_when(gdh < 0 ~ 0,
                           air_temp_f > Upper_Fahrenheit ~ Upper_Fahrenheit - Base_Fahrenheit, 
                           TRUE ~ gdh),
           total_gdh = cumsum(ifelse(is.na(gdh), 0, gdh)))
  
  if (!is.null(field)) {
    
    total_gdhs <- temp_data %>% 
      rbind(temp_data[nrow(data),]) %>% 
      select(FIELD_ID, CROP, seeding_date, date_time, gdh, total_gdh)
    
  } else{
    
    total_gdhs <- temp_data %>% 
      group_by(field_id) %>% 
      slice_max(total_gdh)
  }
  return(total_gdhs)
}
