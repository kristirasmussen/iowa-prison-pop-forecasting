fill_dates <- function(data,dates_begin="2012-01-01",dates_end="2021-11-01") {
  
  dates <- tibble(
    date = tk_make_timeseries(dates_begin, dates_end, by = "month")
  )
  
  df <- data %>% 
    right_join(dates) %>%
    replace(is.na(.),0) 
  return(df)
}