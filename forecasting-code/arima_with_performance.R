# Loading library
library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(openxlsx)

# import functions
fill_dates <- function(data,dates_begin="2012-01-01",dates_end="2021-11-01") {
  
  dates <- tibble(
    date = tk_make_timeseries(dates_begin, dates_end, by = "month")
  )
  
  df <- data %>% 
    right_join(dates) %>%
    replace(is.na(.),0) 
  return(df)
}

# Loading data
load("/Users/grantruedy/Georgia_Tech/CSE_6242/Project/monthly_prison_pop.rda")

#df <- monthly_prison_pop #%>% group_by(ds) %>% summarize(count = sum(n))

length(unique(monthly_prison_pop$offense_classifications))
table(monthly_prison_pop$offense_classifications)

# Filtering Other misdemanor for now, because of low frequency (only 1)
df <- monthly_prison_pop %>% 
filter(!offense_classifications %in% c("None","No Charge", 'Other Misdemeanor', 'Felony - Enhancement to Original Penalty', NA))
# table(df$offense_classifications)


#ts1 <- ts(df$count, start=2010, freq=12)
#plot(ts1, main='Prison Population Over Time', ylab='Population')

# aggregate offense_classifications and offense_types in one column and sum up the population
df_grouped <- df %>% 
  rename(date = ds) %>%
  unite(offense_category, c(offense_classifications,offense_types, offense_subtypes), sep = "-") %>%
  group_by(date, offense_category) %>% 
  summarize(count = sum(n))

df_grouped <- df_grouped %>% 
  filter(!offense_category %in% c('Simple Misdemeanor-Property','NA-No Charge')) %>% 
  # filter(date <= '2020-01-01') %>% 
  filter(date >= as.Date('2012-01-01'))

# save final data before forecasting
save(df_grouped, file = './data/final_data_before_model.rda')

# Check for unique offense category length
length(unique((df_grouped$offense_category)))

sort(table(df_grouped$offense_category))

df_nested <- df_grouped %>%
  group_by(offense_category) %>%
  nest() %>% 
  mutate(data = map(data, fill_dates))

df_ts <- df_nested %>%
  mutate(data.ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -date, 
                       start    = 2012,
                       freq     = 12))

fun <- function(x) {auto.arima(x, D=1)}
fun1 <- function(x) {accuracy(auto.arima(x, D=1))}

df_fit <- df_ts %>%
  mutate(fit.ets =  purrr::map(data.ts, fun)) #%>% 
  #mutate(acc = purrr::map(data.ts, fun1)) 

df_performance <- df_fit %>% select(offense_category, acc) %>% 
  unnest_wider(acc) 

names(df_performance) = 
  c('offense_category' ,'ME', 'RMSE', 'MAE', 'MPE', 'MAPE', 'MASE', 'ACF1')

save(df_performance, file='df_performance_seasonality.rda')

#sw_tidy
df_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = offense_category, value = estimate)


#sw_augment
df_ets <- df_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)


df_ets %>%
  ggplot(aes(x = date, y = .resid, group = offense_category)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ offense_category, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")


#Forecasting
df_forecast <- df_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 120))


df_tidy <- df_forecast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)


df_new <- df_tidy %>% select(offense_category, count, key, index, lo.95, hi.95)

df_new$count <- if_else(df_new$count < 0, 0, df_new$count)
df_new$lo.95 <- if_else(df_new$lo.95 < 0, 0, df_new$lo.95)
df_new$hi.95 <- if_else(df_new$hi.95 < 0, 0, df_new$hi.95)

df_forecast <- df_new %>% separate(offense_category, into = c('offense_classifications', 'offense_types', 'offense_subtypes'), 
                             sep = "-") 

save(df_forecast, file = 'all_levels_forecast.rda')
##stop here 


save(df_new, file = 'arima_seasonality_after_2012_zeros.rda')

############ Plotting #################################################################################
##########################################################################################################
# Aggravated Misdemeanor-Other

df_new %>% filter(offense_category %in% 'Aggravated Misdemeanor-Other') %>% 
  ggplot(aes(x = index, y = count, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_line() +
  labs(title = "Aggravated Misdemeanor-Other",
       subtitle = "ETS Model Forecasts",
       x = "Year", y = "People") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  #facet_wrap(~ offense_category, scales = "free_y", ncol = 3) +
  theme_tq() 
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

load("/Users/grantruedy/Georgia_Tech/CSE_6242/Project/df_performance_seasonality.rda")
df_performance_seasonality <- df_performance
load("/Users/grantruedy/Georgia_Tech/CSE_6242/Project/df_performance_no_seasonality.rda")
df_performance_no_seasonality <- df_performance

df_performance_seasonality$Seasonality <- 'Seasonal Difference'
df_performance_no_seasonality$Seasonality <- 'No Seasonal Difference'

combined <- union_all(df_performance_seasonality,df_performance_no_seasonality)
final <- combined %>% separate(offense_category, into = c('offense_classifications', 'offense_types'), 
                    sep = "-") 
write.xlsx(final, 'final_performance.xlsx')




