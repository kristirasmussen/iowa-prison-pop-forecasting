library(flexdashboard)
library(echarts4r)
library(dplyr)
library(janitor)
library(lubridate)
library(timetk)
library(shiny)
library(plotly)


load("./data/monthly_prison_pop.rda")
load("./data/pre_covid_forecast_offense_type.rda")
load("./data/all_data_forecast_offense_type_arima.rda") 
load("./data/pre_covid_forecast_offense_type_arima.rda") 
load("./data/arima_with_seasonality_before_covid.rda") 
load("./data/arima_with_seasonality_after_2012.rda") 

monthly_prison_pop %>% 
  filter(ds >= as.Date('2020-08-01')) %>%
  filter(!offense_types %in% c(NA, "None", "No Charge")) %>% 
  group_by(offense_types) %>% 
  mutate(lag1 = lag(n, 1, default = 0)) %>% 
  summarise(avg = mean(n),
            stdev = sd(n),
            diff_avg = mean(n - lag(n), na.rm = T),
            diff_sd = sd(n - lag(n), na.rm = T))

#################


fig <- all_data_forecast_offense_type %>%
  filter(offense_classifications == 'Aggravated Misdemeanor',
         offense_types == 'Other') %>%
  # group_by(index) %>%
  # summarise(count = sum(count),
  #           count_hi = sum(hi.95),
  #           count_lo = sum(lo.95)) %>%
  arrange(index) %>%
  plot_ly(x = ~index, y = ~ count, type = "scatter", mode='lines', hoverinfo = "x+y") %>%
  layout(xaxis = list(title = "Date"), 
         yaxis = list(title = "Count of Incarcerated Individuals", rangemode = "tozero")
         ,title = "All Data Predictions - Aggravated Misdemeanor - Other")

fig <- fig %>% add_trace(y = ~hi.95, mode = 'lines') 
fig <- fig %>% add_trace(y = ~lo.95, mode = 'lines')

fig

#########################

fig <- pre_covid_forecast_offense_type %>%
  filter(index > "2012-01-01",
         offense_classifications == 'Aggravated Misdemeanor',
         offense_types == 'Other') %>%
  # group_by(index) %>%
  # summarise(count = sum(count),
  #           count_hi = sum(hi.95),
  #           count_lo = sum(lo.95)) %>%
  arrange(index) %>%
  plot_ly(x = ~index, y = ~ count, type = "scatter", mode='lines', hoverinfo = "x+y") %>%
  layout(xaxis = list(title = "Date"), 
         yaxis = list(title = "Count of Incarcerated Individuals", rangemode = "tozero")
         ,title = "Pre Covid Predictions - Aggravated Misdemeanor - Other")

fig <- fig %>% add_trace(y = ~hi.95, mode = 'lines') 
fig <- fig %>% add_trace(y = ~lo.95, mode = 'lines')

fig

#################



fig <- df_new %>%
  filter(offesnse_category == 'Aggravated Misdemeanor-Other') %>%
  # group_by(index) %>%
  # summarise(count = sum(count),
  #           count_hi = sum(hi.95),
  #           count_lo = sum(lo.95)) %>%
  arrange(index) %>%
  plot_ly(x = ~index, y = ~ count, type = "scatter", mode='lines', hoverinfo = "x+y") %>%
  layout(xaxis = list(title = "Date"), 
         yaxis = list(title = "Count of Incarcerated Individuals", rangemode = "tozero")
         ,title = "All Data Predictions - Aggravated Misdemeanor - Other")

fig <- fig %>% add_trace(y = ~hi.95, mode = 'lines') 
fig <- fig %>% add_trace(y = ~lo.95, mode = 'lines')

fig


######### Quarterly ###########
# fig <- monthly_prison_pop %>%
#   filter(ds > "2012-01-01") %>%
#   mutate(qtr = quarter(ds, with_year = T)) %>% 
#   group_by(qtr) %>%
#   summarise(count=sum(qtr),
#             count_hi = sum(qtr),
#             count_lo = sum(qtr)) %>%
#   arrange(qtr) %>%
#   plot_ly(x = ~qtr, y = ~ count, type = "scatter", mode='lines', hoverinfo = "x+y") %>%
#   layout(xaxis = list(title = "Date"), 
#          yaxis = list(title = "Count of Incarcerated Individuals", rangemode = "tozero")
#          ,title = "Monthly Prison Population for Selected Offense Types")
# 
# fig


