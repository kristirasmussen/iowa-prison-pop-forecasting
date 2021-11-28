library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)

load("/Users/grantruedy/Georgia_Tech/CSE_6242/Project/monthly_prison_pop.rda")

df <- monthly_prison_pop %>% group_by(ds) %>% summarize(count = sum(n))

table(monthly_prison_pop$offense_classifications)

ts1 <- ts(df$count, start=2010, freq=12)
plot(ts1, main='Prison Population Over Time', ylab='Population')

df_grouped <- monthly_prison_pop %>% rename(date = ds) %>%
  unite(Merged, c(offense_classifications,offense_types), sep = "-") %>%
  group_by(date, Merged) %>% 
  summarize(count = sum(n))

unique((monthly_prison_pop$offense_subtypes))


table(df_grouped$Merged)

df_nested <- df_grouped %>%
  group_by(Merged) %>%
  nest()

df_ts <- df_nested %>%
  mutate(data.ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -date, 
                       start    = 2010,
                       freq     = 12))


df_fit <- df_ts %>%
  mutate(fit.ets = map(data.ts, ets))


#sw_tidy
df_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = Merged, value = estimate)


#sw_augment
df_ets <- df_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)


df_ets %>%
  ggplot(aes(x = date, y = .resid, group = Merged)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Bike Quantity Sold By Secondary Category",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ Merged, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")


#Forecasting
df_forecast <- df_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 120))


df_tidy <- df_forecast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)


df_new <- df_tidy %>% select(Merged, count, key, index, lo.95, hi.95)

all_data_forecast_offense_type <- df_new %>% separate(Merged, into = c('offense_classifications', 'offense_types'), 
                    sep = "-") 

save(all_data_forecast_offense_type, file='all_data_forecast_offense_type.rda')

pre_covid_forecast_offense_type <- df_new %>% separate(Merged, into = c('offense_classifications', 'offense_types'), 
                                          sep = "-") 

save(pre_covid_forecast_offense_type, file='pre_covid_forecast_offense_type.rda')

split_df %>%
  ggplot(aes(x = index, y = count, color = key, group = offense_classifications)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  #geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
  #            fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Prison Population by Offense Type",
       subtitle = "ETS Model Forecasts",
       x = "Year", y = "People") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ offense_classifications, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
