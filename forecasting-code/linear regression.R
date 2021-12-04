library(timetk)
library(tidyquant)

#Get data 

load("./data/monthly_prison_pop.rda")


# Filtering Other misdemanor for now, because of low frequency (only 1)
df <- monthly_prison_pop %>% 
  filter(!offense_classifications %in% 'Other Misdemeanor')
table(df$offense_classifications)



# aggregate offense_classifications and offense_types in one column and sum up the population
df_grouped <- df %>% rename(date = ds) %>%
  unite(offesnse_category, c(offense_classifications,offense_types), sep = "-") %>%
  group_by(date, offesnse_category) %>% 
  summarize(count = sum(n))

df_grouped <- df_grouped %>% 
  filter(!offesnse_category %in% c('Simple Misdemeanor-Property','NA-No Charge')) %>% 
  #filter(date <= '2020-01-01') %>% 
  filter(date > '2012-01-01') %>% 
  ungroup()

df_reg <- df_grouped %>% 
  tk_augment_timeseries_signature()

df_reg

train_lm <- df_reg %>% filter(date > '2012-01-01' & date < '2019-11-01')
test_lm <- df_reg %>% filter(date <= '2021-11-01' & date >= '2019-11-01')

#Regression model
fit_lm <- lm(count ~ ., data = select(train_lm,-date))
summary(fit_lm)


#Forecast model
pred <- predict(fit_lm, newdata = select(test_lm,-date,index.num))
error_lm <- test_lm %>% select(date,actual = count) %>% 
  mutate(pred = pred,
         error = actual - pred,
         error_pct = error/actual)
error_lm

#Calculate error
#Error function
f_error <- function(data){
  data %>% summarise(
    n=length(error),
    mean = mean(error),
    var = sum((error-mean)^2)/(n-1),
    std = sqrt(var),
    mae = mean(abs(error)),
    rmse = mean(error^2)^0.5,
    mape = mean(abs(error_pct)),
    mpe = mean(error_pct),
    skew = sum(((error - mean)/std)^3)/n,
    kurtosis = sum(((error - mean)/std)^4)/n-3
  ) 
}


f_error(error_lm)

lm_metrics = data.table(f_error(error_lm))
save(lm_metrics, file = 'lm_performance_metrics')
