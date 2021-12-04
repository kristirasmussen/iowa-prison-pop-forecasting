library(timetk)
library(tidyquant)

#Get data 
load("./data/monthly_prison_pop.rda")


# Filtering Other misdemanor for now, because of low frequency (only 1)
df <- monthly_prison_pop %>% 
  filter(!offense_classifications %in% 'Other Misdemeanor')
table(df$offense_classifications)

df_grouped <- df %>% rename(date = ds) %>%
  # unite(offesnse_category, c(offense_classifications,offense_types), sep = "-") %>%
  group_by(date) %>% 
  summarize(count = sum(n))


df_reg <- df_grouped %>% 
  tk_augment_timeseries_signature()

df_reg


#Regression model
fit_lm <- lm(count ~ ., data = select(df_reg,-date, -index.num,- diff))
summary(fit_lm)


#Forecast model
df_reg_pred <- df_reg %>% select(-date,-index.num,-diff)
pred <- predict(fit_lm, newdata = df_reg_pred)
error_lm <- df_reg %>% select(date,actual = count) %>% 
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
    mse = mean(error^2),
    rmse = mean(error^2)^0.5,
    mape = mean(abs(error_pct)),
    mpe = mean(error_pct),
    skew = sum(((error - mean)/std)^3)/n,
    kurtosis = sum(((error - mean)/std)^4)/n-3
  ) 
}


f_error(error_lm)


lm_metrics = data.table(f_error(error_lm))
lm_metrics$AIC <- AIC(fit_lm)

#save performance metrics
save(lm_metrics, file = 'lm_performance_metrics_benchmarking')
write.csv(lm_metrics, file = 'lm_performance_metrics_benchmarking.csv')
