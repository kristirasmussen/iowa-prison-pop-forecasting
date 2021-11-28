library(dplyr)
library(tidyr)
library(RTseries)
library(forecast)

load("/Users/grantruedy/Georgia_Tech/CSE_6242/Project/monthly_prison_pop.rda")

#ARIMA by total count
df <- monthly_prison_pop %>% group_by(ds) %>% summarize(count = sum(n))

col1 <- df$count
ts1 <- ts(col1, start=2010, freq=12)
tsd1 <- tsd(ts1,time.units="Month", data.title="Prison Population",
           response.units="Number of People")

plot(ts1, main='Prison Population Over Time', ylab='Population')

iden(tsd1)

iden(tsd1, d = 1)

iden(tsd1, d = 2)



esti(data.tsd = tsd1, model =model.pdq(q = 1,d=1) ,y.range =c(0,10000))

esti(data.tsd = tsd1, model =model.pdq(p = 0,d=2, q=1) ,y.range =c(0,10000))


fit <- arima(ts1, order=c(p=0,d =1, q=1))

accuracy(fit)
plot(forecast(fit))

plot(forecast(auto.arima(ts1)))



#ARIMA by Offense Type
table(monthly_prison_pop$offense_types)

df1 <- monthly_prison_pop %>% group_by(ds, offense_types) %>% summarize(count = sum(n))

#Drug
drug <- df1 %>% filter(offense_types =='Drug')
col2 <- drug$count
ts_drug <- ts(col2, start=2010, freq=12)
tsd_drug <- tsd(ts_drug,time.units="Month", data.title="Prison Population for Drug Offenses",
            response.units="Number of People")

plot(ts_drug, main='Prison Population Over Time (Drug)', ylab='Population')

iden(tsd_drug, d = 2)

esti(data.tsd = tsd_drug, model =model.pdq(q = 1,d=1) ,y.range =c(0,2500))
esti(data.tsd = tsd_drug, model =model.pdq(p =1, d = 2,q = 1) ,y.range =c(0,2500))



fit1 <- arima(ts_drug, order=c(p=0,d =1, q=1))

accuracy(auto.arima(ts_drug))

plot(forecast(auto.arima(ts_drug)))

#Property
property <- df1 %>% filter(offense_types =='Property')
col3 <- property$count
ts_property <- ts(col3, start=2010, freq=12)
tsd_property <- tsd(ts_property,time.units="Month", data.title="Prison Population for Drug Offenses",
                response.units="Number of People")

plot(ts_property, main='Prison Population Over Time (Property)', ylab='Population')

iden(tsd_property, d = 1)

esti(data.tsd = tsd_property, model =model.pdq(q = 1,d=1) ,y.range =c(0,2500))

auto.arima(ts_property)

plot(forecast(auto.arima(ts_property)))
