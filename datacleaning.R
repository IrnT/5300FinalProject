library(tidyverse)

#readins and cleaning

#basic weather data 
weather <- read.csv("./data/weather_history.csv")
weather$data_block_id = as.integer(weather$data_block_id)
#weather$datetime <- as.POSIXct(weather$datetime, "")
head(weather)
#write.csv(weather,"weather.csv")

#station data
stations <- read.csv("./data/editedweatherstationmapping.csv")

#gas prices
gas <- read.csv("./data/gas_prices.csv")
gas <- subset(gas, select = -c(forecast_date, origin_date))
colnames(gas) <- c("daily_gas_low_per_mwh", "daily_gas_high_per_mwh", "data_block_id")
head(gas)

#electricity prices
electric <- read.csv("./data/electricity_prices.csv")
head(electric)
summary(electric)
electric <- subset(electric, select = -c(origin_date))
colnames(electric) <- c("datetime", "hourly_solar_per_mwh", "data_block_id")
#electric$datetime <- as.POSIXct(electric$datetime)
head(electric)

#prosumer info
prosumer <- read.csv("./data/prosumer.csv")
head(prosumer)
summary(prosumer)

#client info
client <- read.csv("./data/client.csv")
head(client)
summary(client)
client <- subset(client, select = -c(date))

#first merge - weather and stations by long and lat to get county averages each hour block
bigPapa <- merge(weather, stations, by = c("latitude", "longitude"), all.x = TRUE, sort = FALSE)
bigPapa <- subset(bigPapa, select = -c(latitude, longitude))
bigPapa <- bigPapa[order(bigPapa$datetime), ]
bigPapa <- subset(bigPapa, county != -1)
rownames(bigPapa) <- NULL

head(bigPapa)
summary(bigPapa)

#average em
averagedPapa <- bigPapa %>%
  group_by(datetime, county) %>%
  summarize(datetime = first(datetime),
            avg_temp = mean(temperature),
            avg_dew = mean(dewpoint),
            avg_rain = mean(rain),
            avg_snowfall = mean(snowfall),
            avg_surface_pressure = mean(surface_pressure),
            avg_cloudcover_total = mean(cloudcover_total),
            avg_cloudcover_low = mean(cloudcover_low),
            avg_cloudcover_mid = mean(cloudcover_mid),
            avg_cloudcover_high = mean(cloudcover_high),
            avg_windspeed_10m = mean(windspeed_10m),
            avg_winddirection_10m = mean(winddirection_10m),
            avg_shortwave_radiation = mean(shortwave_radiation),
            avg_direct_solar_radiation = mean(direct_solar_radiation),
            avg_diffuse_radiation = mean(diffuse_radiation),
            data_block_id = first(data_block_id),
            county_name = first(county_name),
            county = first(county)
            )
head(averagedPapa)
summary(averagedPapa)

#merge with gas
bigPapa <- merge(averagedPapa, gas, by = "data_block_id", all.x = TRUE)
head(bigPapa)
summary(electric)
summary(bigPapa)

electric <- electric[electric$datetime >= ("2023-03-31 00:00:00"),]

#drop data_block_id because its causing SO MANY PROBLEMS
bigPapa <- subset(bigPapa, select = -c(data_block_id))
electric <- subset(electric, select = -c(data_block_id))

electricBoogaloo <- merge(x = bigPapa, y = electric, by = c('datetime'), all = TRUE)
head(electricBoogaloo)

biggestPapa <- merge(x = prosumer, y = electricBoogaloo, by = c('datetime', 'county'), all = TRUE)
head(biggestPapa)
summary(biggestPapa)

finalSet <- merge(x = biggestPapa, y = client, by = c('data_block_id', 'county', 'product_type', 'is_business'), all.x = TRUE)
head(finalSet)

finalSet <- finalSet[order(finalSet$datetime), ]
head(finalSet)

write.csv(finalSet, "./data/finalset.csv")
