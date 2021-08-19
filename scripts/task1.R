library(data.table)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(tidyr)
library(rstatix)
library(tidyverse)
library(ggpubr)
library(suncalc)
library(stringr)
library(imputeTS)
library(dplyr)
library(tsibble)
library(anomalize)
library(feasts)
library(timeDate)
library(nortest)
library(plotly)
library(tidyverse)
library(writexl)
library(coin)
library(prophet)

# Berlin
options(lat=52.52)
options(lon=13.405)

# Drezden
options(lat=51.04503)
options(lon=13.74030)

# Erfurt
options(lat=50.98028)
options(lon=11.03180)

# Hamburg
options(lat=53.53931)
options(lon=9.989613)

# Holzdorf
options(lat=54.560047)
options(lon=9.906278)

# Laage
options(lat=53.93227)
options(lon=12.34675)

# Braunschweig
options(lat=52.27547)
options(lon=10.53405)


# Rostok
options(lat=54.132586)
options(lon=12.06454)

# Greifswald
options(lat=54.092230)
options(lon=13.40645)

# Nobitz
options(lat=50.957103)
options(lon=12.492122)


# Get list of Data.Tables and merge it into one Data.Table
data <- lapply(list.files(path ="./input/50Hertz", full.name=TRUE), fread, skip=4, header=TRUE)

data <- rbindlist(data)

# Drop useless column (?)
data[, V5:=NULL]

# Set column names 
setnames(data, c("Datum", "Von", "bis", "MW"), c("measurement_date", "from", "to", "MW"))

# Adding datetime columns
data[, from:=paste(measurement_date, from)] 
data[, to:=paste(measurement_date, to)] 

# Dropping date column
data[, measurement_date:=NULL]

options(tz="Europe/Minsk")

# Parsing string to DateTime
data$from <- parse_date_time(data$from, "%d.%m.%y %H:%M", tz=getOption("tz"))
data$to <- parse_date_time(data$to, "%d.%m.%y %H:%M", tz=getOption("tz"))

data[format(to, "%H:%M") == "00:00", to:=to + days(1)]

weather_data <- fread("./stacking_data/Nobitz.csv", skip=6)
weather_data
weather_data[, V14:=NULL]
weather_data[, 1]
setnames(weather_data, 1, "to")

weather_data$to <- parse_date_time(weather_data$to, "%d.%m.%y %H:%M", tz=getOption("tz"))

weather_data

# Merge
setkey(data, to)
setkey(weather_data, to)


data_merged <- weather_data[data, roll="nearest"]

data_merged[, `:=`(from = NULL, ff10 =NULL, "W'W'" =NULL)]

data_merged

data_merged <- mutate(data_merged,
       is_light = if_else(to %between% 
                 select(getSunlightTimes(date = as.Date(to), lat = getOption("lat"), 
                                         lon = getOption("lon"), keep=c("sunrise", "sunset"), 
                                         tz=getOption("tz")), sunrise, sunset), TRUE, FALSE)
)

#data_merged[, `:=`(WW=NULL, c=NULL, DD=NULL)]

data_merged
data_merged <- data_merged %>% 
  mutate(T = na_locf(T),
         P0 = na_locf(P0),
         P = na_locf(P),
         VV = na_locf(VV),
         U = na_locf(U),
         Ff = na_locf(Ff),
         Td = na_locf(Td))

data_merged

data_merged <- data_merged[!are_duplicated(data_merged, index=to)]
data_tsbl <- as_tsibble(data_merged, key=NULL, index = to)



data_daily <- data_tsbl %>% 
  index_by(dt = as.Date(to)) %>% 
  summarise(MW = mean(MW)) %>% 
  fill_gaps() %>% 
  mutate(MW = forecast::na.interp(MW))




data_daily %>%
  model(STL(MW ~ trend(window = 365) + season(period = "year"))) %>%
  components() %>% 
  autoplot() + theme_minimal()

data_anomaly <- data_tsbl %>% 
  time_decompose(MW, frequency = "1 year", trend="1 year") %>% 
  anomalize(remainder) %>% 
  time_recompose()

data_anomaly %>% plot_anomaly_decomposition(size_dots=0.2) 

data_merged[!data_anomaly$anomaly == 'Yes']

data_merged <- data_merged %>% 
mutate(weekday = weekdays(to),
       month = month(to),
       season = ifelse(month %in% 3:5, "Spring",
                       ifelse(month %in% 6:8, "Summer",
                              ifelse(month %in% 9:11, "Autumn",
                                     "Winter"))))

data_merged[, `:=`(season=NULL, month=NULL)]
data_merged$is_light <- factor(data_merged$is_light)
data_merged$weekday <- factor(data_merged$weekday)

lockdown <- fread('Input/Lockdown/lockdown.csv', header=TRUE)

data_merged[, date:=as.Date(data_merged$to)]

lockdown$stay_home_flag <- lockdown$stay_home_flag + 1
lockdown$workplace_flag <- lockdown$workplace_flag + 1
lockdown$school_flag <- lockdown$school_flag + 1
lockdown[, date:=as.Date(lockdown$date)]

data_merged[VV == '10.0 and more', VV:="10"]

data_lockdown <- lockdown[data_merged, on='date']


merge(data_merged, lockdown, by='date')

data_lockdown <- data_lockdown %>% 
  replace_na(list(school = 0,
                  school_flag=0,
                  workplace=0,
                  workplace_flag=0,
                  stay_home=0,
                  stay_home_flag=0,
                  borders=0))



data_lockdown[, date:=NULL]

data_lockdown


fwrite(data_lockdown, 'stacking_data/data_Nobitz.csv')

as.Date(data_lockdown$to)

data_train <- data_lockdown %>% 
  select(ds = to, y = MW) %>% 
  slice(1:(n() - 90)) %>% 
  as.data.frame()

data_test <- data_lockdown %>% 
  select(ds = to, y = MW) %>% 
  tail(90) %>% 
  as.data.frame()


data_train %>% 
  ggplot(., aes(ds, y)) +
  geom_line() + 
  theme_minimal()



model <- prophet(data_train)  

forecast <- predict()


install.packages("rdwd")
library(rdwd)
data(geoIndex)
library(leaflet)
leaflet(geoIndex) %>% addTiles() %>%
  addCircles(~lon, ~lat, radius=900, stroke=F, color=~col) %>%
  addCircleMarkers(~lon, ~lat, popup=~display, stroke=F, color=~col)

link <- selectDWD(id=760,
                  var=c('air_temperature',
                        'cloud_type',
                        'dew_point',
                        'moisture',
                        'precipitation',
                        'soil_temperature',
                        'visibility',
                        'wind_synop'),
                  per='historical',
                  res="hourly")

unlist(link)
file <- dataDWD(unlist(link), read=FALSE)
clim <- readDWD(file, varnames=TRUE)
head(clim,1)
m <- nearbyStations(lat=53.53931,
               lon=9.989613,
               radius=50,
               res="hourly",
               mindate=as.Date("2014-07-10"))
tail(data.table(clim[[1]]))


?selectDWD


