# install.packages("weatherData")
# library("devtools")
# install_github("ozagordi/weatherData")# will install new version


library(weatherData)
library(dplyr)

rest_coord <- read.csv("Ristoranti_coordinates_CAPS.csv", header = TRUE, stringsAsFactors = FALSE, encoding =  "latin1") #  encoding "latin1" to read tones in words in italian
provinces <- data.frame(province_name = as.character(unique(rest_coord$Provincia)), weather_station_code = 0) 


# Get weather station codes per province----------------------------------------------
prov_names = as.character(provinces$province_name)
k=0
try(
  for (c in prov_names) {
    k = k + 1
    x <- getStationCode(c)
    if (length(x)>=2){
      x <- strsplit(x[[2]], "\\s+")[[1]]
      if (nchar(x[3])!=4){
        provinces$weather_station_code[k] <- x[4]
      }
      else{
        provinces$weather_station_code[k] <- x[3]
      }
    }
    else if (length(x)==1){
      x <- strsplit(x[[1]], "\\s+")[[1]]
      if (nchar(x[3])!=4){
        provinces$weather_station_code[k] <- x[4]
      }
      else{
        provinces$weather_station_code[k] <- x[3]
      }
    }
  }, silent = TRUE)



# Get Weather Data ----------------------------------------------------------------------
getSummarizedWeather("LIVM", "2016-08-01", end_date = "2016-12-31", opt_custom_columns=T,custom_columns= c(3,22))

# Dataframe weather_data for each province all data per day
weather_data <- data.frame(Date = as.POSIXct(character()), Mean_TemperatureC = numeric(), Events = as.character(), Province = as.character())

for (j in (1:nrow(provinces))) {
  # pre-allocate list
  list_data_year <- vector(mode='list')
  # loop over dates, and fetch data
  o = 0
  for(i in c("2014","2015","2016"))
  {
    o = o + 1
    print(paste0("Fetching data for province: ", provinces$province_name[j], " for year: ", i))
    list_data_year[[o]] <- getSummarizedWeather(provinces$weather_station_code[j], paste(i, "01-01", sep="-"), end_date = paste(i, "12-31", sep="-"), opt_custom_columns=T,custom_columns= c(3,22))
  }
  prov_df <- rbind(list_data_year[[1]], list_data_year[[2]],list_data_year[[3]])
  prov_df$Province <- provinces$province_name[j]
  weather_data<- rbind(weather_data,prov_df)
}

# Binary Column for Rain-Thunderstorm-Snow (1) otherwise(0)
events <- unique(weather_data$Events)
events_remove <- c(NA, "", "Fog")
events <- events [! events %in% events_remove]

weather_data$Extreme_events <- ifelse(weather_data$Events %in% events,1,0)

# save weather dataframe
write.csv(weather_data, file = "weather_data.csv")


# Read weather Data
weather_data_frame <- read.csv("weather_data.csv", header = TRUE, stringsAsFactors = FALSE, encoding =  "latin1") #  encoding "latin1" to read tones in words in italian

# Dataframe with NA values
weather_data_NAs <- data.frame(Date = character(), Province = character())
weather_data_NAs$Date <- as.character(weather_data_NAs$Date)
weather_data_NAs$Province <- as.character(weather_data_NAs$Province)
# Print NA values
n=0
for (k in (1:nrow(weather_data_frame))) {
  if (is.na(weather_data_frame$Mean_TemperatureC[k])==TRUE){
    day = format(as.Date(weather_data_frame$Date[1]), format="%m/%Y")
    
    n= n+1
    #print(weather_data_frame$Date[k])
    weather_data_NAs[n,1] <- weather_data_frame$Date[k]
    weather_data_NAs[n,2] <- as.character(weather_data_frame$Province[k])
  }
}

# Replace NA with month's average per province
for (k in (1:nrow(weather_data_frame))) {
  if (is.na(weather_data_frame$Mean_TemperatureC[k])==TRUE){
    day = as.character(weather_data_frame$MonthYear[k])
    province_day = as.character(weather_data_frame$Province[k])
    mean_day_temp = as.numeric(mean(weather_data_frame[weather_data_frame$MonthYear==day & weather_data_frame$Province == province_day, "Mean_TemperatureC"], na.rm = TRUE))
    print(mean_day_temp)
    weather_data_frame$Mean_TemperatureC[k] <- mean_day_temp

  }
}
write.csv(weather_data_frame, file = "weather_data_frame.csv")

# Extract specific dates
weather_data_frame1516 <- weather_data_frame[format(as.Date(weather_data_frame$Date), format="%Y")>2014 & format(as.Date(weather_data_frame$Date), format="%Y")<2017,]
weather_data_frame1516$Date <- as.Date(weather_data_frame1516$Date)
# how many entries per province
table(weather_data_frame1516$Province)

# Data for all days in 2015-2016---------------------------------------

weather_data_all_days_1516 <- data.frame(Date = character(), Province = character())
for (p in unique(weather_data_frame1516$Province)) {
  dt <- data.frame(Date = seq(as.Date("2015-01-01"),as.Date("2016-12-31"),"day"), Province = p)
  weather_data_all_days_1516 <- rbind(weather_data_all_days_1516 , dt)
}
weather_data_all_days_1516$MonthYear <- format(as.Date(weather_data_all_days_1516$Date), format="%m/%Y")

weather_data_all_days_1516 <- left_join(weather_data_all_days_1516, weather_data_frame1516, by = c("Date", "Province"))

# Replace NA's with months average
for (k in (1:nrow(weather_data_all_days_1516))) {
  if (is.na(weather_data_all_days_1516$Mean_TemperatureC[k])==TRUE){
    day = as.character(weather_data_all_days_1516$MonthYear[k])
    province_day = as.character(weather_data_all_days_1516$Province[k])
    mean_day_temp = as.numeric(mean(weather_data_all_days_1516[weather_data_all_days_1516$MonthYear==day & weather_data_all_days_1516$Province == province_day, "Mean_TemperatureC"], na.rm = TRUE))
    # if mean temp of month available
    if (is.na(mean_day_temp)==FALSE){
      #print(mean_day_temp)
      weather_data_all_days_1516$Mean_TemperatureC[k] <- mean_day_temp
    }
  }
}
# Binary Column for Rain-Thunderstorm-Snow (1) otherwise(0)
events <- unique(weather_data_all_days_1516$Events)
events_remove <- c(NA, "", "Fog")
events <- events [! events %in% events_remove]
weather_data_all_days_1516$Extreme_events <- ifelse(weather_data_all_days_1516$Events %in% events,1,0)


# how many entries per province (731 days)
table(weather_data_all_days_1516$Province)

# Check if there are Missing values per column
sapply(weather_data_all_days_1516, function(x) any(is.na(x)))

# Save weather data epr province for all days between 2015 and 2016
write.csv(weather_data_all_days_1516[c(1,2,5,7)], file = "weather_data_all_days_1516.csv")


