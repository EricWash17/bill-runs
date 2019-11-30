library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Load and clean data
data = read.csv("./data/bill-run-data.csv"
                , header = TRUE
                , na.strings = c("", "NA")
                , stringsAsFactors = FALSE)

names(data) <- tolower(names(data))

## Convert character to date
data$date <- as.POSIXct(data$date, format = '%m/%d/%y')

## Convert times to numerics (minutes)
data$time <- gsub("AM", "", data$time)
data$hour <- gsub('\\:.+', "", data$time)
data = data %>%
  mutate(hour = ifelse(hour == 12, 0, hour)
         , hour_as_minutes = as.numeric(hour) * 60)
reg_ex_non_hour = "^(.*?)\\:" # caputre hours to drop
data$non_hour <- gsub(reg_ex_non_hour, "", data$time)
data$time <- gsub(reg_ex_non_hour, "", data$time)

data$non_hour <- as.numeric(as.difftime(data$non_hour, format = "%M:%S", units = "mins"))
data = data %>%
  mutate(time = paste(hour, time, sep = ":")
         , time_in_minutes = round(hour_as_minutes + non_hour, 2)
         , percentile = round((place / starters) * 100, 0)
         , race_type = paste(round(distance, 0), metric, sep = " ")) %>%
  select(-c(hour_as_minutes, non_hour, hour, starters))

data$pace <- round(as.numeric(as.difftime(data$pace, format = "%M:%S", units = "mins")), 2)

## Format and bin race types
data = data %>%
  mutate(race_type = ifelse(distance %in% c(5, 10, 13, 26) | distance == 8 & metric == "km", race_type,
                ifelse(distance < 5 & metric == "km" | distance < 3 & metric == "miles", "short race", "other distance")))

table(data$race_type)
## Save
saveRDS(data, file = "./data/shiny_data.Rds")
