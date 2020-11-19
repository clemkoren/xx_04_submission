library(data.table)

# You will need to change your working directory
setwd("/Users/Clemence/Desktop/xx_04_submission")

fileSources = file.path("code_task1", list.files("code_task1", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# code goes here
#CreateFakeData()

# load individual level data file
d <- readRDS("data_raw/individual_level_data.RDS")
d
View(d)
summary(d)




#### Q.6 Aggregate the dataset per day per municip. ####

# aggregate the number of sick people per day per municipality
d_agg <- aggregate(d$value, FUN = length, by = list(date = d$date, location_code = d$location_code))
colnames(d_agg)[3] <- "value"
summary(d_agg)





#### Q.7 Fill in the missing dates ####

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2020-12-31")

nb_municip <- 356
nb_days <- length((seq.Date(start_date, end_date, by = "day")))
# there are 4018 days in our study period
nb_days*nb_municip
# we expect the table to have 1430408 obs.



# table listing all days of the study period
table_date <- data.table(date = seq.Date(start_date, end_date, by = "day"))

# table listing the location code of all municipalities
table_location <- norway_locations_b2020[,1]
colnames(table_location)[1] <- "location_code"

# add empty lines to d_agg based on the full list of dates and locations
# and fill them with a value of 0 sick individuals
library(tidyr)
d_agg_compl <- d_agg %>% complete(table_date, table_location, fill = list(x=0))
colnames(d_agg_compl)[3] <- "value"
summary(d_agg_compl)





#### Q.8 Collapse data to ISOweek / ISOyear ####

# convert all dates to ISO year / week
d_agg_compl$date <- isoyearweek(d_agg_compl$date)
summary(d_agg_compl)

# aggregate according to date
d_clean <- aggregate(d_agg_compl$value, FUN = sum, by = list(date = d_agg_compl$date, location_code = d_agg_compl$location_code))
summary(d_clean)






#### Q.9 Split the data into production and training data ####

# subset into training and production data
d_training <- subset(d_clean, d_clean$date >= "2010-01" & d_clean$date <= "2019-52")
d_production <- subset(d_clean, d_clean$date >= "2020-01")

# split the training data into each municipality
d_training_split <- split(d_training, d_training$location_code)
# split the production data into each municipality
d_production_split <- split(d_production, d_production$location_code)


