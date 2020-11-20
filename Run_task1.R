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
library(fhidata)
table_location <- norway_locations_b2020[,1]
colnames(table_location)[1] <- "location_code"

# add empty lines to d_agg based on the full list of dates and locations
# and fill them with a value of 0 sick individuals
library(tidyr)
d_agg_compl <- d_agg %>% complete(table_date, table_location, fill = list(value=0))
summary(d_agg_compl)





#### Q.8 Collapse data to ISOweek / ISOyear ####

# convert all dates to ISO year / week
library(fhi)
d_agg_compl$date <- isoyearweek(d_agg_compl$date)
summary(d_agg_compl)

# aggregate according to date
d_clean <- aggregate(d_agg_compl$value, FUN = sum, by = list(date = d_agg_compl$date, location_code = d_agg_compl$location_code))
colnames(d_clean)[3] <- "value"
summary(d_clean)


# export the data in data_clean for future use
saveRDS(d_clean, file = "data_clean/weekly_cases_per_municip.rds")




#### Q.9 Split the data into production and training data ####

# subset into training and production data
d_training <- subset(d_clean, d_clean$date >= "2010-01" & d_clean$date <= "2019-52")
d_production <- subset(d_clean, d_clean$date >= "2020-01")

# split the training data into each municipality
d_training_split <- split(d_training, d_training$location_code)
# split the production data into each municipality
d_production_split <- split(d_production, d_production$location_code)





#### Q.10 Regression model ####

# exploratory graph on one municipality
x <- d_training_split$municip0301

plot(x$value, type="l", pch=20, lwd=0.4, xaxt="n", las=2,
     main="Weekly cases of Disease X in municipality 0301", ylab="Number of cases", xlab="Year")
axis(side=1, at=c(0, 52, 104, 156, 208, 260, 312, 364, 416, 468, 520),
     labels=c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"), las=2, tick=T)
# add a vertical line every 52 weeks = roughly separating each year
abline(v=c(0, 52, 104, 156, 208, 260, 312, 364, 416, 468, 520), lwd= 0.5, col="grey")



# we see seasonality in the number of recorded sick people every year, with more sick people in the middle of the year
# the function CreateFakeData also indicates a slight positive trend



# I will fit a sine curve to represent the seasonality of the data

# make a new variable for the number of weeks since the beginning of the study, to help with modelisation
x$week_id <- c(1:521)
summary(x)

# fit a sinusoidal linear model
xc <- cos(2 * pi * x$week_id / 52)
xs <- sin(2 * pi * x$week_id / 52)
fit.lm <- lm(x$value ~ xc + xs)
summary(fit.lm)





#### Q.11 Prediction Interval ####

# I didn't manage to do a 2 standard deviation prediction interval
# So I will assume that the 95% confidence interval represents well enough the interval requested
x_pred_int <- as.data.frame(predict(fit.lm, newdata = x, interval = "predict", level = 0.95))

# plot the fitted prediction, upper and lower limit of the interval on the previous graph
lines(x$week_id, x_pred_int$fit, col="red")
lines(x$week_id, x_pred_int$lwr, col="darkgrey", lwd=0.5)
lines(x$week_id, x_pred_int$upr, col="darkgrey", lwd=0.5)
legend ("bottomleft", legend = c("model prediction", "95% confidence interval"), col=c("red", "darkgrey"), lwd=1.3, cex=0.8)





#### Q.12 Identify potential outbreaks####

x$outbreak <- x$value - x_pred_int$upr
x$outbreak[x$outbreak<=0] <- NA
x$outbreak[x$outbreak>0] <- 1

points(x$week_id, x$outbreak, pch=20)

# due to lack of time, I could not find a proper way to display the outbreaks




#### Rest of the exercice ####

# I would have made a function to automate tasks 10 to 18 for each municipality.


