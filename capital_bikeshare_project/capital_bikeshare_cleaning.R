library(here)

#### Download file & read in data ####
# URL of the dataset
download_url <- "https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip"

# Set the download path using here()
download_path <- here("data", "bike+sharing+dataset.zip")

# Check if file already exists; download if not.
if (!file.exists(download_path)) {
  # Download the dataset
  download.file(download_url, download_path, method = "auto", quiet = FALSE, mode = "wb")
  cat("Dataset downloaded successfully and saved at:", download_path, "\n")
}

# Unzip the dataset
unzip(download_path, exdir = here("data"))
bike <- read.csv(here("data", "hour.csv"))[,-1] # ignore the first id column - "instant"

# Display the first few rows of the "bike" data frame
str(bike) # remember: "atemp" is normalized temperature in Celsius

# Data preparation ####
# Arranging values and changing data type
bike$yr <- as.factor(ifelse(bike$yr == 0, '2011', '2012'))

bike$mnth <- as.factor(months(as.Date(bike$dteday), 
                              abbreviate = TRUE))

bike$hr <- factor(bike$hr)

bike$weekday <- as.factor(weekdays(as.Date(bike$dteday)))

bike$season <- as.factor(ifelse(bike$season == 1, 'Spring',
                                ifelse(bike$season == 2, 'Summer',
                                       ifelse(bike$season == 3, 
                                              'Fall', 'Winter'))))

# weather situation
bike$weathersit <- as.factor(ifelse(bike$weathersit == 1, 'Good',
                                    ifelse(bike$weathersit == 2, 
                                           'Fair',
                                           ifelse(bike$weathersit == 
                                                    3, 'Bad', 
                                                  'Very Bad'))))

bike$holiday<-as.factor(ifelse(bike$holiday == 0, 'No', 'Yes'))

bike$workingday<-as.factor(ifelse(bike$workingday == 0, 'No', 
                                  'Yes'))

#Changing columns names
#names(bike)[names(bike) == "registered"] <- "new"
names(bike)[names(bike) == "cnt"] <- "total"

#Denormalizing: Temperature
for (i in 1:nrow(bike)){
  tn = bike[i, 10]
  t = (tn * (39 - (-8))) + (-8)
  bike[i, 10] <- t
}

# and converting to Fahrenheit
bike$temp <- bike$temp * 9/5 + 32

#Denormalizing: Feeling temperature
for (i in 1:nrow(bike)){
  tn = bike[i, 11]
  t = (tn * (50 - (-16))) + (-16)
  bike[i, 11] <- t
}

# and converting to Fahrenheit
bike$atemp <- bike$atemp * 9/5 + 32


#Denormalizing: Humidity
bike$hum <- bike$hum * 100
names(bike)[names(bike) == "hum"] <- "humidity" # use more descriptive name

#Denormalizing: Wind speed
bike$windspeed <- bike$windspeed * 67

#Write the new file
bike <- bike[-1] # ignore the original "dteday" col in output file
write.csv(bike, here("capital_bikeshare_project/data_clean", "bike_clean.csv"), row.names = FALSE)
