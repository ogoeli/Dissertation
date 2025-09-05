library(sf)
library(sp)
library(tidyverse)
library(raster)
library(dplyr)
library(data.table)
library(terra)
library(jsonlite)
library(bfast)
library(Rbeast)
library(DBEST)


#data <- read.csv('/scratch/ope4/Downloads/Band_Values_By_Class.csv', quote = "\"")

data <- boundary



# Step 1: Extract the date part (first 8 characters from 'system.index')
data$timestamp <- substr(data$system.index, 1, 8)

# Step 2: Convert the extracted date to Date class in R
data$timestamp <- as.Date(data$timestamp, format = "%Y%m%d")

# Step 3: Extract the year and month from timestamp
data$year_month_1 <- format(data$timestamp, "%Y-%m")

# Extract year and month from the system_index column
data <- data %>%
  mutate(
    year = substr(system.index, 1, 4),
    month = substr(system.index, 5, 6),
    year_month = paste(year, month, sep = "-")
  )


# Calculate indices
data$NDVI <- (data$B8 - data$B4) / (data$B8 + data$B4)
data$NDWI <- (data$B3 - data$B8) / (data$B3 + data$B8)
data$RENDVI <- (data$B8 - data$B5) / (data$B8 + data$B5)
data$CCCI <- data$NDVI * data$RENDVI
data$EWDI = ((data$B11 - data$B8) / (data$B11 + data$B8)) - ((data$B8 - data$B4) / (data$B8 + data$B4))


df <- data |>
  group_by(.geo, year_month_1) |>
  # distinct(.geo, year_month_1, .keep_all = TRUE) |>
  filter(cover == 4) # | cover == 3




# Assuming 'geometry' identifies the location, filter by the first value of 'geometry'
df_2 <- df[df$.geo == df$.geo[15], ]

str(df_2)

# Step 2: Convert the extracted date to Date class in R
df_2$year_month <- as.Date(df_2$year_month, format = "%Y%m")



############# DBEST MODEL ###################



time_series <- ts(df_2$CCCI, start=c(2019, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed <- na.approx(time_series)



DBEST.bettle <- DBEST(data=time_series_imputed, data.type="cyclical", 
                     algorithm="change detection", 
                     breakpoints.no=3, first.level.shift=0.1, 
                     second.level.shift=0.2, duration=148, 
                     distance.threshold="default", alpha=0.05, plot="on")

print(DBEST.bettle)
plot(DBEST.bettle)

######################################################


############################### BEAST MODEL ####################################



time_series <- ts(df_2$NDWI, start=c(2019, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed <- na.approx(time_series)


BEAST_BETTLE <- beast(time_series_imputed, # start  = c(2019,01,01),deltat = 1/365,  # period = 365      
          season  = 'harmonic')  
print(BEAST_BETTLE)
plot(BEAST_BETTLE)





############################################################################################



#################################### BFAST MODE ###########################



time_series <- ts(df_2$NDWI, start=c(2019, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed <- na.approx(time_series)




# ratio of distance between breaks (time steps) and length of the time series
BFAST_BETTLE <- bfast(time_series_imputed, season = "harmonic")
plot(BFAST_BETTLE)
BFAST_BETTLE





##########################################################################################################



