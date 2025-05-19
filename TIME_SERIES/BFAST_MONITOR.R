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

colnames(data)[colnames(data) == 'grid_code'] <- 'cover'


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



# Convert columns to numeric
data$B12 <- as.numeric(data$B12)
data$B8 <- as.numeric(data$B8)
data$B8A <- as.numeric(data$B8A)
data$B9 <- as.numeric(data$B9)
data$B4 <- as.numeric(data$B4)
data$cover <- as.numeric(data$cover)

# Verify the changes
str(data)

df <- data |>
  group_by(.geo, year_month_1) |>
 # distinct(.geo, year_month_1, .keep_all = TRUE) |>
  filter(cover == 4) # | cover == 3




# Compute vegetation indices
df <- df %>%
  mutate(
    NDVI = (B8 - B4) / (B8 + B4))

# Assuming 'geometry' identifies the location, filter by the first value of 'geometry'
df_2 <- df[df$.geo == df$.geo[630], ]

str(df_2)

# Step 2: Convert the extracted date to Date class in R
df_2$year_month <- as.Date(df_2$year_month, format = "%Y%m")


############################

time_series <- ts(df_2$NDVI, start=c(2019, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed <- na.approx(time_series)

#  annual streamflow of the Nile River    
out = beast(time_series_imputed, tcp.minmax =c(0,3), season='harmonic') #  'none': trend-only data without seasonlaity   
print(out)                   
plot(out, show.tip.label = TRUE, main = "Trend and abrupt change in NDVI value")
abline(v = 2023.5671, col = "red", lty = 5)



out$RMSE
out$R2
out$trend[1] #ncp
out$trend[9][1] #cp
out$trend[10][1] #cpPr
out$season$cp[1]
change_points <- out$trend[[9]]  # or: out$trend$cp
cp_probabilities <- out$trend[[10]]  # or: out$trend$cpPr

first_cp <- change_points[1]
second_cp <- change_points[2]
third_cp <- change_points[3]

first_pr <- cp_probabilities[1]
second_pr <- cp_probabilities[2]
third_pr <- cp_probabilities[3]


plot(out)                            # plot many variables 
plot(out, vars=c('y','s','t') )      # plot the Y, seasonal, and trend components only
plot(out, vars=c('s','scp','samp','t','tcp','tslp'))# Plot some selected variables in 

stl(time_series_imputed, s.window = 7) %>%
  plot()



# Initialize an empty list to store the results
results_list <- list()

bands <- c("NDVI")

# Iterate over unique geometries
for (geom in unique(df$.geo)) {
  for (band in bands) {
    # Filter data for the current geometry
    df_2 <- df[df$.geo == geom, ]
    
    # Create time series
    time_series <- ts(df_2[[band]], start=c(2019, 1), end=c(2024, 12), frequency = 12)  # assuming monthly data
    
    # Interpolate missing values
    time_series_imputed <- na.approx(time_series)
    
    # Apply beast model
    out <- beast(time_series_imputed, tcp.minmax = c(0, 3), season = 'harmonic')  # 'none': trend-only data without seasonality
    plot(out)
    
    # Extract change points and probabilities
    change_points <- out$trend[[9]]  # or: out$trend$cp
    cp_probabilities <- out$trend[[10]]  # or: out$trend$cpPr
    
    # Sort change points and their probabilities by the year (chronologically)
    sorted_indices <- order(change_points)  # Order the change points by the year (numerically)
    
    # Apply sorted indices to change_points and cp_probabilities
    change_points_sorted <- change_points[sorted_indices]
    cp_probabilities_sorted <- cp_probabilities[sorted_indices]
    
    # Extract individual sorted change points and probabilities
    first_cp <- change_points_sorted[1]
    second_cp <- change_points_sorted[2]
    third_cp <- change_points_sorted[3]
    first_pr <- cp_probabilities_sorted[1]
    second_pr <- cp_probabilities_sorted[2]
    third_pr <- cp_probabilities_sorted[3]
    
    # Store the results for the current geometry and band
    results_list[[paste(geom, band, sep = "_")]] <- data.frame(
      geom = geom,
      band = band,  # ensure band is correctly assigned
      RMSE = out$RMSE,
      R2 = out$R2,
      ncp = out$trend[1],
      cp = change_points_sorted,
      Season_cp = out$season$cp[1],
      cpPr = cp_probabilities_sorted,
      change_points = change_points_sorted,
      cp_probabilities = cp_probabilities_sorted,
      first_cp = first_cp,
      second_cp = second_cp,
      third_cp = third_cp,
      first_pr = first_pr,
      second_pr = second_pr,
      third_pr = third_pr
    )
  }
}

# Combine all results into one data frame
results_df <- do.call(rbind, results_list)

# Remove duplicates based on 'geom' and 'band'
results_df_unique <- results_df[!duplicated(results_df[c("geom", "band")]), ]

# Inspect the structure of the final results
str(results_df_unique)




##############3





###############


df_3 <- data |>
  group_by(.geo, year_month_1) |>
  #distinct(.geo, year_month_1, .keep_all = TRUE) |>
  filter(cover == 3) # | cover == 3


# Compute vegetation indices
df_3 <- df_3 %>%
  mutate(
    NDVI = (B8 - B4) / (B8 + B4))


# Assuming 'geometry' identifies the location, filter by the first value of 'geometry'
df_4 <- df_3[df_3$.geo == df_3$.geo[130], ]

str(df_4)

# Step 2: Convert the extracted date to Date class in R
df_4$year_month <- as.Date(df_4$year_month, format = "%Y%m")

time_series_2 <- ts(df_4$NDVI, start=c(2019, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed_2 <- na.approx(time_series_2)

#  annual streamflow of the Nile River    
out_2 = beast(time_series_imputed_2, season='harmonic') #  'none': trend-only data without seasonlaity   
print(out_2)                   
plot(out_2)


out_2$RMSE
out_2$R2
out_2$trend[1] #ncp
out_2$trend[9][1] #cp
out_2$trend[10][1] #cpPr
out_2$season$cp[1]



stl(time_series_imputed_2, s.window = 7) %>%
  plot()



# Initialize an empty list to store the results
results_list <- list()

bands <- c("NDVI")

# Iterate over unique geometries
for (geom in unique(df_3$.geo)) {
  for (band in bands) {
    # Filter data for the current geometry
    df_4 <- df_3[df_3$.geo == geom, ]
    
    # Create time series
    time_series_2 <- ts(df_4[[band]], start=c(2019, 1), end=c(2024, 12), frequency = 12)  # assuming monthly data
    
    # Interpolate missing values
    time_series_imputed_2 <- na.approx(time_series_2)
    
    # Apply beast model
    out_2 <- beast(time_series_imputed_2, tcp.minmax = c(0, 3), season = 'harmonic')  # 'none': trend-only data without seasonality
    #plot(out)
    
    # Extract change points and probabilities
    change_points <- out_2$trend[[9]]  # or: out$trend$cp
    cp_probabilities <- out_2$trend[[10]]  # or: out$trend$cpPr
    
    # Sort change points and their probabilities by the year (chronologically)
    sorted_indices <- order(change_points)  # Order the change points by the year (numerically)
    
    # Apply sorted indices to change_points and cp_probabilities
    change_points_sorted <- change_points[sorted_indices]
    cp_probabilities_sorted <- cp_probabilities[sorted_indices]
    
    # Extract individual sorted change points and probabilities
    first_cp <- change_points_sorted[1]
    second_cp <- change_points_sorted[2]
    third_cp <- change_points_sorted[3]
    first_pr <- cp_probabilities_sorted[1]
    second_pr <- cp_probabilities_sorted[2]
    third_pr <- cp_probabilities_sorted[3]
    
    # Store the results for the current geometry and band
    results_list[[paste(geom, band, sep = "_")]] <- data.frame(
      geom = geom,
      band = band,  # ensure band is correctly assigned
      RMSE = out_2$RMSE,
      R2 = out_2$R2,
      ncp = out_2$trend[1],
      cp = change_points_sorted,
      Season_cp = out_2$season$cp[1],
      cpPr = cp_probabilities_sorted,
      change_points = change_points_sorted,
      cp_probabilities = cp_probabilities_sorted,
      first_cp = first_cp,
      second_cp = second_cp,
      third_cp = third_cp,
      first_pr = first_pr,
      second_pr = second_pr,
      third_pr = third_pr
    )
  }
}

# Combine all results into one data frame
results_df_2 <- do.call(rbind, results_list)

# Remove duplicates based on 'geom' and 'band'
results_df_unique_2 <- results_df_2[!duplicated(results_df_2[c("geom", "band")]), ]

# Inspect the structure of the final results
str(results_df_unique_2)





#####################################################


out=beast.irreg(df_2$B12, time=df_2$year_month) 
plot(out)
print(out)

# Inspect the structure of the output from beast.irreg
str(out)


out$RMSE
out$R2
out$trend[1][1] #ncp
out$trend[9][1][1] #cp
out$trend[10][1][1] #cpPr

out$cp[1]



# Initialize an empty list to store the results
results_list <- list()


bands <-  c("B12") 


df_2$timestamp <- as.numeric(as.POSIXct(df_2$timestamp))


# Iterate over unique geometries
for (geom in df$.geo) {
  for (band in bands) {
    # Filter data for the current geometry
    df_2 <- df[df$.geo == geom, ]
    
    # Apply beast model
    out=beast.irreg(df_2[[band]], time=df_2$timestamp)
    #plot(out)
    # Store the results for the current geometry
    # Store the results for the current geometry and band
    results_list[[paste(geom, band, sep = "_")]] <- data.frame(
      geometry = geom,
      band = band,  # ensure band is correctly assigned
      RMSE = out$RMSE,
      R2 = out$R2,
      ncp = out$trend[1],
      cp = out$trend[9],
      cpPr = out$trend[10],
      Season_cp = out$season$cp[1]
    )
  }
}


# Combine all results into one data frame
results_df <- do.call(rbind, results_list)


results_df_unique <- results_df[!duplicated(results_df[c("geometry", "band")]), ]


str(results_df_unique)






# Run the bfast function on the imputed time series
bfast_result <- bfast(time_series_imputed, max.iter = 10, season = "harmonic")

summary(bfast_result)

plot(bfast_result, sim = df_2)
bfast_result



#################################

install.packages("forecast")  # Install the forecast package (only needed once)
library(forecast)             # Load the package


summary(df_4$NDVI)  # Check if all values are the same or if there are NAs

# Fit an ARIMAX model with external regressor (ad spend)
model <- auto.arima(time_series_imputed_2, ic = 'aicc')

# Show model summary
summary(model)

plot(model)

plot(forecast(model,h=20))
