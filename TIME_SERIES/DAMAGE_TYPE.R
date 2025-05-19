# Load necessary libraries
install.packages("Rbeast")


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



data_6 <- read.csv('/scratch/ope4/Downloads//DATA/CSV/DATA/Band_Values_By_POLY_6.csv')
data_7 <- read.csv('/scratch/ope4/Downloads//DATA/CSV/DATA/Band_Values_POLY_7.csv')
data_8 <- read.csv('/scratch/ope4/Downloads//DATA/CSV/DATA/Band_Values_POLY_8.csv')
data_9 <- read.csv('/scratch/ope4/Downloads//DATA/CSV/DATA/Band_Values_POLY_9.csv')
data_10 <- read.csv('/scratch/ope4/Downloads//DATA/CSV/DATA/Band_Values_POLY_10.csv')






##select the healthy points and point of interest
data_6 <- data_6 |>
  filter(classification == 3 | classification == 1 | classification == 4) ## mortality

data_7 <- data_7 |>
  filter(classification == 3 | classification == 1 | classification == 4) ## crown discoloration

data_8 <- data_8 |>
  filter(classification == 3 | classification == 1 | classification == 4) ##75% Defoliation

data_9 <- data_9 |>
  filter(classification == 3 | classification == 1 | classification == 4) ##50% Defoliation

data_10 <- data_10 |>
  filter(classification == 3 | classification == 1 | classification == 4) ##Crown Dieback


## rename the point of interest to match the polygon number
data_6$classification[data_6$classification == 1 | data_6$classification == 4] <- 6

data_7$classification[data_7$classification == 1 | data_7$classification == 4] <- 7

data_8$classification[data_8$classification == 1 | data_8$classification == 4] <- 8

data_9$classification[data_9$classification == 1 | data_9$classification == 4] <- 9

data_10$classification[data_10$classification == 1 | data_10$classification == 4] <- 10

data <- rbind(data_6, data_7, data_8, data_9, data_10)

str(data)

colnames(data)[colnames(data) == 'classification'] <- 'cover'

# Randomly select 50% of rows
#data <- data %>%
#  slice_sample(prop = 0.1)

table(data$cover)


# Extract coordinates and split into longitude and latitude
coords <- t(sapply(data$.geo, function(x) {
  coords <- fromJSON(x)$coordinates
  return(coords)
}))

# Assign longitude and latitude to separate columns
data$longitude <- coords[, 1]
data$latitude <- coords[, 2]

# View the new columns
head(data[, c("longitude", "latitude")])

boundary <- read_sf("/scratch/ope4/POLY_2/POLY_2/POLY_2.shp")


# Ensure your 'data' dataframe has 'longitude' and 'latitude' columns
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = st_crs(boundary))

# Assign the CRS of the boundary to data_sf
data_sf <- st_set_crs(data_sf, st_crs(boundary)) #|>
  #dplyr::select(-.geo)

# Reproject data_sf to match boundary's CRS
data_sf <- st_transform(data_sf, st_crs(boundary))



#apply tree mask

tree_mask <- brick("/scratch/ope4/MERGE/TREE_MASK/TREE_MASK.TIF")
#tree_mask <- brick("/scratch/ope4/Downloads//DATA/TIF/Tree_NoTree_Mark.tif")


#tree_mask <- brick("/home/ope4/JUPYTER_CODE/ML/predicted_classification_top_bands.tif")

#plot(tree_mask)

# Check CRS of points and raster
crs_points <- st_crs(data_sf)  # or crs(points) if using sp package
crs_raster <- crs(tree_mask)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(data_sf, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(tree_mask, points)  # or 'simple' if preferred



nrow(extracted_values)
nrow(data_sf)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_15 <- cbind(extracted_values_2, data_sf)

table(extracted_values_15$TREE_MASK)
table(extracted_values_15$cover_code)

# Now, convert the dataframe to an sf object and assign CRS
boundary <- extracted_values_15 |>
  dplyr::filter(TREE_MASK >= 1) ##height greater than or equal to 4


write.csv(boundary, "/scratch/ope4/Downloads//DATA/CSV/DATA/boundary.csv", col.names = TRUE, row.names = TRUE)

?write.csv()
table(boundary$cover)

boundary$timestamp <- sub(".*_(\\d{8})T.*", "\\1", boundary$system.index)



# Step 1: Extract the date part (first 8 characters from 'system.index')
#boundary$timestamp <- substr(boundary$system.index, 1, 8)

# Step 2: Convert the extracted date to Date class in R
boundary$timestamp <- as.Date(boundary$timestamp, format = "%Y%m%d")

# Step 3: Extract the year and month from timestamp
boundary$year_month <- format(boundary$timestamp, "%Y-%m")

##extract just the year
boundary$year <- format(boundary$timestamp, "%Y")


# 
data <- boundary 


# Group by year_month and cover, and calculate the mean for each band
grouped_data <- data %>%
  group_by(year_month, cover) %>%
  summarise(across(starts_with('B'), mean, na.rm = TRUE)) 


# Reshape the data for plotting
long_data <- grouped_data %>%
  pivot_longer(cols = starts_with('B'), names_to = 'Band', values_to = 'Mean_Value') |>
  dplyr::filter(cover == 3 | cover == 7) |>
  dplyr::filter(Band == "B7")

# Plot the trends, considering year and month
ggplot(long_data, aes(x = year_month, y = Mean_Value, color = cover, group = cover)) +
  geom_line() +
  geom_point() +
  labs(title = 'Time Series Trends by Cover, Year, and Month',
       x = 'Year-Month',
       y = 'Mean Value') +
  theme_minimal() +
  #facet_grid(~cover) +
  theme(legend.position = 'right') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis l




########################################

# Verify the changes
str(data)

df <- data |>
  group_by(.geo, year_month) |>
  # distinct(.geo, year_month_1, .keep_all = TRUE) |>
  filter(cover == 7) # | cover == 3




# Compute vegetation indices
df <- df %>%
  mutate(
    NDVI = (B5 - B4) / (B5 + B4))

# Assuming 'geometry' identifies the location, filter by the first value of 'geometry'
df_2 <- df[df$.geo == df$.geo[100], ]


str(df_2)

# Step 2: Convert the extracted date to Date class in R
df_2$year_month <- as.Date(df_2$year_month, format = "%Y%m")


time_series <- ts(df_2$B7, start=c(2017, 1), end = c(2024, 12), frequency = 12)  # assuming monthly data from 2021

# Interpolate missing values
time_series_imputed <- na.approx(time_series)

#  annual streamflow of the Nile River    
out = beast(time_series_imputed, tcp.minmax =c(0,3), season='harmonic') #  'none': trend-only data without seasonlaity   
print(out)                   
plot(out, show.tip.label = TRUE, main = "Trend and abrupt change in NDVI value")



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
