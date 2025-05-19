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



data <- read.csv('/scratch/ope4/Downloads//DATA/CSV/Band_Values_By_Class_PLANET_2_PERCENT.csv')

str(data)

colnames(data)[colnames(data) == 'grid_code'] <- 'cover'

# Randomly select 50% of rows
#data <- data %>%
#  slice_sample(prop = 0.1)


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
tree_mask <- brick("/scratch/ope4/Downloads//DATA/TIF/Tree_NoTree_Mark.tif")


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
  dplyr::filter(cover_code >= 5) ##height greater than or equal to 5

# Step 1: Extract the date part (first 8 characters from 'system.index')
boundary$timestamp <- substr(boundary$system.index, 1, 8)

# Step 2: Convert the extracted date to Date class in R
boundary$timestamp <- as.Date(boundary$timestamp, format = "%Y%m%d")

# Step 3: Extract the year and month from timestamp
boundary$year_month_1 <- format(boundary$timestamp, "%Y-%m")

# Extract year and month from the system_index column
data <- boundary %>%
  mutate(
    year = substr(system.index, 1, 4),
    month = substr(system.index, 5, 6),
    year_month = paste(year, month, sep = "-")
  )


# Group by year_month and cover, and calculate the mean for each band
grouped_data <- data %>%
  group_by(year_month, cover) %>%
  summarise(across(starts_with('B'), mean, na.rm = TRUE)) 


# Reshape the data for plotting
long_data <- grouped_data %>%
  pivot_longer(cols = starts_with('B'), names_to = 'Band', values_to = 'Mean_Value') |>
  dplyr::filter(cover == 3 | cover == 1) |>
  dplyr::filter(Band == "B12")

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





#######################################


# Compute vegetation indices
data <- data %>%
  mutate(
    NDWI = (B3 - B8) / (B3 + B8),
    
    # Updated DSWI formula: (NIR + Green) / (SWIRI + Red)
    # Sentinel-2 Bands Mapping:
    # - NIR    → B8  (835.1 nm)
    # - Green  → B3  (560 nm)
    # - SWIRI  → B11 (1613.7 nm)
    # - Red    → B4  (664.5 nm)
    DSWI = (B8 + B3) / (B11 + B4),
    
    NGRDI = (B3 - B4) / (B3 + B4),
    RDI = B8 / B11,
    GLI = (2 * B3 - B4 - B2) / (2 * B3 + B4 + B2),
    NDRE2 = (B8A - B5) / (B8A + B5),
    PBI = B11 / B8,
    NDVI = (B8 - B4) / (B8 + B4),
    GNDVI = (B8 - B3) / (B8 + B3),
    CIG = (B8 / B3) - 1,
    CVI = (B8 * B4) / (B3^2),
    NDRE3 = (B7 - B5) / (B7 + B5)
  )

# View the first few rows to confirm calculations
head(data)

# Compute DRS (Drought Stress)
data <- data %>%
  mutate(
    DRS = sqrt((B4^2) + (B11^2))  # Red is B4 and SWIR is B11
  )

# Compute DRS min and max (you may want to do this over a time period or region)
DRS_min <- min(data$DRS, na.rm = TRUE)
DRS_max <- max(data$DRS, na.rm = TRUE)

# Compute NDRS
data <- data %>%
  mutate(
    NDRS = (DRS - DRS_min) / (DRS_max - DRS_min)
  )

# View the first few rows to confirm calculations
head(data)




# Group by year_month and cover, and calculate the mean for each band
grouped_data <- data %>%
  group_by(year_month, cover) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) |>
  select(-system.index, -geometry , -year, -month, -TREE_MASK) |>
  filter(cover == 1)


str(grouped_data)

# Reshape the data to long format focusing only on the indices
indices_data <- grouped_data %>%
  pivot_longer(cols = c(NDWI, DSWI, NGRDI, RDI, GLI, NDRE2, PBI, NDVI, GNDVI,  CVI, NDRE3,  NDRS),  #CIG,
               names_to = 'Index', 
               values_to = 'Value') 



# Plot the vegetation indices over time
ggplot(indices_data, aes(x = year_month, y = Value, color = Index, group = Index)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Vegetation Indices Over Time",
       x = "Year-Month",
       y = "Index Value",
       color = "Index") +
  theme_minimal() +
  scale_y_continuous(limits = c(-1, 1),  # Adjust these limits to fit your data range
                     breaks = seq(-1, 1, by = 0.2),  # Adjust the breaks
                     labels = scales::number_format(accuracy = 0.1)) +  # Adjust the format if needed
  theme_minimal()

