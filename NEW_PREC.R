library(sf)
library(raster)
library(tidyverse)


###path to the database
path_ds <- "/scratch/ope4/MERGE/OLD - CONUS_Region3_AllYears.gdb"
###check number of layers in the database
layers <- st_layers(path_ds)

layer_points <- "DAMAGE_POINTS_FLAT_Allyears_CONUS_Rgn3"
damage_points <- st_read(path_ds, layer = layer_points)


##load the boundary box shape file
boundary <- read_sf("/scratch/ope4/MERGE/SHP/sw_biotic_communities_WGS84_clipped_ARIZONA.shp")
bounding_box <- st_bbox(boundary)
# Convert the bounding box to an sf object for plotting
boundary_extent_sf <- st_as_sfc(bounding_box)

if (st_crs(damage_points) != st_crs(boundary)) {
  damage_points <- st_transform(damage_points, st_crs(boundary))
}

# Now try cropping again
damage_points_cropped <- st_crop(damage_points, boundary)


unique(damage_points$SURVEY_YEAR)












###########################################################################################################################

# Ensure points are in WGS84 (longitude, latitude)
damage_points_cropped <- st_transform(damage_points_cropped, crs = 4326)

# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015:2021) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_cropped, "Spatial")
  
  # Extract raster values and convert to data frame
  extracted_values_year <- raster::extract(mean_raster_year_prec_cropped, points_sp) %>%
    as.data.frame() %>%
    setNames("precipitation") %>%
    mutate(year = year)
  
  # Store extracted values in list
  extracted_values[[as.character(year)]] <- extracted_values_year
  cat("Extracted values for year", year, "\n")
}


# Merge all years into a wide format and exclude the "year" column
extracted_values_wide <- Reduce(function(x, y) full_join(x, y, by = "ID"),
                                lapply(names(extracted_values), function(year) {
                                  df <- extracted_values[[year]] %>%
                                    rename(!!paste0("prec_", year) := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec <- cbind(damage_points_cropped, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec)
str(damage_points_with_prec)

###########################################################################################################################




###########################################################################################################################

damage_points_cropped <- damage_points_with_prec

folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015:2021) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_cropped, "Spatial")
  
  # Extract raster values and convert to data frame
  extracted_values_year <- raster::extract(mean_raster_year_tmax_cropped, points_sp) %>%
    as.data.frame() %>%
    setNames("tmax") %>%
    mutate(year = year)
  
  # Store extracted values in list
  extracted_values[[as.character(year)]] <- extracted_values_year
  cat("Extracted values for year", year, "\n")
}

# Merge all years into a wide format and exclude the "year" column
extracted_values_wide <- Reduce(function(x, y) full_join(x, y, by = "ID"),
                                lapply(names(extracted_values), function(year) {
                                  df <- extracted_values[[year]] %>%
                                    rename(!!paste0("tmax_", year) := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax <- cbind(damage_points_cropped, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax)
str(damage_points_with_tmax)

##########################################################################################################################





###########################################################################################################################

damage_points_cropped <- damage_points_with_tmax

# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015:2021) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_cropped, "Spatial")
  
  # Extract raster values and convert to data frame
  extracted_values_year <- raster::extract(mean_raster_year_tmin_cropped, points_sp) %>%
    as.data.frame() %>%
    setNames("tmin") %>%
    mutate(year = year)
  
  # Store extracted values in list
  extracted_values[[as.character(year)]] <- extracted_values_year
  cat("Extracted values for year", year, "\n")
}

# Merge all years into a wide format and exclude the "year" column
extracted_values_wide <- Reduce(function(x, y) full_join(x, y, by = "ID"),
                                lapply(names(extracted_values), function(year) {
                                  df <- extracted_values[[year]] %>%
                                    rename(!!paste0("tmin_", year) := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin <- cbind(damage_points_cropped, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin)
str(damage_points_with_tmin)

############################################################################################################################


damage_points_cropped <- damage_points_with_tmin



# Extract the coordinates from the geometry column
coordinates <- st_coordinates(damage_points_cropped)

# Bind the coordinates to the original data frame (excluding geometry)
damage_points_cropped <- cbind(st_drop_geometry(damage_points_cropped), coordinates)

# Write the data to CSV (including the XY coordinates)
write.csv(damage_points_cropped, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped.csv", row.names = FALSE)



# Export the data as a CSV file
#write.csv(damage_points_cropped, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped.csv", row.names = FALSE)


# Plot points for each year colored by a WorldClim variable (e.g., precipitation)

ggplot(damage_points_cropped) +
  geom_sf(aes(color = tmin_2018)) +
  facet_wrap(~ SURVEY_YEAR) +
  scale_color_viridis_c(option = "plasma", name = "Precipitation") +
  theme_minimal() +
  labs(title = "WorldClim Data for Points in Each Year",
       subtitle = "Colored by Precipitation",
       x = "Longitude",
       y = "Latitude")




###########################################################################################################################



data <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped.csv")

str(data)

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_15.tif")


#filter the damage_points_cropped for only DCA by beetles
damage_points_cropped_2 <- data %>%
  filter(grepl("beetle", DCA_COMMON_NAME, ignore.case = TRUE))


# Now, convert the dataframe to an sf object and assign CRS
damage_points_cropped_2 <- sf::st_as_sf(damage_points_cropped_2, coords = c("X", "Y"), crs = crs(raster_stack))


# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)


#unique(damage_points_cropped_2$STATUS)
# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)

if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2015


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2015)


table(extracted_values_2$status_2015)

#####################################################################################################################




damage_points_cropped_15 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_15, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_15.gpkg", append=FALSE)

# Write the data to CSV (including the XY coordinates)
#write.csv(damage_points_cropped_15, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_15.csv", row.names = FALSE)






##########################################################################################################################

#damage_points_cropped_2 <- extracted_values_2

#damage_points_cropped_2 <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_15.csv")


damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_15.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_16.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2016


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2016)


table(extracted_values_2$status_2016)

############################################################################################################################






damage_points_cropped_16 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_16, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_16.gpkg",append=FALSE)









############################################################################################################################

damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_16.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_17.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2017


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2017)


table(extracted_values_2$status_2017)

############################################################################################################################





damage_points_cropped_17 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_17, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_17.gpkg", append=FALSE)





############################################################################################################################

damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_17.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_18.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2018


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2018)


table(extracted_values_2$status_2018)

############################################################################################################################






damage_points_cropped_18 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_18, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_18.gpkg", append=FALSE)






############################################################################################################################

damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_18.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_19.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2019


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2019)


table(extracted_values_2$status_2019)

############################################################################################################################



damage_points_cropped_19 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_19, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_19.gpkg", append=FALSE)







############################################################################################################################

damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_19.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_20.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2020


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2020)


table(extracted_values_2$status_2020)

############################################################################################################################




damage_points_cropped_20 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_20, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_20.gpkg", append=FALSE)




############################################################################################################################

damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_20.gpkg")

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_21.tif")

# Get the number of bands
num_bands <- nlayers(raster_stack)
print(num_bands)

class(damage_points_cropped_2)
#unique(damage_points_cropped_2$status_2015)


# Check CRS of points and raster
crs_points <- st_crs(damage_points_cropped_2)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(damage_points_cropped_2, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(damage_points_cropped_2)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)

extracted_values_2 <- cbind(extracted_values_2, damage_points_cropped_2)


# Define a range of survey years
#years <- 2015:2021
year <- 2021


# Define the name of the new column
col_name <- paste0("status_", year)

# Add the new column based on SURVEY_YEAR and year comparison
extracted_values_2 <- extracted_values_2 %>%
  dplyr::mutate(!!sym(col_name) := ifelse(SURVEY_YEAR == year, 1, 0))


x2015 <- extracted_values_2 |>
  filter(SURVEY_YEAR == 2021)


table(extracted_values_2$status_2021)

############################################################################################################################



damage_points_cropped_21 <- extracted_values_2

# Export as Shapefile
st_write(damage_points_cropped_21, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_21.gpkg", append=FALSE)




###########################################################################################################


damage_points_cropped_2 <- st_read("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_21.gpkg")



colnames(damage_points_cropped_2)


damage_points_cropped_ML <- damage_points_cropped_2 |>
 dplyr:: select(
    "LANDSAT_21_1", "LANDSAT_21_2", "LANDSAT_21_3", "LANDSAT_21_4", "LANDSAT_21_5", "LANDSAT_21_6", 
    "LANDSAT_21_7", "LANDSAT_21_8", "LANDSAT_21_9", "LANDSAT_20_1", "LANDSAT_20_2", "LANDSAT_20_3", 
    "LANDSAT_20_4", "LANDSAT_20_5", "LANDSAT_20_6", "LANDSAT_20_7", "LANDSAT_20_8", "LANDSAT_20_9", 
    "LANDSAT_19_1", "LANDSAT_19_2", "LANDSAT_19_3", "LANDSAT_19_4", "LANDSAT_19_5", "LANDSAT_19_6", 
    "LANDSAT_19_7", "LANDSAT_19_8", "LANDSAT_19_9", "LANDSAT_18_1", "LANDSAT_18_2", "LANDSAT_18_3", 
    "LANDSAT_18_4", "LANDSAT_18_5", "LANDSAT_18_6", "LANDSAT_18_7", "LANDSAT_18_8", "LANDSAT_18_9", 
    "LANDSAT_17_1", "LANDSAT_17_2", "LANDSAT_17_3", "LANDSAT_17_4", "LANDSAT_17_5", "LANDSAT_17_6", 
    "LANDSAT_17_7", "LANDSAT_17_8", "LANDSAT_17_9", "LANDSAT_16_1", "LANDSAT_16_2", "LANDSAT_16_3", 
    "LANDSAT_16_4", "LANDSAT_16_5", "LANDSAT_16_6", "LANDSAT_16_7", "LANDSAT_16_8", "LANDSAT_16_9", 
    "LANDSAT_15_1", "LANDSAT_15_2", "LANDSAT_15_3", "LANDSAT_15_4", "LANDSAT_15_5", "LANDSAT_15_6", 
    "LANDSAT_15_7", "LANDSAT_15_8", "LANDSAT_15_9", 
    "prec_2016", "prec_2017", "prec_2018", "prec_2019", "prec_2020", "prec_2021", 
    "tmax_2015", "tmax_2016", "tmax_2017", "tmax_2018", "tmax_2019", "tmax_2020", "tmax_2021", 
    "tmin_2015", "tmin_2016", "tmin_2017", "tmin_2018", "tmin_2019", "tmin_2020", "tmin_2021", 
    "status_2015", "status_2016", "status_2017", "status_2018", "status_2019", "status_2020",    
    "status_2021", "geom", "SURVEY_YEAR"  )





##export out the CSV

write.csv(damage_points_cropped_ML, "/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_ML.csv")

