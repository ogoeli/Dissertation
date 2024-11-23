library(sf)
library(sp)
library(tidyverse)



data <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped.csv")

str(data)

####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_15.tif")


#filter the damage_points_cropped for only DCA by beetles
damage_points_cropped_2 <- data %>%
  filter(grepl("beetle", DCA_COMMON_NAME, ignore.case = TRUE))


# Now, convert the dataframe to an sf object and assign CRS
damage_points_cropped_2 <- sf::st_as_sf(damage_points_cropped_2, coords = c("X", "Y"), crs = crs(raster_stack))



##load the boundary box shape file
boundary <- read_sf("/scratch/ope4/MERGE/SHP/sw_biotic_communities_WGS84_clipped_ARIZONA.shp")
bounding_box <- st_bbox(boundary)
# Convert the bounding box to an sf object for plotting
boundary_extent_sf <- st_as_sfc(bounding_box)





#######################################################################################################################
damage_points_cropped_15 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2015)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_15, dist = 3)  # adjust 'dist' as needed

# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_15), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_15[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_15))

# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_15$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_15$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_15 <- damage_points_cropped_15 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
 # rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_15, random_points_sf)


combined_data$SURVEY_YEAR <- 2015


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_15.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_15 <- cbind(extracted_values_2, combined_data)




# Now, convert the dataframe to an sf object and assign CRS
extracted_values_15 <- sf::st_as_sf(extracted_values_15, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_15, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_15 <- cbind(extracted_values_15, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_15)
str(damage_points_with_prec_15)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_15, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_15 <- cbind(damage_points_with_prec_15, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_15)
str(damage_points_with_tmax_15)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2015) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_15, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_15 <- cbind(damage_points_with_tmax_15, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_15)
str(damage_points_with_tmin_15)

class(damage_points_with_tmin_15)



year_2015 <- damage_points_with_tmin_15


# Export as Shapefile
st_write(year_2015, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2015.gpkg", append=FALSE)

#########################################################################################################################







#######################################################################################################################
damage_points_cropped_16 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2016)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_16, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_16), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_16[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_16))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_16$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_16$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_16 <- damage_points_cropped_16 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_16, random_points_sf)


combined_data$SURVEY_YEAR <- 2016


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_16.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_16 <- cbind(extracted_values_2, combined_data)





# Now, convert the dataframe to an sf object and assign CRS
extracted_values_16 <- sf::st_as_sf(extracted_values_16, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2016) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_16, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_16 <- cbind(extracted_values_16, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_16)
str(damage_points_with_prec_16)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2016) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_16, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_16 <- cbind(damage_points_with_prec_16, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_16)
str(damage_points_with_tmax_16)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2016) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_16, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_16 <- cbind(damage_points_with_tmax_16, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_16)
str(damage_points_with_tmin_16)

class(damage_points_with_tmin_16)



year_2016 <- damage_points_with_tmin_16


# Export as Shapefile
st_write(year_2016, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2016.gpkg", append=FALSE)



#########################################################################################################################








#######################################################################################################################
damage_points_cropped_17 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2017)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_17, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_17), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_17[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_17))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_17$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_17$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_17 <- damage_points_cropped_17 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_17, random_points_sf)


combined_data$SURVEY_YEAR <- 2017


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_17.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_17 <- cbind(extracted_values_2, combined_data)





# Now, convert the dataframe to an sf object and assign CRS
extracted_values_17 <- sf::st_as_sf(extracted_values_17, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2017) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_17, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_17 <- cbind(extracted_values_17, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_17)
str(damage_points_with_prec_17)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2017) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_17, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_17 <- cbind(damage_points_with_prec_17, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_17)
str(damage_points_with_tmax_17)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2017) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_17, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_17 <- cbind(damage_points_with_tmax_17, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_17)
str(damage_points_with_tmin_17)

class(damage_points_with_tmin_17)



year_2017 <- damage_points_with_tmin_17


# Export as Shapefile
st_write(year_2017, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2017.gpkg", append=FALSE)

#########################################################################################################################






#######################################################################################################################
damage_points_cropped_18 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2018)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_18, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_18), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_18[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_18))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_18$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_18$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_18 <- damage_points_cropped_18 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_18, random_points_sf)


combined_data$SURVEY_YEAR <- 2018


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_18.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_18 <- cbind(extracted_values_2, combined_data)





# Now, convert the dataframe to an sf object and assign CRS
extracted_values_18 <- sf::st_as_sf(extracted_values_18, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2018) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_18, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_18 <- cbind(extracted_values_18, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_18)
str(damage_points_with_prec_18)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2018) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_18, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_18 <- cbind(damage_points_with_prec_18, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_18)
str(damage_points_with_tmax_18)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2018) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_18, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_18 <- cbind(damage_points_with_tmax_18, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_18)
str(damage_points_with_tmin_18)

class(damage_points_with_tmin_18)



year_2018 <- damage_points_with_tmin_18


# Export as Shapefile
st_write(year_2018, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2018.gpkg", append=FALSE)

#########################################################################################################################









#######################################################################################################################
damage_points_cropped_19 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2019)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_19, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_19), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_19[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_19))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_19$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_19$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_19 <- damage_points_cropped_19 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_19, random_points_sf)


combined_data$SURVEY_YEAR <- 2019


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_19.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_19 <- cbind(extracted_values_2, combined_data)





# Now, convert the dataframe to an sf object and assign CRS
extracted_values_19 <- sf::st_as_sf(extracted_values_19, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2019) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_19, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_19 <- cbind(extracted_values_19, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_19)
str(damage_points_with_prec_19)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2019) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_19, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_19 <- cbind(damage_points_with_prec_19, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_19)
str(damage_points_with_tmax_19)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2019) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_19, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_19 <- cbind(damage_points_with_tmax_19, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_19)
str(damage_points_with_tmin_19)

class(damage_points_with_tmin_19)



year_2019 <- damage_points_with_tmin_19


# Export as Shapefile
st_write(year_2019, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2019.gpkg", append=FALSE)

#########################################################################################################################








#######################################################################################################################
damage_points_cropped_20 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2020)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_20, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_20), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_20[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_20))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_20$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_20$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_20 <- damage_points_cropped_20 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_20, random_points_sf)


combined_data$SURVEY_YEAR <- 2020


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_20.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_20 <- cbind(extracted_values_2, combined_data)




# Now, convert the dataframe to an sf object and assign CRS
extracted_values_20 <- sf::st_as_sf(extracted_values_20, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2020) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_20, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_20 <- cbind(extracted_values_20, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_20)
str(damage_points_with_prec_20)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2020) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_20, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_20 <- cbind(damage_points_with_prec_20, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_20)
str(damage_points_with_tmax_20)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2020) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_20, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_20 <- cbind(damage_points_with_tmax_20, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_20)
str(damage_points_with_tmin_20)

class(damage_points_with_tmin_20)



year_2020 <- damage_points_with_tmin_20


# Export as Shapefile
st_write(year_2020, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2020.gpkg", append=FALSE)

#########################################################################################################################









#######################################################################################################################
damage_points_cropped_21 <- damage_points_cropped_2 |>
  filter(SURVEY_YEAR ==2021)

# Step 1: Create a buffer around each geometry point (outside)
buffered_points <- sf::st_buffer(damage_points_cropped_21, dist = 3)  # adjust 'dist' as needed



# Step 2: Generate 10 random points outside the boundary for each geometry point
random_points_list <- lapply(1:nrow(damage_points_cropped_21), function(i) {
  buffer_geom <- buffered_points[i, ]
  original_point <- damage_points_cropped_21[i, ]
  
  # Exclude original geometry and validate buffer
  buffer_without_original <- st_difference(buffer_geom, original_point)
  if (!st_is_empty(buffer_without_original) && st_is_valid(buffer_without_original)) {
    # Sample points and exclude those within the original geometry
    st_sample(buffer_without_original, size = 10, type = "random") %>%
      .[!st_intersects(., original_point, sparse = FALSE)]
  } else {
    NULL
  }
})

# Combine all valid points into a single sf object
random_points_sf <- do.call(c, random_points_list) %>%
  st_sf(geometry = ., crs = st_crs(damage_points_cropped_21))


# Step 4: Add the 'status' column
# For the original points
damage_points_cropped_21$status <- 1

# For the random points
random_points_sf$status <- 0

# Step 5: Combine the original data with the random points
# Add relevant columns from the original data (like DAMAGE_POINT_ID)
random_points_sf$DAMAGE_POINT_ID <- rep(damage_points_cropped_21$DAMAGE_POINT_ID, each = 10)

damage_points_cropped_21 <- damage_points_cropped_21 |>
  select(geometry, status, DAMAGE_POINT_ID )

#random_points_sf <- random_points_sf %>% 
#  rename(geometry = x)

# Combine both original points and random points
combined_data <- rbind(damage_points_cropped_21, random_points_sf)


combined_data$SURVEY_YEAR <- 2021


####input your raster file
raster_stack <- brick("/scratch/ope4/MERGE/RASTER_TIFF/LANDSAT_21.tif")





# Check CRS of points and raster
crs_points <- st_crs(combined_data)  # or crs(points) if using sp package
crs_raster <- crs(raster_stack)


if (!identical(crs_points, crs_raster)) {
  # Transform points to match raster CRS if they differ
  points <- st_transform(combined_data, crs_raster)  # or spTransform for sp objects
}


# Use nearest neighbor interpolation to extract values
extracted_values <- raster::extract(raster_stack, points, method = 'bilinear')  # or 'simple' if preferred


nrow(extracted_values)
nrow(combined_data)

#extracted_values <- cbind(extracted_values, damage_points_cropped_2)

# Ensure extracted_values is a data frame
extracted_values_2 <- as.data.frame(extracted_values)


extracted_values_21 <- cbind(extracted_values_2, combined_data)




# Now, convert the dataframe to an sf object and assign CRS
extracted_values_21 <- sf::st_as_sf(extracted_values_21, crs = crs(raster_stack))


# Define folder path for WorldClim data (precipitation)
folder_path_prec <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_prec_2010-2019/"
all_files_prec <- list.files(folder_path_prec, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2021) {
  
  # Select files for specified months and year
  files_05_to_09_prec <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_prec, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_prec <- calc(stack(lapply(files_05_to_09_prec, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_prec_cropped <- mask(crop(mean_raster_year_prec, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(extracted_values_21, "Spatial")
  
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
                                    rename(!!paste0("prec") := precipitation) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_prec_21 <- cbind(extracted_values_21, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_prec_21)
str(damage_points_with_prec_21)







folder_path_tmax <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmax_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmax <- list.files(folder_path_tmax, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2021) {
  
  # Select files for specified months and year
  files_05_to_09_tmax <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmax, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmax <- calc(stack(lapply(files_05_to_09_tmax, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmax_cropped <- mask(crop(mean_raster_year_tmax, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_prec_21, "Spatial")
  
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
                                    rename(!!paste0("tmax") := tmax) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)


# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmax_21 <- cbind(damage_points_with_prec_21, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmax_21)
str(damage_points_with_tmax_21)







# Define the folder path for WorldClim data FOR PREC
folder_path_tmin <- "/scratch/ope4/MERGE/wc2.1_cruts4.06_2.5m_tmin_2010-2019/"

# List all TIFF files for precipitation (adjust for other variables if needed)
all_files_tmin <- list.files(folder_path_tmin, pattern = "\\.tif$", full.names = TRUE)

# Convert boundary to a Spatial object for raster operations
boundary_sp <- as(boundary, "Spatial")

# Initialize list to store extracted values
extracted_values <- list()

# Loop through years and extract values
for (year in 2021) {
  
  # Select files for specified months and year
  files_05_to_09_tmin <- grep(paste0(year, "-0[5-9]\\.tif"), all_files_tmin, value = TRUE)
  
  # Calculate mean precipitation for selected months
  mean_raster_year_tmin <- calc(stack(lapply(files_05_to_09_tmin, raster)), mean)
  
  # Crop and mask the raster to the boundary
  mean_raster_year_tmin_cropped <- mask(crop(mean_raster_year_tmin, boundary_sp), boundary_sp)
  
  # Convert points to Spatial for raster extraction
  points_sp <- as(damage_points_with_tmax_21, "Spatial")
  
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
                                    rename(!!paste0("tmin") := tmin) %>%
                                    select(-year) # Remove the "year" column here
                                  df$ID <- 1:nrow(df)  # Add a unique ID column for joining
                                  df
                                })
) %>% select(-ID)  # Remove the ID column from the final result

# Display the structure of the wide data frame
str(extracted_values_wide)

# Bind `damage_points_cropped` with the wide-format extracted values
damage_points_with_tmin_21 <- cbind(damage_points_with_tmax_21, extracted_values_wide)

# View or save the wide-format data frame
head(damage_points_with_tmin_21)
str(damage_points_with_tmin_21)

class(damage_points_with_tmin_21)



year_2021 <- damage_points_with_tmin_21


# Export as Shapefile
st_write(year_2021, "/scratch/ope4/MERGE/BUFFER_EXPORT/year_2021.gpkg", append=FALSE)

#########################################################################################################################



year_2015 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2015.gpkg")

year_2016 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2016.gpkg")

year_2017 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2017.gpkg")

year_2018 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2018.gpkg")

year_2019 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2019.gpkg")

year_2020 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2020.gpkg")

year_2021 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2021.gpkg")






combined_dataset_BUFFER <- rbind(year_2015, year_2016, year_2017, year_2018, year_2019, year_2020, year_2021)



# Export as Shapefile
st_write(combined_dataset_BUFFER, "/scratch/ope4/MERGE/BUFFER_EXPORT/combined_dataset_BUFFER.gpkg", append=FALSE)
