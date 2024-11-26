library(raster)
library(tidyverse)
library(sf)

###LOAD DATA
###path to the database
path_ds <- "/scratch/ope4/MERGE/OLD - CONUS_Region3_AllYears.gdb"
###check number of layers in the database
layers <- st_layers(path_ds)

layer_points <- "DAMAGE_POINTS_FLAT_Allyears_CONUS_Rgn3"
damage_points <- st_read(path_ds, layer = layer_points)
#head(damage_points)

##load the boundary box shape file
boundary <- read_sf("/scratch/ope4/MERGE/SHP/sw_biotic_communities_WGS84_clipped_ARIZONA.shp")
bounding_box <- st_bbox(boundary)
# Convert the bounding box to an sf object for plotting
boundary_extent_sf <- st_as_sfc(bounding_box)

if (st_crs(damage_points) != st_crs(boundary)) {
  damage_points <- st_transform(damage_points, st_crs(boundary))
}

# Now try cropping again
#damage_points_cropped <- st_crop(damage_points, boundary)


####RASTER FILE
# Load the raster stack and the points file
raster_stack <- stack("/scratch/ope4/MERGE/2015_TIF/Landsat_15.tif")
#plot(raster_stack)

# added by Allie
damage_points_cropped <- st_crop(damage_points, raster_stack)


# Filter for polygons where 'DCA_COMMON_NAME' contains the word "beetle"
damage_points_cropped_15 <- damage_points_cropped %>%
  filter(grepl("beetle", DCA_COMMON_NAME, ignore.case = TRUE)) |>
filter(SURVEY_YEAR == 2016)
#str(damage_points_cropped_15)


###the crs return NA so i am reprojecting it.
# raster::crs(raster_stack) <- sf::st_crs(4326)$proj4string

###show crs
print(raster::crs(raster_stack))  # CRS of raster stack
print(sf::st_crs(damage_points_cropped_15))  # CRS of points

##crop the raster
#raster_stack <- raster::crop(x = raster_stack, y = damage_points_cropped)
#plot(raster_stack)

##select the bands since the python script didnt do it 
# Assuming your raster_stack is a RasterStack or RasterBrick
raster_stack <- raster::subset(raster_stack, c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7", "SR_QA_AEROSOL", "ST_B10"))


# Ensure points are a spatial object if not already
# Extract coordinates from the SHAPE column in the sf object
points <- sf::st_coordinates(damage_points_cropped$SHAPE)
#print(points)
#points <- extent(-114.8165,31.33218,-109.0452,37.00427)
# Extract pixel values at each point for all 12 bands
extracted_values <- raster::extract(raster_stack, points)

# Combine extracted values with the original points data
points_with_values <- cbind(points, extracted_values)

# Convert points_with_values to a data frame
points_with_values <- as.data.frame(points_with_values)
#as.factor(points_with_values$SR_B4)
# Display the result
#print(points_with_values)

# Step 2: Define the bounding box as a spatial extent
bounding_box <- st_as_sfc(st_bbox(raster_stack))
#bounding_box <- st_as_sfc(st_bbox(damage_points_cropped_15))

# Convert the bounding box to a SpatialPolygons object
bounding_box_sp <- as(bounding_box, "Spatial")

# Step 3: Create an empty raster with the bounding box extent
raster_template <- raster(extent(bounding_box_sp), res = 0.01)  # Adjust resolution as needed

# Step 4: Rasterize the points to create infected (1) raster
# Convert damage_points to a spatial object if it’s an sf object
if (inherits(damage_points_cropped_15, "sf")) {
  damage_points_sp <- as(damage_points_cropped_15, "Spatial")
} else {
  damage_points_sp <- damage_points_cropped_15
}
infected_raster <- rasterize(damage_points_sp, raster_template, field = 1)

# Step 5: Fill NA values with 0 (non-infected areas)
infected_raster[is.na(infected_raster)] <- 0

# Convert raster to DataFrame
infected_df <- as.data.frame(infected_raster, xy = TRUE)

# Add a column for year (2015)
infected_df$year <- 2015
as.factor(infected_df$layer)
# Count occurrences of 0 and 1 in the layer column
count_df <- infected_df %>%
  group_by(layer) %>%
  summarise(count = n())
# Create a status column based on raster values
infected_df$STATUS <- ifelse(infected_df$layer == 1, 1, 0)

# Remove the layer column
infected_df <- infected_df %>% 
  dplyr::select(x, y, year, STATUS)


# Combine with points_with_values to get final dataset
infected_df <- sf::st_as_sf(infected_df, coords = c("x", "y"), crs = 4326)
points_with_values <- sf::st_as_sf(points_with_values,  coords = c("X", "Y"),crs = 4326)
#points_with_values <- sf::st_as_sf(points_with_values, coords = points)
# Join them
df_join <- sf::st_join(infected_df, points_with_values)
# View
df_join


joined_data <- st_join(infected_df, points_with_values, join = st_intersects)
unique(joined_data$SR_B2)





# Create the plot
# Create the plot
ggplot(data = joined_data) +
  geom_sf(aes(color = factor(STATUS, levels = c(0, 1), labels = c("uninfected", "infected")))) +  # Use factor with labels
  scale_color_manual(values = c("uninfected" = "black", "infected" = "white")) +  # Customize colors
  labs(color = "Status", title = "Infected vs Uninfected Points") +
  theme_minimal() +
  theme(legend.position = "right")  # Position the legend



# Filter the dataframe for rows where STATUS is 1
infected_pixel_values <- joined_data %>%
  filter(STATUS == 1)

print(infected_pixel_values)

hist(raster_stack[])
head(r[],200)




damage_points_cropped <- damage_points_cropped$SHAPE

crs(damage_points_cropped) <- crs(raster_stack)

values <- extract(raster_stack, damage_points_cropped)




