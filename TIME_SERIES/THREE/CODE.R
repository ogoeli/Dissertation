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



data_1 <- read.csv('/scratch/ope4/Downloads/DATA/CSV/THREE/Band_Values_POLY_1-001.csv')

data_3 <- read.csv('/scratch/ope4/Downloads/DATA/CSV/THREE/Band_Values_POLY_3-001.csv')

str(data)

colnames(data_1)[colnames(data_1) == 'classification'] <- 'cover'

colnames(data_3)[colnames(data_3) == 'classification'] <- 'cover'



poly_1 <- data_1 |>
  filter (cover == 4)

poly_3 <- data_3 |>
  filter (cover == 4)

data <- rbind(poly_1, poly_3)

data <- read.csv('/scratch/ope4/Downloads/DATA/CSV/THREE/Band_Values_POLY_Monica.csv')




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
  dplyr::filter(TREE_MASK >= 1) ##height greater than or equal to 5

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

# Calculate indices
data$NDVI <- (data$B8 - data$B4) / (data$B8 + data$B4)
data$NDWI <- (data$B3 - data$B8) / (data$B3 + data$B8)
data$RENDVI <- (data$B8 - data$B5) / (data$B8 + data$B5)
data$CCCI <- data$NDVI * data$RENDVI

# Group by year_month and cover, and calculate the mean for each band
grouped_data <- data %>%
  group_by(year_month, cover) %>%
  summarise(across(starts_with('B'), mean, na.rm = TRUE)) #%>%
  #summarise(across(matches('^(N|CC)'), mean, na.rm = TRUE)) 

grouped_data$year_month <- as.Date(paste0(grouped_data$year_month, "-01"))

# Then apply log() only on numeric columns (bands)
#grouped_data[ , sapply(grouped_data, is.numeric)] <- log(grouped_data[ , sapply(grouped_data, is.numeric)])

# Reshape the data for plotting
long_data <- grouped_data %>%
  pivot_longer(cols = starts_with('B'), names_to = 'Band', values_to = 'Mean_Value') 


my_colors <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
  "#e6ab02", "#a6761d", "#666666", "#a6cee3", "#1f78b4",
  "#b2df8a", "#33a02c", "#fb9a99"
)


# Plot the trends, considering year and month
ggplot(long_data, aes(x = year_month, y = Mean_Value, color = factor(Band), group = Band)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = my_colors) +
  #scale_color_brewer(palette = "Set1") +  # or "Dark2", "Paired", etc.
  labs(title = 'Time Series Trends by Cover, Year, and Month',
       x = 'Year-Month',
       y = 'Mean Value') +
  theme_minimal() +
  #facet_grid(~cover) +
  theme(legend.position = 'right') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis l




#---------------------------------------------------

# Group by year_month and cover, and calculate the mean for each band
grouped_data <- data %>%
  group_by(year_month, cover) %>%
  #summarise(across(everything(), mean, na.rm = TRUE)) |>
  summarise(across(matches('^(N|CC|E)'), mean, na.rm = TRUE)) 


str(grouped_data)

##select year
grouped_data$year_month <- as.Date(paste0(grouped_data$year_month, "-01"))

#absolute of NDWI
#grouped_data[ , sapply(grouped_data, is.numeric)] <- abs(grouped_data[ , sapply(grouped_data, is.numeric)])

# Then apply log() only on numeric columns (bands)
#grouped_data[ , sapply(grouped_data, is.numeric)] <- log(grouped_data[ , sapply(grouped_data, is.numeric)])

#absolute value of all col
#grouped_data[ , sapply(grouped_data, is.numeric)] <- abs(grouped_data[ , sapply(grouped_data, is.numeric)])

# Reshape the data to long format focusing only on the indices
indices_data <- grouped_data %>%
  pivot_longer(cols = c(NDWI, NDVI, CCCI),  #CIG,
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
  theme_minimal() 








#------------------------ bands 


library(purrr)

decomposed_results <- long_data %>%
  #filter(cover == 4) %>%
  split(.$Band) %>%
  map(~ {
    x <- .x %>% arrange(year_month)
    ts_data <- ts(x$Mean_Value, start = c(year(x$year_month[1]), month(x$year_month[1])), frequency = 12)
    decompose(ts_data, type = "additive")
  })

decomposed_results_1 <- decomposed_results$B4

# Example: plot one of them
plot(decomposed_results_1$trend)




library(zoo)

my_colors <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
  "#e6ab02", "#a6761d", "#666666", "#a6cee3", "#1f78b4",
  "#b2df8a", "#33a02c", "#fb9a99"
)

# Create a named list for custom x-axis labels with subscripts
custom_labels <- c(
  "B1" = expression(B[1]),
  "B2" = expression(B[2]),
  "B3" = expression(B[3]),
  "B4" = expression(B[4]),
  "B5" = expression(B[5]),
  "B6" = expression(B[6]),
  "B7" = expression(B[7]),
  "B8" = expression(B[8]),
  "B8A" = expression(B[8*A]),
  "B9" = expression(B[9]),
  "B11" = expression(B[11]),
  "B12" = expression(B[12])
)

  
  
# Combine trend components into one dataframe
all_trends <- purrr::map_dfr(names(decomposed_results), function(band) {
  data.frame(
    Date = as.Date(as.yearmon(time(decomposed_results[[band]]$trend))),
    Band = band,
    Trend = as.numeric(decomposed_results[[band]]$trend)
  )
}) %>% 
  filter(!is.na(Trend))


# Define the desired order of bands exactly as in your data
desired_order <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B9", "B11", "B12")

# Set Band as factor with levels in that order
all_trends$Band <- factor(all_trends$Band, levels = desired_order)


# Plot with manual colors and custom legend labels
ggplot(all_trends, aes(x = Date, y = Trend, color = Band)) +
  geom_line() + 
  scale_color_manual(values = my_colors, labels = custom_labels) +
  labs(title = "Decomposed Trend by Band", x = "Date", y = "Decomposed Trend") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









#------------------------------------ INDICES


library(purrr)


indices_data <- indices_data %>%
  mutate(year_month = as.Date(paste0(year_month, "-01")))


decomposed_results <- indices_data %>%
  #filter(cover == 4) %>%
  split(.$Index) %>%
  map(~ {
    x <- .x %>% arrange(year_month)
    ts_data <- ts(x$Value, start = c(year(x$year_month[1]), month(x$year_month[1])), frequency = 12)
    decompose(ts_data, type = "additive")
  })

decomposed_results_1 <- decomposed_results$CCCI

# Example: plot one of them
plot(decomposed_results_1$trend)

plot.ts(decomposed_results_1$trend,
        xlab = "Date",
        ylab = "Trend Value",
        main = "Trend Component of CCCI")



library(zoo)

my_colors <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
  "#e6ab02", "#a6761d", "#666666", "#a6cee3", "#1f78b4",
  "#b2df8a", "#33a02c", "#fb9a99"
)

# Create a named list for custom x-axis labels with subscripts
custom_labels <- c(
  "CCCI" = expression(CCCI),
  "NDVI" = expression(NDVI),
  "NDWI" = expression(NDWI)
)



# Combine trend components into one dataframe
all_trends <- purrr::map_dfr(names(decomposed_results), function(band) {
  data.frame(
    Date = as.Date(as.yearmon(time(decomposed_results[[band]]$trend))),
    Band = band,
    Trend = as.numeric(decomposed_results[[band]]$trend)
  )
}) %>% 
  filter(!is.na(Trend))


# Define the desired order of bands exactly as in your data
desired_order <- c("CCCI", "NDVI", "NDWI")

# Set Band as factor with levels in that order
all_trends$Band <- factor(all_trends$Band, levels = desired_order)


# Plot with manual colors and custom legend labels
ggplot(all_trends, aes(x = Date, y = Trend, color = Band)) +
  geom_line() + 
  scale_color_manual(values = my_colors, labels = custom_labels) +
  labs(title = "Decomposed Trend by Indices", x = "Date", y = "Decomposed Trend") +
  theme_minimal() +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

