# Assuming your data is stored in results_df_unique
library(ggplot2)
library(dplyr)

# Filter data to focus on SWIR 1, assuming band "B12" is the best detection for SWIR 1
best_detection <- results_df_unique %>%
  filter(band == "B8") %>%
  filter(R2 > 0.8) # Threshold for accuracy, you can adjust this

# Extract coordinates and split into longitude and latitude
coords <- t(sapply(best_detection$geometry, function(x) {
  coords <- fromJSON(x)$coordinates
  return(coords)
}))

# Assign longitude and latitude to separate columns
best_detection$longitude <- coords[, 1]
best_detection$latitude <- coords[, 2]

# View the new columns
head(best_detection[, c("longitude", "latitude")])

# Now plot the change locations as points
ggplot(best_detection, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = as.factor(cp)), size = 1, alpha = 0.7) +
  labs(title = "Forest Change Map - SWIR 1", x = "Longitude", y = "Latitude", color = "Change Year") +
  theme_minimal()


# If you want to visualize the area of changes based on hectares (e.g., based on the cpPr variable)
# You could aggregate data by cpPr or other relevant variables, and then visualize areas
area_changes <- best_detection %>%
  group_by(cp) %>%
  summarise(total_area_ha = sum(cpPr, na.rm = TRUE)) # Replace with your own area calculation

# Plot total area of changes per year (cp)
ggplot(area_changes, aes(x = cp, y = total_area_ha)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Annual Forest Change Areas", x = "Year", y = "Area (hectares)") +
  theme_minimal()
