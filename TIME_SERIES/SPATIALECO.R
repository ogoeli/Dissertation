# Load necessary packages
library(spdep)   # For spatial neighbors and models



# Randomly select 50% of rows
data <- data %>%
  slice_sample(prop = 0.01) %>% 
  filter(cover == 1 | cover == 3)

# Convert to spatial object
grid_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Transform to projected CRS (for accurate distance-based or k-NN)
grid_sf_proj <- st_transform(grid_sf, 3857)  # Web Mercator in meters

# Create neighbors (rook's case: up/down/left/right)
coords <- st_coordinates(grid_sf_proj)
nb <- dnearneigh(coords, d1 = 0, d2 = 1000)  # Use distance to define neighbors
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Visualize neighborhood
plot(grid_sf_proj$geometry, main = "Tree Locations")
#plot(nb, coords, add = TRUE, col = 'blue')

# Spatial autocorrelation test (Moran's I)
moran.test(data$cover, lw)

# Autologistic model (requires custom setup or Bayesian framework)
# Here's a pseudo-autologistic approach with spatial lag as a predictor
data$dead_tree <- ifelse(data$cover == 1, 1, 0)
lag_status <- lag.listw(lw, data$dead_tree)
model <- glm(dead_tree ~ lag_status, data = data, family = binomial())

summary(model)



##auto logistic

install.packages("spatialEco")
install.packages("spdep")  # you likely already have this
library(spatialEco)
library(spdep)



auto_model <- autologistic(Y = data$dead_tree, X = NULL, coords = coordinates(data), 
                           neigh = nb, family = "binomial")


# Try different distances (in meters if your coordinates are in meters)
# For example: 5m, 10m, 15m...
thresholds <- c(5, 10, 15, 20, 30)  # You can pick meaningful ones
thresholds <- seq(5, 30, by = 1)
results <- data.frame(distance = thresholds, coef = NA, pval = NA)


for (i in seq_along(thresholds)) {
  d <- thresholds[i]
  nb <- dnearneigh(coords, d1 = 0.01, d2 = d)
  
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  lag_status <- lag.listw(lw, data$dead_tree, zero.policy = TRUE)
  
  model <- glm(data$dead_tree ~ lag_status, family = binomial())
  results$coef[i] <- coef(model)[2]
  results$pval[i] <- summary(model)$coefficients[2, 4]
}

print(results)


plot(results$distance, results$coef, type = "b", pch = 19,
     xlab = "Distance threshold (m)", ylab = "Lag Coefficient",
     main = "Effect of Neighbor Distance on Bark Beetle Spread")




grid_sf_proj$lag_dead <- lag_status
ggplot(grid_sf_proj) +
  geom_sf(aes(color = lag_dead), size = 1) +
  scale_color_viridis_c(option = "plasma", name = "Lag of Dead Trees") +
  theme_minimal() +
  ggtitle("Spatial Lag of Bark Beetle Mortality")




# Install if not already installed
# install.packages("inflection")
library(inflection)

# Remove rows with NA
valid_results <- na.omit(results)

# Find the index of the maximum coefficient
max_coef_index <- which.max(valid_results$coef)

# Get the distance at which the maximum coefficient occurs
max_distance <- valid_results$distance[max_coef_index]

cat("Maximum spatial lag coefficient occurs at distance:", max_distance, "meters\n")


ggplot(valid_results, aes(x = distance, y = coef)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  geom_vline(xintercept = valid_results$distance[max_coef_index], linetype = "dashed", color = "red") +
  annotate("text", x = valid_results$distance[max_coef_index] + 1, y = max(valid_results$coef, na.rm = TRUE),
           label = paste("Elbow at", valid_results$distance[max_coef_index], "m"), hjust = 0, color = "red") +
  labs(title = "Local Infection Effect vs. Distance",
       x = "Distance threshold (m)",
       y = "Coefficient of spatial lag (β)") +
  theme_minimal() +
  scale_x_continuous(breaks = 5:30)





##K nearest neighbors
k <- 4
nb_knn <- knn2nb(knearneigh(coords, k = k))
lw_knn <- nb2listw(nb_knn, style = "W")
lw_knn <- nb2listw(nb_knn, style = "W")
lw_knn <- nb2listw(nb_knn, style = "W")
lag_knn <- lag.listw(lw_knn, data$dead_tree, zero.policy = TRUE)
grid_sf_proj$lag_knn <- lag_knn

ggplot(grid_sf_proj) +
  geom_sf(aes(color = lag_knn), size = 1) +
  scale_color_viridis_c(option = "plasma", name = paste0("kNN Lag (k=", k, ")")) +
  theme_minimal() +
  ggtitle("k-Nearest Neighbor Lag of Dead Trees")





# Try different distances (in meters if your coordinates are in meters)
# For example: 5m, 10m, 15m...
thresholds <- c(5, 10, 15, 20, 30)  # You can pick meaningful ones
thresholds <- seq(5, 30, by = 1)
results <- data.frame(distance = thresholds, coef = NA, pval = NA)


for (i in seq_along(thresholds)) {
  d <- thresholds[i]
  nb <- dnearneigh(coords, d1 = 0.01, d2 = d)
  
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  lag_status <- lag.listw(lw, data$dead_tree, zero.policy = TRUE)
  
  model <- glm(data$dead_tree ~ lag_status, family = binomial())
  results$coef[i] <- coef(model)[2]
  results$pval[i] <- summary(model)$coefficients[2, 4]
}

print(results)


plot(results$distance, results$coef, type = "b", pch = 19,
     xlab = "Distance threshold (m)", ylab = "Lag Coefficient",
     main = "Effect of Neighbor Distance on Bark Beetle Spread")




grid_sf_proj$lag_dead <- lag_status
ggplot(grid_sf_proj) +
  geom_sf(aes(color = lag_dead), size = 1) +
  scale_color_viridis_c(option = "plasma", name = "Lag of Dead Trees") +
  theme_minimal() +
  ggtitle("Spatial Lag of Bark Beetle Mortality")




# Install if not already installed
# install.packages("inflection")
library(inflection)

# Remove rows with NA
valid_results <- na.omit(results)

# Find the index of the maximum coefficient
max_coef_index <- which.max(valid_results$coef)

# Get the distance at which the maximum coefficient occurs
max_distance <- valid_results$distance[max_coef_index]

cat("Maximum spatial lag coefficient occurs at distance:", max_distance, "meters\n")


ggplot(valid_results, aes(x = distance, y = coef)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  geom_vline(xintercept = valid_results$distance[max_coef_index], linetype = "dashed", color = "red") +
  annotate("text", x = valid_results$distance[max_coef_index] + 1, y = max(valid_results$coef, na.rm = TRUE),
           label = paste("Elbow at", valid_results$distance[max_coef_index], "m"), hjust = 0, color = "red") +
  labs(title = "Local Infection Effect vs. Distance",
       x = "Distance threshold (m)",
       y = "Coefficient of spatial lag (β)") +
  theme_minimal() +
  scale_x_continuous(breaks = 5:30)

