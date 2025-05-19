library(tidyverse)
library(sf)
library(RColorBrewer)


data <- read.csv("/scratch/ope4/MERGE/BUFFER_EXPORT/combined_dataset_BUFFER.csv")


# Step 1: Convert the geom column from character to list of numeric coordinates
data$coords <- gsub("c\\(|\\)", "", data$geom)  # Remove 'c(' and ')'
data$coords <- strsplit(data$coords, ", ")  # Split by comma and space
data$coords <- lapply(data$coords, as.numeric)  # Convert to numeric pairs

# Step 2: Create a new data frame with longitude and latitude extracted from coords
data$LONG <- sapply(data$coords, function(x) x[1])
data$LAT <- sapply(data$coords, function(x) x[2])

# Step 3: Convert to sf object using LONG and LAT
data <- st_as_sf(data, coords = c("LONG", "LAT"), crs = 4326) |>
  select(-geom, -coords)




#####################################################################################################################################

str(data)


# Create a map with temperature (e.g., tmax) as color
p1 <- ggplot(data) +
  geom_sf(aes(color = tmax), size = 0.5) + # Map with tmax as the color
  scale_color_viridis_c(option = "plasma") + # Choose a color scale (viridis palette)
  #theme_minimal() +
  labs(title = "Temperature Map (tmax)", color = "Max Temp (°C)") +
  theme(legend.position = "right") +
  facet_wrap(~SURVEY_YEAR) + 
  theme_bw() +
  coord_sf() +  
  theme( strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect( size = 1)
  ) 

p1 







p2 <- ggplot(data) +
  geom_sf(aes(color = tmin), size = 0.5) + # Map with tmax as the color
  scale_color_viridis_c(option = "plasma") + # Choose a color scale (viridis palette)
  #theme_minimal() +
  labs(title = "Temperature Map (tmin)", color = "Min Temp (°C)") +
  theme(legend.position = "right") +
  facet_wrap(~SURVEY_YEAR) + 
  theme_bw() +
  coord_sf() +  
  theme( strip.background = element_blank(),
         panel.spacing = unit(0.5, "lines"),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.border = element_rect( size = 1)
  ) 

p2 




p3 <- ggplot(data) +
  geom_sf(aes(color = prec), size = 0.5) + # Map with tmax as the color
  scale_color_viridis_c(option = "plasma") + # Choose a color scale (viridis palette)
  #theme_minimal() +
  #labs(title = "Temperature Map (tmin)", color = "Min Temp (°C)") +
  theme(legend.position = "right") +
  facet_wrap(~SURVEY_YEAR) + 
  theme_bw() +
  coord_sf() +  
  theme( strip.background = element_blank(),
         panel.spacing = unit(0.5, "lines"),
         panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),
         panel.border = element_rect( size = 1)
  ) 

p3 










################################################################################################################################


# Aggregate the data to count the number of status 0 and 1 for each year
status_count <- data %>%
  group_by(SURVEY_YEAR, status) %>%
  summarise(count = n(), .groups = "drop") |>
  filter(status == 1)

# View the summary
head(status_count)





# Bar plot of the status counts by year
ggplot(status_count, aes(x = factor(SURVEY_YEAR), y = count)) +
  geom_bar(position="dodge", stat="identity", width = 0.5, color="black",size=1)+  
  theme_minimal() +
  labs(title = "", 
       x = "", y = "") +
  scale_fill_viridis_d(option = "D") +  
  theme(axis.text = element_text(size = 12, color = 'black'),  
        axis.title = element_text(size = 12),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(size = 0.1), 
        panel.background = element_rect(fill = 'transparent')) +  
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 




















##############################################################################################################################

climate_summary <- data %>%
  filter(status == 1) %>%
  group_by(SURVEY_YEAR) %>%
  summarise(
    avg_prec = mean(prec, na.rm = TRUE),
    avg_tmax = mean(tmax, na.rm = TRUE),
    avg_tmin = mean(tmin, na.rm = TRUE),
    .groups = "drop"
  )




# Precipitation over the years
ggplot(climate_summary, aes(x = SURVEY_YEAR, y = avg_prec)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  labs(
    title = "Average Precipitation (prec) Over the Years",
    x = "Survey Year",
    y = "Average Precipitation (mm)"
  ) +
  theme(
    axis.text = element_text(size = 12, color = 'black'),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  )





# Maximum temperature over the years
ggplot(climate_summary, aes(x = SURVEY_YEAR, y = avg_tmax)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(
    title = "Average Maximum Temperature (tmax) Over the Years",
    x = "Survey Year",
    y = "Average Maximum Temperature (°C)"
  ) +
  theme(
    axis.text = element_text(size = 12, color = 'black'),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  )

# Minimum temperature over the years
ggplot(climate_summary, aes(x = SURVEY_YEAR, y = avg_tmin)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 2) +
  theme_minimal() +
  labs(
    title = "Average Minimum Temperature (tmin) Over the Years",
    x = "Survey Year",
    y = "Average Minimum Temperature (°C)"
  ) +
  theme(
    axis.text = element_text(size = 12, color = 'black'),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  )



library(tidyr)

# Reshape data for combined plot
climate_long <- climate_summary %>%
  pivot_longer(
    cols = c(avg_prec, avg_tmax, avg_tmin),
    names_to = "climate_factor",
    values_to = "value"
  )

# Combined plot
ggplot(climate_long, aes(x = SURVEY_YEAR, y = value, color = climate_factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Climate Factors Over the Years",
    x = "Survey Year",
    y = "Value",
    color = "Climate Factor"
  ) +
  scale_color_manual(
    values = c("avg_prec" = "blue", "avg_tmax" = "red", "avg_tmin" = "green"),
    labels = c("Precipitation", "Max Temp", "Min Temp")
  ) +
  theme(
    axis.text = element_text(size = 12, color = 'black'),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "top"
  )












beetle_attacks <- status_count %>%
  group_by(SURVEY_YEAR) %>%
  summarise(total_attacks = sum(count), .groups = "drop")



library(ggplot2)

ggplot(beetle_attacks, aes(x = factor(SURVEY_YEAR), y = total_attacks)) +
  geom_bar(stat = "identity", fill = "darkred", color = "black", width = 0.6) +
  theme_minimal() +
  labs(
    title = "Beetle Attacks Over the Years",
    x = "Survey Year",
    y = "Total Beetle Attacks"
  ) +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
