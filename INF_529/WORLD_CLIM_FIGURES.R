library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sp)

data <- read.csv("/scratch/ope4/MERGE/DEC-04-2024/combined_dataset_BUFFER.csv") 



# Group by 'SURVEY_YEAR' and calculate the mean for each variable (precipitation, tmax, tmin)
climate_data_mean <- data %>%
  group_by(SURVEY_YEAR) %>%
  summarize(
    mean_prec = mean(prec, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_tmin = mean(tmin, na.rm = TRUE),
    .groups = 'drop'
  ) |>
pivot_longer(cols = starts_with("mean"),  
                 names_to = "mean_clim",             
                 values_to = "variable") 

# View the calculated means
head(climate_data_mean)

# Plot the mean values over the years for each climate variable
p1 <- ggplot(climate_data_mean, aes(x = as.factor(SURVEY_YEAR), y = variable, color = mean_clim, group = mean_clim)) +
  geom_line(size = 1) +  # Line color
  geom_point(size = 3) +  # Point markers
  labs(title = "",
       x = "Year", y = "Mean Value",
       color = "Variable") +
  theme_classic() +
  theme(legend.position = "bottom")

p1

# Save the plot
ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/WORLD_CLIM.png", p1)



#######################

data <- read_csv("/scratch/ope4/MERGE/BUFFER_EXPORT/CLASS_CHANGE.csv")



# Load necessary libraries
library(ggplot2)
library(viridis)

# Filter data for NDWI and NDVI
ndwi_data <- data %>% filter(INDEX == "NDWI")
ndvi_data <- data %>% filter(INDEX == "NDVI")
ndti_data <- data %>% filter(INDEX == "NDTI")

# Plot for NDWI
p2 <- ggplot(ndwi_data, aes(x = factor(YEAR), y = PERCENT, fill = CLASS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "NDWI by Year and Class", 
       x = "Year", 
       y = "Value", 
       fill = "Class") +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE)  +
  ylim(0,100)

p2

# Save the plot
ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/p_ndwi2.png", p2)



# Plot for NDVI
p3 <- ggplot(ndvi_data, aes(x = factor(YEAR), y = PERCENT, fill = CLASS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "NDVI by Year and Class", 
       x = "Year", 
       y = "Value", 
       fill = "Class") +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE)  +
  ylim(0,100)

p3

# Save the plot
ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/p_ndvi3.png", p3)




# Plot for NDTI
p4 <- ggplot(ndti_data, aes(x = factor(YEAR), y = PERCENT, fill = CLASS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "NDTI by Year and Class", 
       x = "Year", 
       y = "Value", 
       fill = "Class") +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE)  +
  ylim(0,100)

p4

# Save the plot
ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/p_ndti3.png", p4)





####################################################
dataset <- read_csv("/scratch/ope4/MERGE/BUFFER_EXPORT/data_NDVI_NDWI_YEAR.csv")



p5 <- ggplot(dataset, aes(x = as.factor(YEAR), y = NDVI, group = 1)) + ##the group aes is needed to draW the line
  geom_line(color = "black", size = 1) +  # Line color
  geom_point(size = 3, color = "darkgreen") +  # Point markers
  labs(title = "NDVI for infected trees across years", x = "Year", y = "Mean NDVI") +
  theme_minimal() +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "black",  size = 17),
        axis.line = element_line(size = 0.1),
        axis.text = element_text(color = "black", size = 17),  
        axis.title = element_text(size = 17), 
        aspect.ratio = 1,
        panel.border = element_rect(size = 1.5, fill = NA)
  ) 
p5

ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/trend_ndvi.png", p5)





p6 <- ggplot(dataset, aes(x = as.factor(YEAR), y = NDWI, group = 1)) + ##the group aes is needed to draW the line
  geom_line(color = "black", size = 1) +  # Line color
  geom_point(size = 3, color = "darkgreen") +  # Point markers
  labs(title = "NDWI for infected trees across years", x = "Year", y = "Mean NDWI") +
  theme_minimal() +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "black",  size = 17),
        axis.line = element_line(size = 0.1),
        axis.text = element_text(color = "black", size = 17),  
        axis.title = element_text(size = 17), 
        aspect.ratio = 1,
        panel.border = element_rect(size = 1.5, fill = NA)
  ) 
p6


ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/trend_ndWi.png", p6)




p7 <- ggplot(dataset, aes(x = as.factor(YEAR), y = NDTI, group = 1)) + ##the group aes is needed to draW the line
  geom_line(color = "black", size = 1) +  # Line color
  geom_point(size = 3, color = "darkgreen") +  # Point markers
  labs(title = "NDTI for infected trees across years", x = "Year", y = "Mean NDTI") +
  theme_minimal() +
  theme_classic() +
  theme(text = element_text(family = "Arial", color = "black",  size = 17),
        axis.line = element_line(size = 0.1),
        axis.text = element_text(color = "black", size = 17),  
        axis.title = element_text(size = 17), 
        aspect.ratio = 1,
        panel.border = element_rect(size = 1.5, fill = NA)
  ) 
p7


ggsave("/scratch/ope4/MERGE/BUFFER_EXPORT/trend_ndTi.png", p7)












