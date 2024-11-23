library(tidyverse)
library(data.table)


damage_points_cropped_ML <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_ML.csv")





str(damage_points_cropped_ML)



scaled_data_ML_2 <- damage_points_cropped_ML %>%
  select(-geom, -...1) %>%
  pivot_longer(cols = starts_with("LANDSAT_"),  # Pivot all columns that start with 'L_'
               names_to = "LANDSAT",        # New column for the column names (L_1, L_2, etc.)
               values_to = "values")       # New column for the values



# Extract the group name
scaled_data_ML_2 <- scaled_data_ML_2 %>%
  mutate(group = paste0("LANDSAT_", sub(".*_", "", LANDSAT))) # Extract group name (e.g., LANDSAT_1)



unique(scaled_data_ML_2$group)

# Define a custom order for the group column
order <- c( "LANDSAT_1", "LANDSAT_2", "LANDSAT_3", "LANDSAT_4", "LANDSAT_5", "LANDSAT_6", "LANDSAT_7", "LANDSAT_8", "LANDSAT_9")

# Convert 'group' column to a factor with the custom order
scaled_data_ML_2$group <- factor(scaled_data_ML_2$group, levels = order, ordered = TRUE)

# Sort the dataframe by 'group' column
df_sorted <- scaled_data_ML_2[order(scaled_data_ML_2$group), ]

# Print the sorted dataframe
print(df_sorted)



# Assign a unique row index for each group
scaled_data_ML_2 <- df_sorted %>%
  group_by(group) %>%
  mutate(row = row_number()) %>% # Row index for stacking within groups
  ungroup()

# Reshape to wide format
wide_data <- scaled_data_ML_2 %>%
  pivot_wider(names_from = group, values_from = values)

# View the reshaped data
#print(wide_data, n = 50, width = Inf)



# Assuming `wide_data` is the reshaped data
wide_data_filled <- wide_data %>%
  fill(everything(), .direction = "downup")  # Fills NA values by propagating the closest non-NA value

# View the filled data
print(wide_data_filled, n = 50, width = Inf)







##############################################################################################################
#damage_points_cropped_ML <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_ML.csv")

#str(damage_points_cropped_ML)
#colnames(damage_points_cropped_ML)


# Assuming 'damage_points_cropped_ML' is your data frame
damage_points_long <- wide_data_filled %>%
 #select( -...1) |>
  #select( -geom) |>
  pivot_longer(cols = starts_with("status"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year",             # New column for the year
               values_to = "status")          # New column for the values of LANDSAT bands


# Scale only the numeric columns except for 'status'
scaled_data <- damage_points_long %>%
  select(-status) %>%          # Remove the 'status' column
  select(-year) %>%  
  select(where(is.numeric)) %>% # Select only numeric columns
  scale() %>%                  # Scale the numeric columns
  as.data.frame() %>%          # Convert the scaled data back to a data frame
  bind_cols(damage_points_long %>% select(status)) %>%  # Add the 'status' column back
  bind_cols(damage_points_long %>% select(year))  # Add the 'status' column back

scaled_data <- na.omit(scaled_data)
# View the scaled data
head(scaled_data)


scaled_data_ML <- scaled_data |>
  select(-year)

# Assuming 'damage_points_cropped_ML' is your data frame
scaled_data_ML_2 <- scaled_data_ML %>%
  #select( -...1) |>
  #select( -geom) |>
  pivot_longer(cols = starts_with("prec"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year_prec",             # New column for the year
               values_to = "prec")          # New column for the values of LANDSAT bands



scaled_data_ML_2 <- scaled_data_ML_2 %>%
  #select( -...1) |>
  #select( -geom) |>
  pivot_longer(cols = starts_with("tmax"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year_tmax",             # New column for the year
               values_to = "tmax")          # New column for the values of LANDSAT bands




#scaled_data_ML_2 <- scaled_data_ML_2 %>%
  #select( -...1) |>
  #select( -geom) |>
  #pivot_longer(cols = starts_with("tmin"),  # Specify the columns to pivot (all LANDSAT columns)
           #    names_to = "year_tmin",             # New column for the year
    #           values_to = "tmin")          # New column for the values of LANDSAT bands


str(scaled_data_ML_2)


scaled_data_ML_2 <- scaled_data_ML_2 |>
  select(-year_prec) |>
    select(-year_tmax) |>
  #select(-year_tmin) |>
  select(-SURVEY_YEAR) |>
  select(-row)


str(scaled_data_ML_2)



# Reorder columns so that "status" is the last column
setcolorder(scaled_data_ML_2, c(setdiff(names(scaled_data_ML_2), "status"), "status"))

# Check the structure to ensure "status" is now the last column
str(scaled_data_ML_2)

###############################################################################################################






#write.csv(scaled_data_ML_2, "/scratch/ope4/MERGE/EXPORT_SHP/scaled_data_ML_2.csv")


# Fast export to CSV
fwrite(scaled_data_ML_2, "/scratch/ope4/MERGE/EXPORT_SHP/scaled_data_ML_2.csv", row.names = FALSE)




