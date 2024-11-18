library(tidyr)
library(dplyr)
library(data.table)


damage_points_cropped_ML <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/damage_points_cropped_ML.csv")


#str(damage_points_cropped_ML)



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




scaled_data_ML_2 <- scaled_data_ML_2 %>%
  #select( -...1) |>
  #select( -geom) |>
  pivot_longer(cols = starts_with("tmin"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year_tmin",             # New column for the year
               values_to = "tmin")          # New column for the values of LANDSAT bands


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




##############################################################################################################

#data <- fread("/scratch/ope4/MERGE/EXPORT_SHP/scaled_data_ML_2.csv")

scaled_data_ML_2 <- scaled_data_ML %>%
  #select(-geom, -...1) %>%
  pivot_longer(cols = starts_with("LANDSAT_"),  # Pivot all columns that start with 'L_'
               names_to = "LANDSAT",        # New column for the column names (L_1, L_2, etc.)
               values_to = "values")       # New column for the values

# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
#names(data_1) <- gsub("LANDSAT_\\d{2}_(\\d+)", "L_\\1", names(data_1))
# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
#names(data_1) <- gsub("L_\\d{2}_(\\d+)", "L_\\1", names(data_1))

str(scaled_data_ML_2)


wide_data <- scaled_data_ML_2 %>%
  mutate(LANDSAT_num = sub(".*_(\\d+)$", "landsat_\\1", LANDSAT)) %>%
  
  # Pivot the data into long format first (wide to long transformation)
  pivot_longer(cols = starts_with("landsat_"), 
               names_to = "LANDSAT_num", 
               values_to = "landsat_values") %>%
  
  # Optionally, if needed, group and arrange to control the order
  arrange(LANDSAT_num, .by_group = TRUE) |>
  select(-LANDSAT_num, -LANDSAT)

print(wide_data[, c(25, 23)], n = 50)



# Step 1: Create a unique 'landsat_value' by adding a suffix
wide_data <- wide_data %>%
  group_by(landsat_values) %>%
  mutate(landsat_values_unique = paste(landsat_values, row_number(), sep = "_")) %>%
  ungroup()  # Remove the grouping

# Step 2: Pivot the data wider using the unique 'landsat_values_unique'
wide_data_wide <- wide_data %>%
  pivot_wider(names_from = landsat_values_unique, values_from = values)

# Print the first 50 rows of the newly transformed data
print(wide_data_wide, n = 50)














# Assuming 'damage_points_cropped_ML' is your data frame
scaled_data_ML_2 <- scaled_data_ML_2 %>%
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




scaled_data_ML_2 <- scaled_data_ML_2 %>%
  #select( -...1) |>
  #select( -geom) |>
  pivot_longer(cols = starts_with("tmin"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year_tmin",             # New column for the year
               values_to = "tmin")          # New column for the values of LANDSAT bands






str(scaled_data_ML_2)



scaled_data_ML_2 <- scaled_data_ML_2 |>
  select(-year_prec) |>
  select(-year_tmax) |>
  select(-year_tmin) 


str(scaled_data_ML_2)


unique(data_1$LANDSAT)



# Convert the dataset into long format
data_long <- data_1 %>%
  pivot_longer(
    cols = -c(LANDSAT, values),  # Keep 'LANDSAT' and 'values' as they are
    names_to = c(".value", "year"), # Separate the variable and year
    names_pattern = "(.*)_(\\d{4})" # Capture variable (e.g., 'prec', 'tmax', etc.) and year (2015-2021)
  )

# View the result
str(data_long)



# Transform data to wide format
wide_data <- scaled_data_ML_2 %>%
  mutate(LANDSAT = sub(".*_(\\d)$", "landsat_\\1", LANDSAT)) %>%
  pivot_wider(names_from = LANDSAT, values_from = values)

# Print result
print(wide_data)























# Assuming 'damage_points_cropped_ML' is your data frame
damage_points_long <- damage_points_cropped_ML %>%
  select( -...1) |>
  select( -geom) |>
  pivot_longer(cols = starts_with("status"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "year",             # New column for the year
               values_to = "status")          # New column for the values of LANDSAT bands








str(data_1)

data_1 <- data_1 %>%
  select(LANDSAT_21_1, LANDSAT_20_1, LANDSAT_19_1, LANDSAT_18_1,LANDSAT_17_1, LANDSAT_16_1, L_15_1, status, prec, tmax, tmin) %>%
  pivot_longer(cols = starts_with("L_"),  # Pivot all columns that start with 'L_'
               names_to = "LANDSAT_1",        # New column for the column names (L_1, L_2, etc.)
               values_to = "values")       # New column for the values

str(data_1)


#############################################################################################################

# Fast export to CSV
fwrite(data_1, "/scratch/ope4/MERGE/EXPORT_SHP/data_1.csv", row.names = FALSE)







############################################################################################################

data <- fread("/scratch/ope4/MERGE/EXPORT_SHP/data_1.csv")

# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("LANDSAT_\\d{2}_(\\d+)", "LANDSAT_\\1", names(data))
# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("L_\\d{2}_(\\d+)", "L_\\1", names(data))




data_2 <- data %>%
  pivot_longer(cols = starts_with("L_1"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "L_1",             # New column for the year
               values_to = "values")          # New column for the values of LANDSAT bands


str(data_2)

##############################################################################################################


# Fast export to CSV
fwrite(data_2, "/scratch/ope4/MERGE/EXPORT_SHP/data_2.csv", row.names = FALSE)





###############################################################################################################

data <- fread("/scratch/ope4/MERGE/EXPORT_SHP/data_2.csv")

# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("LANDSAT_\\d{2}_(\\d+)", "LANDSAT_\\1", names(data))
# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("L_\\d{2}_(\\d+)", "L_\\1", names(data))




data_3 <- data %>%
  pivot_longer(cols = starts_with("L_1"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "L_1",             # New column for the year
               values_to = "values")          # New column for the values of LANDSAT bands


str(data_3)

################################################################################################################



# Fast export to CSV
fwrite(data_3, "/scratch/ope4/MERGE/EXPORT_SHP/data_3.csv", row.names = FALSE)





###############################################################################################################

data <- fread("/scratch/ope4/MERGE/EXPORT_SHP/data_2.csv")

# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("LANDSAT_\\d{2}_(\\d+)", "LANDSAT_\\1", names(data))
# Rename columns to remove the year suffix (e.g., LANDSAT_20_1 to LANDSAT_1)
names(data) <- gsub("L_\\d{2}_(\\d+)", "L_\\1", names(data))




data_4 <- data %>%
  pivot_longer(cols = starts_with("L_1"),  # Specify the columns to pivot (all LANDSAT columns)
               names_to = "L_1",             # New column for the year
               values_to = "values")          # New column for the values of LANDSAT bands


str(data_4)

################################################################################################################














################################################################################################################




















































# Define a function to pivot a subset of columns and convert them to long format
pivot_band_subset <- function(data, col_pattern) {
  data %>%
    select(matches(col_pattern), contains("_1")) %>%
    pivot_longer(
      cols = matches(col_pattern),
      names_to = "Year",
      names_pattern = "LANDSAT_(\\d{2})_1",
      values_to = "Band_1_Value"
    ) %>%
    mutate(Year = as.numeric(Year))
}

# Process chunks by year groups (e.g., LANDSAT_21 to LANDSAT_19)
chunk_1 <- pivot_band_subset(scaled_data_ML_2, "LANDSAT_21|LANDSAT_20|LANDSAT_19")
chunk_2 <- pivot_band_subset(scaled_data_ML_2, "LANDSAT_18|LANDSAT_17|LANDSAT_16|L_15")

# Combine the chunks
band_1_data <- bind_rows(chunk_1, chunk_2) %>%
  arrange(Year)

stacked_data <- data.frame()

for (year in 15:21) {
  
  # Print the year being processed
  print(paste("Processing year:", year))
  
  # Generate column names for L_ and LANDSAT_ formats
  columns <- c(paste0("L_", year %% 100, "_", 1:9), paste0("LANDSAT_", year %% 100, "_", 1:9))
  
  # Select columns that exist in the data and add to stacked_data
  available_columns <- columns[columns %in% names(scaled_data_ML_2)]
  if (length(available_columns) > 0) {
    stacked_data <- rbind(stacked_data, scaled_data_ML_2[, available_columns])
  }
}



# Remove all columns that start with "L"
columns_to_exclude <- grep("^L", names(scaled_data_ML_2), value = TRUE)

# Subset the data by excluding those columns
scaled_data_no_L <- scaled_data_ML_2[, !names(scaled_data_ML_2) %in% columns_to_exclude]



# Check the structure of both datasets
str(stacked_data)
str(scaled_data_no_L)

# If the common key is the row index, you can do a full join based on the row number
scaled_data_ML_new <- cbind(stacked_data, scaled_data_no_L)

str(scaled_data_ML_new)





unique(scaled_data_ML_new$status)





library(dplyr)

stacked_data <- data.frame()

# Loop through the years
for (year in 15:21) {
  
  # Print the year being processed
  print(paste("Processing year:", year))
  
  # Generate column names for L_ and LANDSAT_ formats
  columns <- c(paste0("L_", year %% 100, "_", 1:9), paste0("LANDSAT_", year %% 100, "_", 1:9))
  
  # Select columns that exist in the data
  available_columns <- columns[columns %in% names(scaled_data_ML_2)]
  
  if (length(available_columns) > 0) {
    # Subset the data to the available columns
    subset_data <- scaled_data_ML_2[, available_columns]
    
    # Bind rows using dplyr::bind_rows for flexibility with column alignment
    stacked_data <- bind_rows(stacked_data, subset_data)
  }
}

# Check the structure of the final stacked_data
str(stacked_data)











write.csv(stacked_data, "/scratch/ope4/MERGE/EXPORT_SHP/stacked_data.csv")



data <- read_csv("/scratch/ope4/MERGE/EXPORT_SHP/stacked_data.csv")