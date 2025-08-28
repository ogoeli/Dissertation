colnames(df)[colnames(df) == '.geo'] <- 'geom'

df <- df[!duplicated(df[c("geom")]), ]



inner_join_BEAST <- inner_join(
  final_results_BEAST,
  df,
  by = "geom")


inner_join_BFAST <- inner_join(
  final_results_BFAST,
  df,
  by = "geom")


inner_join_DBEST <- inner_join(
  final_results_DBEST,
  df,
  by = "geom")




#dataset <- rbind(inner_join_CCCI, inner_join_NDWI,  inner_join_NDVI, inner_join_EWDI)

USDA <- read.csv('/scratch/ope4/Downloads/DATA/CSV/THREE/damage_points_sf.csv')

USDA <- USDA %>%
  mutate(name = as.character(name))


dataset_BEAST <- inner_join_BEAST %>% 
  inner_join(USDA, by = join_by(name)) |>
  dplyr::select(band, RMSE, R2, first_cp, second_cp, third_cp, period_1, period_2)


dataset_BFAST <- inner_join_BFAST %>% 
  inner_join(USDA, by = join_by(name)) |>
  dplyr::select(band, RMSE, R2, date1, date2, date3, period_1, period_2)

dataset_DBEST <- inner_join_DBEST %>% 
  inner_join(USDA, by = join_by(name)) |>
  dplyr::select(band, RMSE, R2, cp_date_1, cp_date_2, cp_date_3, period_1, period_2)



colnames(dataset_BFAST)

dataset <- inner_join_BEAST |>
  filter(band == 'NDWI') |>
  na.omit()

# Extract coordinates and split into longitude and latitude
coords <- t(sapply(dataset$geom, function(x) {
  coords <- fromJSON(x)$coordinates
  return(coords)
}))

# Assign longitude and latitude to separate columns
dataset$longitude <- coords[, 1]
dataset$latitude <- coords[, 2]


#for DBEST AND BFAST
dataset$year <- lubridate::year(dataset$date3)
dataset$quarter <- ceiling(as.numeric(format(dataset$date3, "%m")) / 3)

# for BEAST
# Create a new column for the year and quarter
dataset$year <- floor(dataset$third_cp)  # Get the year (floor value)
dataset$quarter <- ceiling((dataset$third_cp %% 1) * 4)  # Get the quarter (based on decimal)

# Create a new column that combines year and quarter (optional)
dataset$year_quarter <- paste(dataset$year, "Q", dataset$quarter, sep = "")



# Create the plot
ggplot(dataset, aes(x = longitude, y = latitude, color = factor(year_quarter))) +
  geom_point(size = 3) +
  scale_color_viridis_d() +  # Color scale for discrete values
 facet_grid(~cover)+
   theme_minimal() +
  labs(title = "Geospatial Data Colored by third CP",
       x = "Longitude", y = "Latitude", color = "Third CP")


dataset_BEAST <- dataset_BEAST |> na.omit()


dataset_BEAST <- dataset_BEAST %>%
  mutate(
    first_cp  = as.numeric(first_cp),
    second_cp = as.numeric(second_cp),
    third_cp  = as.numeric(third_cp)
  )


decimal_year_to_date <- function(decimal_year) {
  year <- floor(decimal_year)
  fraction <- decimal_year - year
  days_in_year <- 365
  days_to_add <- round(fraction * days_in_year)
  as.Date(paste0(year, "-01-01")) + days_to_add
}

dataset_BEAST <- dataset_BEAST %>%
  mutate(
    first_cp  = decimal_year_to_date(first_cp),
    second_cp = decimal_year_to_date(second_cp),
    third_cp  = decimal_year_to_date(third_cp)
  )



# Ensure dates are in Date format
dataset_BEAST <- dataset_BEAST %>%
  mutate(
    period_1   = as.Date(period_1, format = "%m/%d/%Y"),
    period_2   = as.Date(period_2, format = "%m/%d/%Y"),
    date1  = as.Date(first_cp),
    date2  = as.Date(second_cp),
    date3  = as.Date(third_cp)
  )


# Compute the midpoint of the actual period
dataset_BEAST <- dataset_BEAST %>%
  mutate(actual_mid = period_1 + as.integer((period_2 - period_1) / 2))

# Convert to Date if needed
dataset_BEAST <- dataset_BEAST %>%
  mutate(across(c(date1, date2, date3, period_1, period_2), as.Date))

# Calculate absolute differences for each predicted-actual pair
dataset_diff <- dataset_BEAST %>%
  rowwise() %>%
  mutate(
    diff_1_1 = abs(as.numeric(date1 - period_1)),
    diff_1_2 = abs(as.numeric(date1 - period_2)),
    diff_2_1 = abs(as.numeric(date2 - period_1)),
    diff_2_2 = abs(as.numeric(date2 - period_2)),
    diff_3_1 = abs(as.numeric(date3 - period_1)),
    diff_3_2 = abs(as.numeric(date3 - period_2))
  ) %>%
  ungroup()

# Find minimum difference for each predicted date to any actual date
dataset_diff <- dataset_diff %>%
  mutate(
    min_diff_cp1 = pmin(diff_1_1, diff_1_2, na.rm = TRUE),
    min_diff_cp2 = pmin(diff_2_1, diff_2_2, na.rm = TRUE),
    min_diff_cp3 = pmin(diff_3_1, diff_3_2, na.rm = TRUE)
  )



performance_dates_summary <- dataset_diff %>%
  group_by(band) %>%
  summarise(
    mean_diff_cp1 = mean(min_diff_cp1, na.rm = TRUE),
    mean_diff_cp2 = mean(min_diff_cp2, na.rm = TRUE),
    mean_diff_cp3 = mean(min_diff_cp3, na.rm = TRUE),
    
    median_diff_cp1 = median(min_diff_cp1, na.rm = TRUE),
    median_diff_cp2 = median(min_diff_cp2, na.rm = TRUE),
    median_diff_cp3 = median(min_diff_cp3, na.rm = TRUE),
    
    n = n()
  ) %>%
  arrange(mean_diff_cp1)








# Reshape for plotting
plot_data <- performance_dates_summary %>%
  pivot_longer(cols = starts_with("mean_diff"), 
               names_to = "predicted_date", 
               values_to = "mean_diff")

ggplot(plot_data, aes(x = reorder(band, mean_diff), y = mean_diff, fill = predicted_date)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(
    name = "Predicted Date Type",  # legend title
    labels = c(
      mean_diff_cp1 = "Prediction 1",
      mean_diff_cp2 = "Prediction 2",
      mean_diff_cp3 = "Prediction 3"
    ),
    values = c("mean_diff_cp1" = "#F8766D", "mean_diff_cp2" = "#7CAE00", "mean_diff_cp3" = "#00BFC4") # optional colors
  ) +
  labs(
    title = "Mean Absolute Difference (days) Between Predicted and Actual Dates by Band: BFAST",
    x = "Band",
    y = "Mean Absolute Difference (days)"
  ) +
  theme_minimal()





library(dplyr)

library(dplyr)

dataset_BFAST <- dataset_BFAST %>%
  filter(!is.na(date3))

dataset_BFAST %>%
  group_by(band) %>%
  summarise(
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    mean_R2 = mean(R2, na.rm = TRUE)
  )
