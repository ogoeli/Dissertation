library(sf)
library(sp)
library(tidyverse)
library(raster)
library(dplyr)

year_2015 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2015.gpkg")

year_2016 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2016.gpkg")

year_2017 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2017.gpkg")

year_2018 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2018.gpkg")

year_2019 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2019.gpkg")

year_2020 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2020.gpkg")

year_2021 <- st_read("/scratch/ope4/MERGE/BUFFER_EXPORT/year_2021.gpkg")


# Function to standardize LANDSAT column names
standardize_landsat_columns <- function(df) {
  # Replace the year-specific prefixes (e.g., LANDSAT_15_1 -> LANDSAT_1)
  colnames(df) <- gsub("LANDSAT_\\d{2}_(\\d+)", "LANDSAT_\\1", colnames(df))
  return(df)
}

# Apply this to all the year datasets
year_2015 <- standardize_landsat_columns(year_2015) |>   filter(LANDSAT_1 != 0)
year_2016 <- standardize_landsat_columns(year_2016) |>   filter(LANDSAT_1 != 0)
year_2017 <- standardize_landsat_columns(year_2017) |>   filter(LANDSAT_1 != 0)
year_2018 <- standardize_landsat_columns(year_2018) |>   filter(LANDSAT_1 != 0)
year_2019 <- standardize_landsat_columns(year_2019) |>   filter(LANDSAT_1 != 0)
year_2020 <- standardize_landsat_columns(year_2020) |>   filter(LANDSAT_1 != 0)
year_2021 <- standardize_landsat_columns(year_2021) |>   filter(LANDSAT_1 != 0)


####################################################################################################################

#year_2015 <- year_2015 |>
#  filter(status == 1)
  
# Calculate NDVI and NDMI
year_2015$NDVI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_4) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2015$NDWI <- (year_2015$LANDSAT_3 - year_2015$LANDSAT_5) / (year_2015$LANDSAT_3 + year_2015$LANDSAT_5)
year_2015$NDTI <- (year_2015$LANDSAT_9 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_9 + year_2015$LANDSAT_6)

# View results
print(year_2015[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2015$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2015$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")


# View results for NDVI, NDWI, and NDTI
print(year_2015[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2015$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")




# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2015 <- year_2015 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2015)
########################################################################################################################


####################################################################################################################

#year_2016 <- year_2016 |>
 # filter(status == 1)
# Calculate NDVI and NDMI
year_2016$NDVI <- (year_2016$LANDSAT_5 - year_2016$LANDSAT_4) / (year_2016$LANDSAT_5 + year_2016$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2016$NDWI <- (year_2016$LANDSAT_3 - year_2016$LANDSAT_5) / (year_2016$LANDSAT_3 + year_2016$LANDSAT_5)
# View results
print(year_2016[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2016$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2016$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")

year_2016$NDTI <- (year_2016$LANDSAT_9 - year_2016$LANDSAT_6) / (year_2016$LANDSAT_9 + year_2016$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2016[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2016$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")



# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2016 <- year_2016 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2016)
########################################################################################################################




####################################################################################################################

#year_2017 <- year_2017 |>
#  filter(status == 1)
# Calculate NDVI and NDMI
year_2017$NDVI <- (year_2017$LANDSAT_5 - year_2017$LANDSAT_4) / (year_2017$LANDSAT_5 + year_2017$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2017$NDWI <- (year_2017$LANDSAT_3 - year_2017$LANDSAT_5) / (year_2017$LANDSAT_3 + year_2017$LANDSAT_5)
# View results
print(year_2017[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2017$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2017$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")

year_2017$NDTI <- (year_2017$LANDSAT_9 - year_2017$LANDSAT_6) / (year_2017$LANDSAT_9 + year_2017$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2017[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2017$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")


# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2017 <- year_2017 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2017)


########################################################################################################################




####################################################################################################################

#year_2018 <- year_2018 |>
#  filter(status == 1)
# Calculate NDVI and NDMI
year_2018$NDVI <- (year_2018$LANDSAT_5 - year_2018$LANDSAT_4) / (year_2018$LANDSAT_5 + year_2018$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2018$NDWI <- (year_2018$LANDSAT_3 - year_2018$LANDSAT_5) / (year_2018$LANDSAT_3 + year_2018$LANDSAT_5)
year_2018$NDTI <- (year_2018$LANDSAT_9 - year_2018$LANDSAT_6) / (year_2018$LANDSAT_9 + year_2018$LANDSAT_6)
# View results
print(year_2018[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2018$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2018$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")

year_2018$NDTI <- (year_2018$LANDSAT_9 - year_2018$LANDSAT_6) / (year_2018$LANDSAT_9 + year_2018$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2018[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2018$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")


# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2018 <- year_2018 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2018)
########################################################################################################################




####################################################################################################################

#year_2019 <- year_2019 |>
 # filter(status == 1)
# Calculate NDVI and NDMI
year_2019$NDVI <- (year_2019$LANDSAT_5 - year_2019$LANDSAT_4) / (year_2019$LANDSAT_5 + year_2019$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2019$NDWI <- (year_2019$LANDSAT_3 - year_2019$LANDSAT_5) / (year_2019$LANDSAT_3 + year_2019$LANDSAT_5)
# View results
print(year_2019[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2019$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2019$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")



year_2019$NDTI <- (year_2019$LANDSAT_9 - year_2019$LANDSAT_6) / (year_2019$LANDSAT_9 + year_2019$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2019[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2019$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")



# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2019 <- year_2019 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2019)

########################################################################################################################


####################################################################################################################

#year_2020 <- year_2020 |>
 # filter(status == 1)
# Calculate NDVI and NDMI
year_2020$NDVI <- (year_2020$LANDSAT_5 - year_2020$LANDSAT_4) / (year_2020$LANDSAT_5 + year_2020$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2020$NDWI <- (year_2020$LANDSAT_3 - year_2020$LANDSAT_5) / (year_2020$LANDSAT_3 + year_2020$LANDSAT_5)
# View results
print(year_2020[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2020$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2020$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")



year_2020$NDTI <- (year_2020$LANDSAT_9 - year_2020$LANDSAT_6) / (year_2020$LANDSAT_9 + year_2020$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2020[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2020$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")



# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2020 <- year_2020 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2020)

########################################################################################################################




####################################################################################################################

#year_2021 <- year_2021 |>
#  filter(status == 1)
# Calculate NDVI and NDMI
year_2021$NDVI <- (year_2021$LANDSAT_5 - year_2021$LANDSAT_4) / (year_2021$LANDSAT_5 + year_2021$LANDSAT_4)
#year_2015$NDMI <- (year_2015$LANDSAT_5 - year_2015$LANDSAT_6) / (year_2015$LANDSAT_5 + year_2015$LANDSAT_6)
year_2021$NDWI <- (year_2021$LANDSAT_3 - year_2021$LANDSAT_5) / (year_2021$LANDSAT_3 + year_2021$LANDSAT_5)
# View results
print(year_2021[, c("NDVI", "NDWI")])
# Calculate the mean of NDVI and NDMI
mean_ndvi <- mean(year_2021$NDVI, na.rm = TRUE)  # Ignore NA values
#mean_ndmi <- mean(year_2015$NDMI, na.rm = TRUE)  # Ignore NA values
mean_ndwi <- mean(year_2021$NDWI, na.rm = TRUE)  # Ignore NA values
# Print the results
cat("Mean NDVI:", mean_ndvi, "\n")
#cat("Mean NDMI:", mean_ndmi, "\n")
cat("Mean NDWI:", mean_ndwi, "\n")




year_2021$NDTI <- (year_2021$LANDSAT_9 - year_2021$LANDSAT_6) / (year_2021$LANDSAT_9 + year_2021$LANDSAT_6)
# View results for NDVI, NDWI, and NDTI
print(year_2021[, c("NDVI", "NDWI", "NDTI")])
mean_ndti <- mean(year_2021$NDTI, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")

# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
year_2021 <- year_2021 %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndvi = mean(NDVI, na.rm = TRUE),
    mean_ndwi = mean(NDWI, na.rm = TRUE),
    mean_ndti = mean(NDTI, na.rm = TRUE)
  )

print(year_2021)
########################################################################################################################

table(data$STATUS)

#data$NEW <- (data$LANDSAT_9 - data$LANDSAT_5) / (data$LANDSAT_9 + data$LANDSAT_5)

data <- read_csv("/scratch/ope4/MERGE/DEC-04-2024/damage_points_cropped.csv") |>
  filter(SURVEY_YEAR == 2018) |>
  filter(STATUS != 2)

data$NEW <- (data$LANDSAT_5 - data$LANDSAT_9) / (data$LANDSAT_5 + data$LANDSAT_9)

#data$NEW <- (data$LANDSAT_5  /  data$LANDSAT_9)
# View results for NDVI, NDWI, and NDTI
mean_ndti <- mean(data$NEW, na.rm = TRUE)  # Ignore NA values
cat("Mean NDTI:", mean_ndti, "\n")

summary(data$NEW)

# Group by STATUS and calculate mean of NDVI, NDWI, and NDTI
data <- data %>%
  group_by(STATUS) %>%
  summarise(
    mean_ndti = mean(NEW, na.rm = TRUE)
  )

print(data)






