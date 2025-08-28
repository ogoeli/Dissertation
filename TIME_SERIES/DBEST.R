library(sf)
library(sp)
library(tidyverse)
library(raster)
library(data.table)
library(terra)
library(jsonlite)
library(bfast)
library(Rbeast)
library(DBEST)
library(zoo)
library(xts)


ndvi_df_beetle <- read.csv('/scratch/ope4/Downloads/DATA/CSV/Synthetic_data_NDVI_beetle_3.csv')
# Convert the 'date' column to Date class
ndvi_df_beetle$date <- as.Date(ndvi_df_beetle$date)


ndvi_df_Drought_event <- read.csv('/scratch/ope4/Downloads/DATA/CSV/Synthetic_data_NDVI_drought_event_2.csv')
# Convert the 'date' column to Date class
ndvi_df_Drought_event$date <- as.Date(ndvi_df_Drought_event$date)








# ==============================================================================
# Plot with Date as primary x-axis and Weeks as secondary x-axis
ggplot(ndvi_df_beetle, aes(x = date, y = ndvi)) +
  geom_line(color = "forestgreen") +
  geom_vline(xintercept = as.Date("2022-12-27"), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = as.Date("2022-12-27") + 70, y = 0.9,
           label = "predicted Bark beetle outbreak", color = "red", size = 4, hjust = 0) +
  scale_x_date(
    name = "Date",
    date_breaks = "1 year",
    date_labels = "%Y",
    sec.axis = sec_axis(
      ~ .,  # Directly use the existing date values for the primary x-axis
      breaks = ndvi_df_beetle$date[seq(1, nrow(ndvi_df_beetle), by = 52)],  # Set breaks based on weeks
      labels = ndvi_df_beetle$day_of_year[seq(1, nrow(ndvi_df_beetle), by = 52)],  # Set labels based on weeks
      name = "Sequential Week"
    )
  ) +
  labs(
    title = "Synthetic Weekly NDVI (2019–2025) with Bark Beetle Infestation",
    y = "NDVI"
  ) +
  theme_minimal()




# ==============================================================================
# Synthetic NDVI data with a drought event to simulate a time series for testing change-point detection models.

# Plot with Date as primary x-axis and Sequential Weeks as secondary x-axis
ggplot(ndvi_df_Drought_event, aes(x = date, y = ndvi)) +
  geom_line(color = "forestgreen") +
  geom_vline(xintercept = as.Date("2021-06-01"), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = as.Date("2021-06-01") + 70, y = 0.9,
           label = "Drought event hit", color = "red", size = 4, hjust = 0) +
  scale_x_date(
    name = "Date",
    date_breaks = "1 year",
    date_labels = "%Y",
    sec.axis = sec_axis(
      trans = ~ .,
      breaks = ndvi_df_Drought_event$date[seq(1, nrow(ndvi_df_Drought_event), by = 52)],  # Breaks for every 52 weeks (1 year)
      labels = ndvi_df_Drought_event$day[seq(1, nrow(ndvi_df_Drought_event), by = 52)],  # Sequential weeks as labels
      name = "Sequential Week"
    )
  ) +
  labs(
    title = "Synthetic Weekly NDVI (2019–2025) - Drought Event Impact",
    y = "NDVI"
  ) +
  theme_minimal()

#################################################################################################################



ts_bettle <- ts(ndvi_df_beetle$ndvi, frequency = 365, start = c(2019, 1)) 
# Interpolate missing values
ts_bettle <- na.approx(ts_bettle)





ts_drought <- ts(ndvi_df_Drought_event$ndvi, frequency = 365, start = c(2019, 1)) 

head(time_series)


############# DBEST MODEL ###################

DBEST.bettle <- DBEST(data=time_series, data.type="cyclical", 
                     seasonality=12, algorithm="change detection", 
                     breakpoints.no=3, first.level.shift=0.1, 
                     second.level.shift=0.2, duration=144, 
                     distance.threshold="default", alpha=0.05, plot="on")

print(DBEST.bettle)



DBEST.drought <- DBEST(data=ts_drought, data.type="cyclical", 
                      seasonality=365, algorithm="change detection", 
                      breakpoints.no=3, first.level.shift=0.1, 
                      second.level.shift=0.2, duration=800, 
                      distance.threshold="default", alpha=0.05, plot="on")

print(DBEST.drought)


######################################################


############################### BEAST MODEL ####################################


BEAST_BETTLE <- beast(ts_bettle, # start  = c(2019,01,01),deltat = 1/365,  # period = 365      
          season  = 'harmonic')  
print(BEAST_BETTLE)
plot(BEAST_BETTLE)




BEAST_DROUGHT <- beast(ts_drought,                  # The supplied time/period values will be ignored bcz
                      start  = c(2019,01,01),# co2 is a ts object; the correct period = 1 will be 
                      deltat = '1 day',        # used.
                      season  = 'harmonic',
                      period = 365)  
print(BEAST_DROUGHT)
plot(BEAST_DROUGHT)


############################################################################################



#################################### BFAST MODE ###########################




rdist <- 10/length(ts_bettle)
# ratio of distance between breaks (time steps) and length of the time series
BFAST_BETTLE <- bfast(time_series, season = "harmonic")
plot(BFAST_BETTLE)
BFAST_BETTLE



BFAST_DROUGHT <- bfast(ts_drought, season = "harmonic")
plot(BFAST_DROUGHT)
BFAST_DROUGHT


##########################################################################################################



