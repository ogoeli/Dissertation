# Load necessary libraries
library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv("/scratch/ope4/MERGE/DEC-04-2024/combined_dataset_BUFFER.csv")



# Aggregate the data to count the number of status 0 and 1 for each year
status_count <- data %>%
  group_by(SURVEY_YEAR, STATUS) %>%
  summarise(count = n(), .groups = "drop") 

status_count <- status_count |>
  filter(STATUS == 1)

# Bar plot of the status counts by year
ggplot(status_count, aes(x = factor(SURVEY_YEAR), y = count)) +
  geom_bar(position="dodge", stat="identity", width = 0.5, color="black",size=1)+  
  theme_minimal() +
  labs(title = "", 
       x = "Year", y = "Number of infected trees") +
  scale_fill_viridis_d(option = "D") +  
  theme(axis.text = element_text(size = 12, color = 'black'),  
        axis.title = element_text(size = 12),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.line = element_line(size = 0.1), 
        panel.background = element_rect(fill = 'transparent')) +  
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 






########----
data <- read.csv("FIG6.csv")



# Create the data frame
data <- data.frame(
  Year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021),
  mean_prec = c(41.435186, 45.602749, 44.419104, 45.012498, 27.805405, 14.270229, 53.384739),
  mean_tmax = c(27.155556, 24.761508, 25.176577, 26.441218, 25.014152, 26.2852, 25.732988),
  mean_tmin = c(11.137037, 8.031445, 8.27091, 9.189257, 8.178187, 9.00068, 8.930264),
  count = c(3, 3315, 3592, 6203, 4242, 3783, 2967)
)


data <- data[, -1]

#####convert the data frame to numeric
df2 <- data.frame(sapply(data, function(x) as.numeric(as.character(x))))
str(df2)

correl <- corr.test(as.matrix(data), use = "pairwise", method="pearson")


P <- corr.p(correl$r, correl$n)

R <- round(correl$r,2) #Extract the correlation coefficients
write.csv(R, "R_PEARSON_NOLOG_regression.csv")

R <- correl$p #Extract the p-values
write.csv(R,"P_PEARSON_N0LOG_regression.csv")



