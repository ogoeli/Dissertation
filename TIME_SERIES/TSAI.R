library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Read CSV
df <- read_csv("/scratch/ope4/Downloads//DATA/CSV/DATA/boundary.csv")

# Select required columns
df <- df %>% 
  dplyr::select(system.index, geometry, B1, B2, B3, B4, B5, B6, B7, B9, B10, B11, cover)


table(df$cover)

# Group by geometry
point_groups <- df %>% 
  group_by(geometry) %>% 
  arrange(system.index, .by_group = TRUE) %>% 
  group_split()

# Set minimum sequence length
min_sequence_length <- 113

# Extract sequences
sequences <- map(point_groups, function(group) {
  features <- group %>% 
    dplyr::select(B1, B2, B3, B4, B5, B6, B7, B9, B10, B11)
  
  if (nrow(features) >= min_sequence_length) {
    list(
      X = as.matrix(features),
      y = group$cover[1]
    )
  } else {
    NULL
  }
}) %>% compact()  # Remove NULLs

# Extract X and y
X <- array(
  unlist(map(sequences, "X")), 
  dim = c(length(sequences), min_sequence_length, 10)
)

y <- unlist(map(sequences, "y"))

# Check shapes
cat("X shape:", dim(X), "\n")
cat("y shape:", length(y), "\n")


install.packages("reticulate")
library(reticulate)

# Save as .npz using reticulate (calls Python)
np <- import("numpy")
np$savez("boundary_timeseries_data.npz", X = X, y = y)



#####################################################


library(readr)
library(dplyr)
library(purrr)

# Read the CSV
df <- read_csv("/scratch/ope4/Downloads//DATA/CSV/DATA/boundary.csv", show_col_types = FALSE)

# Select relevant columns
df <- df %>%
  dplyr::select(system.index, geometry, B1, B2, B3, B4, B5, B6, B7, B9, B10, B11, cover) |>
filter(cover != 3)

# Group by geometry
point_groups <- df %>%
  group_by(geometry) %>%
  group_split()

# Set minimum sequence length
min_sequence_length <- 113

# Initialize lists
X <- list()
y <- c()

# Iterate over each group
for (group in point_groups) {
  # Sort by system.index to maintain temporal order
  group <- group %>% arrange(system.index)
  
  # Extract features as matrix
  features <- group %>%
    dplyr::select(B1, B2, B3, B4, B5, B6, B7, B9, B10, B11) %>%
    as.matrix()
  
  # Append features and label
  X[[length(X) + 1]] <- features
  y <- c(y, group$cover[1])
}


# Check dimensions
print(length(X))        # Number of samples
print(length(y))        # Number of labels
print(dim(X[[1]]))      # Shape of one time series sample (sequence_length, n_bands)




library(reticulate)

# 1. Import numpy
np <- import("numpy")

# 2. Convert R list `X` to a 3D array
#    X is currently a list of matrices with dim 132x10

X_array <- array(unlist(X), dim = c(132, 10, length(X)))  # [time, bands, samples]
X_array <- aperm(X_array, c(3, 1, 2))  # [samples, time, bands]

# 3. Convert `y` to a simple vector
y_vector <- unlist(y)

# 4. Save as .npz
np$savez("boundary_timeseries_data.npz", X = X_array, y = y_vector)
