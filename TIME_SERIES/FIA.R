#https://sci-hub.se/https://doi.org/10.1016/j.envsoft.2020.104664
#https://doserlab.com/files/rfia/

install.packages("rFIA")
library(rFIA)
library(sf)
library(tidyverse)

options(timeout = 3600)  # sets timeout to 10 minutes


# Downloads and unzips data for the state
getFIA(states = 'AZ', dir = '/scratch/ope4/Downloads/DATA/FIA/fia_data', common = TRUE, load = FALSE)


my_grid <- st_read("/scratch/ope4/Downloads/DATA/FIA/POLY_1/POLY_1.shp")


## Load FIA Data from a local directory
fiaRI <- readFIA('/scratch/ope4/Downloads/DATA/FIA/fia_data')

# Only estimates for the most recent inventory year
fiaRI_MR <- clipFIA(fiaRI, mostRecent = TRUE) 
tpaRI_MR <- tpa(fiaRI_MR, totals = TRUE, byPlot = TRUE, treeType = 'all')
head(tpaRI_MR)

## Load FIA Data from a local directory
fiaRI <- readFIA('/scratch/ope4/Downloads/DATA/FIA/fia_data')

## Most recent inventory
fiaRI <- clipFIA(fiaRI, mostRecent = TRUE)


# ------------------------------
# 1. Convert FIA PLOT table to sf
# ------------------------------
fiaRI$plots <- fiaRI$PLOT %>%
  filter(!is.na(LAT) & !is.na(LON)) %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)

# Transform CRS to match your grid
fiaRI$plots <- st_transform(fiaRI$plots, st_crs(my_grid))

# ------------------------------
# 2. Check for overlapping plots
# ------------------------------

##find the nearest plot to my polygon
nearest_plot <- fiaRI$plots[which.min(st_distance(fiaRI$plots, st_centroid(my_grid))), ]
nearest_plot


# ------------------------------
# 3. Extract PLOT ID and INVYR
# ------------------------------
plot_ids <- nearest_plot$PLOT
invyrs    <- nearest_plot$INVYR

# ------------------------------
# 4. Extract TREE and COND info for these plots
# ------------------------------
tree_plot <- fiaRI$TREE %>%
  filter(PLOT %in% plot_ids, INVYR %in% invyrs)

cond_plot <- fiaRI$COND %>%
  filter(PLOT %in% plot_ids, INVYR %in% invyrs)

# ------------------------------
# 5. Calculate basal area (BAA) and TPA
# ------------------------------
tree_plot <- tree_plot %>%
  mutate(
    BAA = 0.005454 * DIA^2 * TPA_UNADJ  # basal area in ft²/acre
  )

# Total per plot
total_TPA <- sum(tree_plot$TPA_UNADJ, na.rm = TRUE) #TPA_UNADJUSTED
total_BAA <- sum(tree_plot$BAA, na.rm = TRUE)

tree_plot_small <- tree_plot |>
  select(
  "CN", "PLT_CN", "INVYR", "STATECD", "COUNTYCD", "PLOT", "SUBP",
  "TREE", "STATUSCD", "SPCD", "SPGRPCD",
  "DIA", "HT", "ACTUALHT", "CR", "TREECLCD", "BAA", "TPA_UNADJ"
)


# Remove rows with NA in TPA or BAA
tree_plot_sub <- tree_plot_small[!is.na(tree_plot_small$TPA_UNADJ), ]

# Acres in 100m x 100m cell
cell_acres <- 100 * 100 / 4046.85642  # 4046.85642 m² per acre 
#≈ 2.471 acres

# Total trees and basal area in the grid cell
total_trees <- sum(tree_plot_sub$TPA_UNADJ * cell_acres)
total_baa   <- sum(tree_plot_sub$BAA * cell_acres)

total_trees
total_baa




#---------------------------------------------------------------------------------------------------------------------
plot(tpaRI_MR)

# All Inventory Years Available (i.e., returns a time series)
tpaRI <- tpa(fiaRI)
head(tpaRI)



# Group estimates by species
tpaRI_species <- tpa(fiaRI_MR, bySpecies = TRUE, totals = TRUE, byPlot = TRUE, treeType = 'all')
head(tpaRI_species, n = 3)

# Group estimates by size class
# NOTE: Default 2-inch size classes, but you can make your own using makeClasses()
tpaRI_sizeClass <- tpa(fiaRI_MR, bySizeClass = TRUE)
head(tpaRI_sizeClass, n = 3)

# Group by species and size class, and plot the distribution 
# for the most recent inventory year
tpaRI_spsc <- tpa(fiaRI_MR, bySpecies = TRUE, bySizeClass = TRUE, totals = TRUE, byPlot = TRUE, treeType = 'all')
plotFIA(tpaRI_spsc, BAA, grp = COMMON_NAME, x = sizeClass,
        plot.title = 'Size-class distributions of BAA by species', 
        x.lab = 'Size Class (inches)', text.size = .75,
        n.max = 5) # Only want the top 5 species, try n.max = -5 for bottom 5




# grpBy specifies what to group estimates by (just like species and size class above)
# treeDomain describes the trees of interest, in terms of FIA variables 
# areaDomain, just like above, describes the land area of interest
tpaRI_own <- tpa(fiaRI_MR, 
                 grpBy = OWNGRPCD, 
                 treeDomain = DIA > 12 & CCLCD %in% c(1,2),
                 areaDomain = PHYSCLCD %in% c(20:29))
head(tpaRI_own)



df <- read_csv("/scratch/ope4/Downloads/DATA/FIA/AZ_PLOT.csv") |>
  filter(INVYR == 2021)

tpaRI_MR


df_split <- tpaRI_MR %>%
  separate(pltID, into = c("UNITCD", "STATECD", "COUNTYCD", "PLOT"), sep = "_", convert = TRUE) |>
  filter(YEAR == 2021)

print(df_split)


library(dplyr)

joined_df <- df %>%
  left_join(df_split, by = c("UNITCD","STATECD", "COUNTYCD", "PLOT")) |>
  filter(TPA != "NA") |>
  select(TPA, BAA,PLOT, LAT, LON, ELEV)










library(sf)
library(tigris)
library(dplyr)

# Load US counties for a state or all states
# options(tigris_use_cache = TRUE)  # caches shapefiles locally

counties_sf <- counties(cb = TRUE, year = 2021)  # get all US counties as sf object


# Convert points to sf object
points_sf <- st_as_sf(joined_df, coords = c("LON", "LAT"), crs = 4326)  # WGS84

# Ensure counties shapefile is also in the same CRS
counties_sf <- st_transform(counties_sf, crs = 4326)

# Spatial join to find counties for points
result <- st_join(points_sf, counties_sf, join = st_within)


# Group by county name and calculate means
county_summary <- result %>%
  st_drop_geometry() %>%  # remove geometry for easier summarizing
  group_by(STATEFP, COUNTYFP, NAME) %>%
  summarise(
    avg_BAA = mean(BAA, na.rm = TRUE),
    avg_TPA = mean(TPA, na.rm = TRUE),
    n_points = n()
  )

#Convert 100m x 100m to acres
grid_area_m2 <- 100 * 100
grid_area_acres <- grid_area_m2 / 4046.85642 #4046.85642m = 1 ACREAS
# ≈ 2.47 acres IN 100M BY 100M GRID CELL


# Add column to your summary
county_summary <- county_summary %>%
  mutate(
    grid_area_acres = grid_area_m2 / 4046.85642,
    trees_per_grid_cell = avg_TPA * grid_area_acres
  )


# Export as shapefile
st_write(result, "/scratch/ope4/Downloads/DATA/FIA/SHP/points_with_counties.shp", delete_layer = TRUE)
