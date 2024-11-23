library(sf)
library(raster)
library(tidyverse)
#library(terra)



####input your raster file
raster_stack <- raster("/scratch/ope4/MERGE/RASTER_TIFF/TREE_MASK.tif")

#plot(raster_stack)

####input your raster file
#raster_stack <- raster("/scratch/ope4/MERGE/2020_40N_120W.tif")

plot(raster_stack)

m <- c(-Inf, 1.9999, 0,
       2, 104, 1)

mat = matrix(m, ncol = 3, byrow = TRUE)



raster_stack_cat <- reclassify(raster_stack, mat)



# Plot the reclassified raster
plot(raster_stack_cat)

# Save the reclassified raster (optional)
writeRaster(raster_stack_cat, "/scratch/ope4/MERGE/RASTER_TIFF/raster_stack_cat.tif", overwrite = TRUE)








