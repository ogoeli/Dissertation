colnames(df)[colnames(df) == '.geo'] <- 'geom'

df <- df[!duplicated(df[c("geom")]), ]



inner_join <- inner_join(
  results_df_unique,
  df,
  by = "geom")

####class 3
colnames(df_3)[colnames(df_3) == '.geo'] <- 'geom'

df_3 <- df_3[!duplicated(df_3[c("geom")]), ]



inner_join_2 <- inner_join(
  results_df_unique_2,
  df_3,
  by = "geom")


dataset <- rbind(inner_join_2, inner_join)


# Extract coordinates and split into longitude and latitude
coords <- t(sapply(dataset$geom, function(x) {
  coords <- fromJSON(x)$coordinates
  return(coords)
}))

# Assign longitude and latitude to separate columns
dataset$longitude <- coords[, 1]
dataset$latitude <- coords[, 2]

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
       x = "Longitude", y = "Latitude", color = "First CP")








# Load required libraries
install.packages("jsonlite")
library(jsonlite)

# Example dataset (replace with your actual dataset)
dataset <- data.frame(
  geom = c('{"type":"Point","coordinates":[-112.33732128307028,36.36633687351052]}',
           '{"type":"Point","coordinates":[-112.33243856741677,36.36636362811684]}',
           '{"type":"Point","coordinates":[-112.32869738163292,36.36637700542]}'),
  band = c('NDVI', 'NDVI', 'NDVI'),
  RMSE = c(0.13224034, 0.09499096, 0.12670167),
  R2 = c(0.6571823, 0.8411857, 0.8281847),
  ncp = c(2.543875, 2.893583, 2.989709),
  cp = c(2020.167, 2020.167, 2020.167),
  date = c("2020-01-01", "2020-01-02", "2020-01-03")  # Add time series column
)

# Parse the geom column and extract the coordinates
dataset$geo_node <- sapply(dataset$geom, function(x) {
  coords <- fromJSON(x)$coordinates
  paste(coords, collapse = ",")  # Combine lat and lon into a string
})

# Reshape the data into a TxN format (time, geo_node, RMSE)
txn_data <- dataset %>%
  dplyr::select(date, geo_node, RMSE) %>%
  arrange(date, geo_node)

# View the result
print(txn_data)

# Save the data to a CSV
write.csv(txn_data, "txn_shaped_data.csv", row.names = FALSE)








###################################################################


install.packages("spatstat")
library(spatstat)


library(spatstat.geom)


# Randomly select 50% of rows
data <- data %>%
  slice_sample(prop = 0.01)



# Define ranges based on your data
xrange <- range(data$longitude, na.rm = TRUE)
yrange <- range(data$latitude, na.rm = TRUE)

m <- factor(data$cover)


# Create the ppp object
point_pattern <- ppp(x = data$longitude,
                     y = data$latitude, marks = m,
                     window = owin(xrange = xrange, yrange = yrange))

unitname(point_pattern) <- c("metre", "metres")
point_pattern
plot(point_pattern)
point_pattern$markformat


contour(density(point_pattern, 10), axes = FALSE)

#To extract this intensity value,
lamb <- summary(point_pattern)$intensity
lamb


#Quadrat counting
Q <- quadratcount(point_pattern, nx = 6, ny = 3)
plot(point_pattern, cex = 0.5, pch = "+")
plot(Q, add = TRUE, cex = 2)

#Kernel density (or intensity) estimation using an isotropic Gaussian kernel
den <- density(point_pattern, sigma = 70)
plot(den)
plot(point_pattern, add = TRUE, cex = 0.5)


#adaptive estimator of intensity which uses a fraction f of the 
#data to construct a Dirichlet tessellation, then forms an intensity estimate that is constant in
#each tile of the tessellation
aden <- adaptive.density(point_pattern, f = 0.01, nrep = 31)
plot(aden, main = "Adaptive intensity")
plot(point_pattern, add = TRUE, cex = 0.5)


# define the quadrats using covariate information
#divided the study region into four zones of equal area according to the covarites
Z <- co_var$NDVI
b <- quantile(Z, probs = (0:4)/4)
Zcut <- cut(Z, breaks = b, labels = 0:4)
V <- tess(image = Zcut)
plot(V)
plot(point_pattern, add = TRUE, pch = "+")




#returns a pixel image whose pixel values are the empty space distances to the
#pattern X measured from every pixel.
emp <- distmap(point_pattern)
plot(emp, main = "Empty space distances")
plot(point_pattern, add = TRUE)
plot(point_pattern %mark% (nndist(point_pattern)/2), markscale = 1, main = "Stienen diagram")



#NN distance
Gc <- Gest(point_pattern)
Gc
plot(Gest(point_pattern))




# get just the B1 column for example
mat <- matrix(data$B12, nrow = 117, byrow = TRUE)  # adjust size as needed

xcol <- matrix(data$longitude, nrow = 117, byrow = TRUE)
yrow <- matrix(data$latitude, nrow = 117, byrow = TRUE)

xcol <- as.vector(xcol[1, ])  # row vector, 43 long
yrow <- as.vector(yrow[, 1])  # column vector, 117 long


B12 <- im(mat, xcol, yrow)

mat <- matrix(data$B8, nrow = 117, byrow = TRUE)  # adjust size as needed

xcol <- matrix(data$longitude, nrow = 117, byrow = TRUE)
yrow <- matrix(data$latitude, nrow = 117, byrow = TRUE)

xcol <- as.vector(xcol[1, ])  # row vector, 43 long
yrow <- as.vector(yrow[, 1])  # column vector, 117 long


B8 <- im(mat, xcol, yrow)

# Compute vegetation indices
data <- data %>%
  mutate(
    NDVI = (B8 - B4) / (B8 + B4))


mat <- matrix(data$NDVI, nrow = 117, byrow = TRUE)  # adjust size as needed

xcol <- matrix(data$longitude, nrow = 117, byrow = TRUE)
yrow <- matrix(data$latitude, nrow = 117, byrow = TRUE)

xcol <- as.vector(xcol[1, ])  # row vector, 43 long
yrow <- as.vector(yrow[, 1])  # column vector, 117 long


NDVI <- im(mat, xcol, yrow)

nrow(data)

factors <- which(5031 %% 1:5031 == 0)
factors

co_var <- list(
  B8 = B8,
  B12 = B12,
  NDVI = NDVI
)


#apply adaptive estimation of intensity to each class in my dataset
V <- split(point_pattern)
A <- lapply(V, adaptive.density)
plot(as.listof(A))



fit <- ppm(point_pattern, ~ B12 + B8 + NDVI, covariates = co_var)
#fit another ppm with tree height, ndvi and other covarites
lam <- predict(fit, locations = point_pattern, type = "trend")
lam_interp <- zoo::na.approx(lam,rule = 2) ## to handle the NAs
Ki <- Kinhom(point_pattern, lam_interp)
plot(Ki, main = "Inhomogeneous K function")
plot(fit)






#pointwise envelopes, which are often used in spatial point pattern analysis 
#(especially with spatstat in R) to visually assess whether a spatial point pattern 
#deviates from complete spatial randomness (CSR)
E <- envelope(fit, Lest, nsim = 19, global = TRUE, correction = "border")
plot(E, main = "envelope for inhomogeneous Poisson")


data(cells)
SimPatList <- list()
for (i in 1:1000) SimPatList[[i]] <- runifpoint(point_pattern$n)
EK <- envelope(point_pattern, Kest, simulate = SimPatList, nsim = 1000)
Ep <- envelope(point_pattern, pcf, simulate = SimPatList, nsim = 1000)





#Analysis of deviance for nested Poisson point process models is implemented in spatstat as
#anova.ppm. The first model should be a sub-model of the second.MODEL SEELCTION
fit <- ppm(point_pattern, ~NDVI+B12+B8, covariates = co_var)
fitnull <- update(fit, ~1)
anova(fitnull, fit, test = "Chi")


#The χ2 goodness-of-fit test based on quadrat counts can be applied to a fitted Poisson model,
#homogeneous or inhomogeneous.
fit <- ppm(point_pattern, ~x)
M <- quadrat.test(fit, nx = 4, ny = 2)
M
#in what way the data appear to depart from the predictions of the model
#The plot displays, for each quadrat, the observed number of points (top left), the predicted
#number of points according to the model (top right), and the Pearson residual (bottom) defined by
#Pearson residual = (observed) − (expected)√expected
plot(point_pattern, pch = ".")
plot(M, add = TRUE, cex = 1.5, col = "red")


### Inhomogeneous cluster point process model

fit <- kppm(point_pattern, ~NDVI + B12, "Thomas", covariates = co_var)
fit
lam <- predict(fit, locations = point_pattern, type = "trend")
lam_interp <- zoo::na.approx(lam,rule = 2) ## to handle the NAs
Ki <- Kinhom(point_pattern, lam_interp)
plot(Ki, main = "Inhomogeneous K function")
plot(fit)



ppm(point_pattern, ~marks)

