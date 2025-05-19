library(httr)
library(jsonlite)



##############################
#Find Datasets
#############################
# Create the query with only soil category
query <- list(list(Category = "soil"), (Organization = "Jet Propulsion Laboratory"))

# Convert the list to JSON
query_json <- toJSON(query, auto_unbox = TRUE)

# Encode for URL
query_encoded <- URLencode(query_json)


#########################
#Get Datasets
#########################
# Construct the API request URL
rest_url <- paste0("https://ecosis.org/api/package/search?text=&filter=", query_encoded, "&start=0&stop=100")

# Print URL for debugging
print(rest_url)


# Define the URL for the API request
url <- rest_url

# Fetch the content directly
response <- curl_fetch_memory(url)

# Convert the response content to text
response_text <- rawToChar(response$content)

# Print the response
#print(response_text)

########################
#Get Spectra
#######################

# Parse the response content as JSON
response_json <- fromJSON(response_text)

# Print the structure of the parsed JSON to understand its format
str(response_json)


dataset_ids <- response_json$items$`Target Type` #[["_id"]]
print(dataset_ids)


# Assuming dataset_ids is a list of vectors
dataset_ids_flat <- data.frame(TYPE = unlist(dataset_ids))
soil_spectra <- dataset_ids_flat[grep("soil", dataset_ids_flat$TYPE, ignore.case = TRUE), ]


# Print the filtered dataset
print(soil_spectra)


# Get the first unique entry from the soil_spectra vector
soil_spectra_value <- unique(soil_spectra)[1]

# Construct the base URL
rest_url <- paste0("https://ecosis.org/api/spectra/search/", soil_spectra_value)

# Add query parameters to the URL
full_url <- paste0(rest_url, "?start=0&stop=2")

# Run the curl command
system(paste("curl", full_url))



# Step 1: Define the search URL (which you've already done)
rest_url <- full_url

# Step 2: Run the search query to get the response
json_response <- system(paste("curl", rest_url), intern = TRUE)

# Join the response into a single string (since it's line-by-line output)
json_response <- paste(json_response, collapse = "")

# Step 3: Parse the JSON response
library(jsonlite)
parsed_data <- fromJSON(json_response)

# Inspect the structure of the first item to see all its fields
str(parsed_data$items[[1]])

##########################
#Download Spectra
#########################

library(httr)

dataset_api_url <- "https://ecosis.org/api/package/drylands-spectral-libraries-in-support-of-emit"
#dataset_api_url <- "https://ecosis.org/api/package/emit-manually-adjusted-vegetation-reflectance-spectra"
dataset_info <- system(paste("curl", shQuote(dataset_api_url)), intern = TRUE)
print(dataset_info)



# Define the base API URL for the dataset
#dataset_api_url <- "https://ecosis.org/api/spectra/search/soil"

# Specify the filter for "soil"
filter <- '{"target_type": "soil"}'

# URL encode the filter
encoded_filter <- URLencode(filter)

# Construct the complete URL with the filter
rest_url <- paste0(dataset_api_url, "?start=0&stop=2&filters=", encoded_filter)

# Run the curl command to fetch the dataset details
dataset_info <- system(paste("curl", shQuote(rest_url)), intern = TRUE)

# Print the result
print(dataset_info)

library(jsonlite)

# Convert the JSON string into an R list
dataset_info_list <- fromJSON(dataset_info)

# Check the structure after parsing
str(dataset_info_list)




# Extract dataset URLs dynamically
data_url <- dataset_info_list$ecosis$resources$url[1]  # First file (simulation_asd_data.csv)
metadata_url <- dataset_info_list$ecosis$resources$url[2]  # Second file (simulation_asd_metadata.csv)

# File paths to save downloaded files
data_file_path <- "/scratch/ope4/LIDAR/simulation_asd_data.csv"
metadata_file_path <- "/scratch/ope4/LIDAR/simulation_asd_metadata.csv"

# Download files using extracted URLs
download.file(data_url, data_file_path)
download.file(metadata_url, metadata_file_path)





# Attempt to download the datasets
download.file("https://data.ecosis.org/dataset/a360734b-df9c-4c55-b2b1-b84e04def064/resource/fd83a444-7a09-4a7e-ab77-c887b9b25/simulation_asd_data.csv", "/scratch/ope4/LIDAR/simulation_asd_data.csv")
download.file("https://data.ecosis.org/dataset/d8a73442-a6f0-40f5-942b-1276d1725178/resource/adbf7136-de77-404e-a6df-995ac856f4c1/download/filtered_veg_metadata.csv", "/scratch/ope4/LIDAR/simulation_asd_metadata.csv")








download.file("https://data.ecosis.org/dataset/a360734b-df9c-4c55-b2b1-b84e04def064/resource/fd83a444-7a09-4a7e-ab77-c887b9b25/simulation_asd_data.csv", "/scratch/ope4/LIDAR/simulation_asd_data.csv")
download.file("https://data.ecosis.org/dataset/d8a73442-a6f0-40f5-942b-1276d1725178/resource/adbf7136-de77-404e-a6df-995ac856f4c1/download/filtered_veg_metadata.csv", "/scratch/ope4/LIDAR/simulation_asd_metadata.csv")

#download.file("https://data.ecosis.org/dataset/a360734b-df9c-4c55-b2b1-b84e04def064/resource/817b0c57-388b-452b-b3c5-acea8d04203c/download/simulation_asd_metadata.csv", "/scratch/ope4/LIDAR/simulation_asd_metadata.csv")







