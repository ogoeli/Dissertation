# Install required packages if you haven't already
#install.packages("httr")
#install.packages("jsonlite")




library(httr)
library(jsonlite)
library(curl)

# Create a list of queries
query <- list(
  list(Category = "soil"),
  list(Organization = "Jet Propulsion Laboratory")
)


query <- list(
  Category = "soil",
  Organization = "Jet Propulsion Laboratory"
)
# Convert the list into a JSON string
query_json <- jsonlite::toJSON(query, auto_unbox = TRUE)

# Escape the JSON string for URL component
query_encoded <- URLencode(query_json)

# Construct the REST URL
rest_url <- paste0("https://ecosis.org/api/package/search?text=&filter=", query_encoded, "&start=0")

# Print the URL
print(rest_url)



# Use system() to run curl command in R with the correct search URL
#search_url <- "https://ecosis.org/api/package/search?text=&filter=[%7B%22Category%22:%22soil%22%7D,%7B%22Organization%22:%22Jet%20Propulsion%20Laboratory%22%7D]&start=0&stop=6"

search_url <- rest_url

# Run the curl command from R
#system(paste("curl", search_url))


# Define the URL for the API request
url <- rest_url

# Fetch the content directly
response <- curl_fetch_memory(url)

# Convert the response content to text
response_text <- rawToChar(response$content)

# Print the response
#print(response_text)



# Parse the response content as JSON
response_json <- fromJSON(response_text)

# Print the structure of the parsed JSON to understand its format
str(response_json)


dataset_ids <- response_json$items[["_id"]]
print(dataset_ids)

# Filter for rows where the 'Keywords' column contains 'soil'
soil_spectra <- dataset_ids[grep("soil", dataset_ids$Keywords, ignore.case = TRUE), ]

# Print the filtered dataset
print(soil_spectra)

str(response_json$filters)

###
# Assuming rest_url contains the correct URL for spectra search (without the final query parameters)

# Define the base URL for spectra search (adjust rest_url to contain the correct dataset ID)
rest_url <- paste0("https://ecosis.org/api/spectra/search/", dataset_ids)

# Construct the full URL with query parameters
full_url <- paste0(rest_url, "?start=0&stop=2")

# Run the curl command from R using system()
system(paste("curl", full_url))

