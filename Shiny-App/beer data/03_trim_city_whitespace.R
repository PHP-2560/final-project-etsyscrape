library(dplyr)

# From the way the data was organized, some whitespace remains surrounding the 
# city names, which negatively impacts sorting functions. 

# Although this could be completed in 02 file before saving the loc_all.rds,
# it would require another lengthy pull from the geocodes API to accomplish. 
# Therefore, we remove whitespace here, separately. 

# Read in the data frame
data_now <- readRDS("beer_data_loc_all.rds")

# Trim any whitespace surrounding the city name
data_now$city <- trimws(as.character(data_now$city))

# Save the new data frame
saveRDS(data_now, "beer_data_loc_all.rds")
