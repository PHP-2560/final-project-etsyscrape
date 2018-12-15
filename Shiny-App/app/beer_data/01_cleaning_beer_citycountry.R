library(dplyr)
library(stringr)
library(miceadds)

# Import data from beer_ratings source
data <- load.Rdata2("beer data/original_beer_ratings_data.RData", path=getwd())

# Convert location column into character string
data$UT_loc <- as.character(data$UT_loc)

# Create the Loc_USA column, a logical vector indicating whether the location contains "United States"
data$Loc_USA <- str_detect(data$UT_loc, pattern = "United States")

# Create new data frame for only American data based on Loc_USA == TRUE
data_USA <- filter(data, Loc_USA == TRUE) 

# Create another data frame for non-American data
data_Other <- filter(data, Loc_USA == FALSE)

# Add city information to the American data by selecting the city
data_USA$city <- str_split(data_USA$UT_loc, ",")
data_USA$city <- lapply(data_USA$city,"[", 1)

# Add state information to American data by selecting the state code
data_USA$state <- str_extract(data_USA$UT_loc, "[:upper:][:upper:]")

# Add country to American data
data_USA$country <- str_extract(data_USA$UT_loc, "United States")

# Extract Canadian data by creating a Loc_Canada logical vector and filtering
data_Other$Loc_Canada <- str_detect(data_Other$UT_loc, pattern = "Canada")
data_Canada <- filter(data_Other, Loc_Canada == TRUE)

# Add city to Canadian data
data_Canada$city <- str_split(data_Canada$UT_loc, ",")
data_Canada$city <- lapply(data_Canada$city,"[", 1)

# Add state to Canadian data
data_Canada$state <- str_extract(data_Canada$UT_loc, "[:upper:][:upper:]")

# Add country to Canadian data
data_Canada$country <- str_extract(data_Canada$UT_loc, "Canada")

# All the remaining data is not from Canada or the United states 
data_unfinished <- filter(data_Other, Loc_Canada == FALSE)

# The country of the unfinished data frame is set as the last word in the string
data_unfinished$country <- word(data_unfinished$UT_loc, -1)

data_unfinished$country <- str_replace(data_unfinished$country, "Zealand", "New Zealand")
data_unfinished$country <- str_replace(data_unfinished$country, "Africa", "South Africa")

# The city of the unfinished data frame is set as the first word in the string
data_unfinished$city <- str_split(data_unfinished$UT_loc, ",")
data_unfinished$city <- lapply(data_unfinished$city,"[", 1)

# Everything that is not Canada or USA will have NA as the state value
data_unfinished$state <- NA

# Refine the data to the columns of interest (or potential interest)
data_USA <- data_USA %>%
  select(BA_super_style, UT_sub_style, BA_sub_style, UT_beer_name, UT_brewery, BA_beer_name, BA_brewery,
UT_ABV, BA_ABV, UT_IBU, UT_rating, UT_raters, BA_rating, BA_raters, UT_loc, city, state, country)

data_Canada <- data_Canada %>%
  select(BA_super_style, UT_sub_style, BA_sub_style, UT_beer_name, UT_brewery, BA_beer_name, BA_brewery,
         UT_ABV, BA_ABV, UT_IBU, UT_rating, UT_raters, BA_rating, BA_raters, UT_loc, city, state, country)

data_unfinished <- data_unfinished %>%
  select(BA_super_style, UT_sub_style, BA_sub_style, UT_beer_name, UT_brewery, BA_beer_name, BA_brewery,
         UT_ABV, BA_ABV, UT_IBU, UT_rating, UT_raters, BA_rating, BA_raters, UT_loc, city, state, country)

# Combine all of the refined data frames to create one cleaned data set
data_clean <- rbind(data_USA, data_Canada, data_unfinished)

# Remove any unwanted characters from the city names
data_clean$city <- gsub("[^[:alpha:]]", " ", data_clean$city)

# Save the cleaned data with city and country info
saveRDS(data_clean, file = "beer_data_citycountry.rds")
