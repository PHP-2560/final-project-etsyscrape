library(dplyr)
library(countrycode)

# Read in the minimum drinking age by countries table
min_age_table <- readRDS("min_age_table.rds")

# Change the column names to easy-to-use names
colnames(min_age_table) <- c("country", "age", "offage")

# Convert the age variable to numeric values
min_age_table$age <- as.numeric(gsub("[*]", "", min_age_table$age))

# Using only the on premise ages for each country
age_tbl <- min_age_table %>%
  select(country, age)

# Removing the first row, which is only titles
age_tbl <- age_tbl[-1,]

# Adding a new column to the data frame containing the country code
# as dictated by ISO3C standards
age_tbl$code <- countrycode(age_tbl$country, 'country.name', 'iso3c')

# Save this data, with only one age per country, and country codes, into
# a file
saveRDS(age_tbl, "age_tbl.rds")
