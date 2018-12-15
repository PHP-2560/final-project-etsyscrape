library(dplyr)
library(countrycode)

min_age_table <- readRDS("min_age_table.rds")

colnames(min_age_table) <- c("country", "age", "offage")

min_age_table$age <- as.numeric(gsub("[*]", "", min_age_table$age))

age_tbl <- min_age_table %>%
  select(country, age)

age_tbl <- age_tbl[-1,]

age_tbl$code <- countrycode(age_tbl$country, 'country.name', 'iso3c')

saveRDS(age_tbl, "age_tbl.rds")
