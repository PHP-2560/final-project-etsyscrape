
# clean age data to use for shiny app tables

age <- readRDS("C:/Users/akosu/Desktop/final-project-etsyscrape/Shiny-App/age data/min_age_table.rds")

# rename variables
names(age)
colnames(age) <- c("Country", "On Premise", "Off Premise")

# delete first row of data as it is a duplicate of column names
age <- age[-1,]

# within each column, remove *
age$`On Premise` <- gsub("[*]", "", age$`On Premise`)
age$`Off Premise` <- gsub("[*]", "", age$`Off Premise`)

clean_age <- as.data.frame(age)
saveRDS(clean_age, "C:/Users/akosu/Desktop/final-project-etsyscrape/Shiny-App/age data/clean_age_app_tables.rds")