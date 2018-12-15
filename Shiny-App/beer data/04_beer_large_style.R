# Creating a large_style variable column which contains the "large" style of the beer, 
# as opposed to the specific sub-style housed in "UT_sub_style". 

# Not every beer will have a "large_style", only those that are a sub-variety of a 
# broader style of beer. 

# If the beer is a sub-style of a broad style of beer, it will be labelled such as
# "Large Style - Sub Style" in UT_sub_style

# Read in the beer data with all locations, as saved from 03
complete_data <- readRDS("beer data/beer_data_loc_all.rds")

# This function searches one variable column for a specific string pattern, and returns
# the resulting matched pattern (or NA). 
ff = function(x, patterns, replacements = patterns, fill = NA, ...)
{
  stopifnot(length(patterns) == length(replacements))
  
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
  
  return(ans)
}

# Create a column called "large_style" that contains the returned matched patterns
# from searching each UT_sub_style for each broad beer style. 
complete_data$large_style <- ff(complete_data$UT_sub_style, 
                                c("Brown Ale", "IPA", "Red Ale", "Lager", 
                                  "Pale Ale", "Pilsner", "Porter", "Sour", "Stout"), 
                                c("Brown Ale", "IPA", "Red Ale", "Lager", 
                                  "Pale Ale", "Pilsner", "Porter", "Sour", "Stout"), 
                                ignore.case = TRUE )

# The data frame now contains all variables of interest, and is saved as
# the beer_complete_data file. 
saveRDS(complete_data, file = "beer_complete_data.rds")
