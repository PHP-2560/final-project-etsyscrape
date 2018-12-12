
complete_data <- readRDS("beer data/beer_data_loc_all.rds")

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

complete_data$large_style <- ff(complete_data$UT_sub_style, c("Brown Ale", "IPA", "Red Ale", "Lager", 
                                                              "Pale Ale", "Pilsner", "Porter", "Sour", "Stout"), 
                                c("Brown Ale", "IPA", "Red Ale", "Lager", 
                                  "Pale Ale", "Pilsner", "Porter", "Sour", "Stout"), ignore.case = TRUE )

saveRDS(complete_data, file = "beer_complete_data.rds")
