#'
#' Function to create Etsy.com URL for item search.
#'
#' This function creates the URL associated with an Etsy.com search for a specific item.
#'
#' @param search_string What item would you like to search for?
#'
#' @param search_order Specify the order in which the search results should be returned.
#'     Default is "most_relevent".
#'     Other options include "date_desc", "price_desc", "price_asc".
#'
#' @keywords url
#'
#' @return Returns an Etsy.com URL as a string.
#'
#' @examples
#' etsyURL("knitted scarf")
#' etsyURL("welcome sign", "date_desc")
#'
#' @export

etsyURL <- function(search_string, search_order="most_relevant") {

  base_url='https://www.etsy.com/search?q='

  search_string <- gsub(' ', '+', search_string, fixed = TRUE)

  search_order <- paste0('&explicit=1&order=', search_order)

  url <- paste0(base_url, search_string, search_order)

  return(url)

}


