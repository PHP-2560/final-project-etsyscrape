#'
#' Function to obtain shop locations of the shops selling the items returned by an Etsy.com search.
#'
#' This function scrapes the location of each Etsy shop as listed on the shop's homepage.
#' \itemize{
#'   \item Utilizes the etsyShopResults() function to extract the name of each Etsy shop from the
#'   Etsy.com search results.
#'   \item Creates a URL for each individual shop, and scrapes the shop URL for the shop location.
#' }
#'
#' @param results_xml An xml object, obtained from scraping an Etsy search results webpage.
#'
#' @keywords html
#' @keywords xml
#' @keywords location
#'
#' @return Returns the shop locations for all shops selling an item returned by the search results,
#' as a character vector.
#'
#' @example
#' etsyShopLocations(results_xml)
#' etsyShopLocations(xml_code)
#'
#' @export

etsyShopLocations <- function(results_xml) {
  shop <- etsyShopResults(results_xml)
  locations <- vector(length = 0)

  for (i in 1:length(shop)) {
    location_url <- paste("https://www.etsy.com/shop/", shop[i], sep="")
    location_page <- xml2::read_html(location_url)

    location <- trimws(rvest::html_text(rvest::html_nodes(location_page, ".shop-location")), which = "both")[1]
    locations <- c(locations, location)
  }
  return(locations)
}
