#'
#' Function to obtain the shop names of the shops selling the items returned by an Etsy.com search.
#'
#' This function obtains the names of the shops returned from an Etsy.com search by reading
#' the scraped xml.
#'
#' @param results_xml An xml object, obtained from scraping an Etsy search results webpage.
#'
#' @keywords html
#' @keywords xml
#' @keywords shops
#'
#' @return Returns the shop names that result from an Etsy.com search as a character vector.
#'
#' @example
#' etsyShopResults(results_xml)
#' etsyShopResults(xml_code)
#'
#' @export


etsyShopResults <- function(results_xml) {
  shops <- trimws(rvest::html_text(rvest::html_nodes(results_xml, "p.text-gray-lighter")), which = "both")
  return(shops)
}
