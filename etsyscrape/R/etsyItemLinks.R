#'
#' Function to obtain URL links for each item returned by an Etsy.com search.
#'
#' This function returns the unique URL for each item resulting from an Etsy.com search.
#'
#' @param results_xml An xml object, obtained from scraping an Etsy search results webpage. 
#'
#' @keywords xml
#' @keywords html
#' @keywords item
#'
#' @return Returns a character vector of URLs, containing the unique URL for each item resulting from an Etsy.com search.
#'
#' @examples
#' etsyItemLinks(results_xml)
#' etsyItemLinks(xml_code)
#'
#' @export

etsyItemLinks <- function(results_xml) {
  num_items <- length(etsyscrape::etsyItemResults(results_xml))
  links <- vector(length = 0)
  
  for (i in 1:num_items) {
    links <- c(links, (rvest::html_attr(xml2::xml_child(xml2::xml_child(rvest::html_nodes(results_xml, ".block-grid-item")[[i]],1),1), "href")))
  }
  return(links)
}
