#'
#' Function to obtain the names of the items returned from an Etsy.com search.
#'
#' This function obtains the names of the items returned from an Etsy.com search by reading
#' the scraped xml.
#'
#' @param results_xml An xml object, obtained from scraping an Etsy search results webpage.
#'
#' \emph{Note: The xml is easily obtained using the etsyURL() and etsyXML() functions.}
#'
#' @keywords html
#' @keywords xml
#' @keywords item
#'
#' @return Returns the names of items that result from an Etsy.com search as a character vector.
#'
#' @examples
#' etsyItemResults(results_xml)
#' etsyItemResults(xml_code)
#'
#' @export

etsyItemResults <- function(results_xml) {
  items <- trimws(rvest::html_text(rvest::html_nodes(results_xml, ".text-body")), which = "both")
  return(items)
}
