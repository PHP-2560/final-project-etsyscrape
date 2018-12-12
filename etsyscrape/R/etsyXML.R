#'
#' Function to read and return the html of an Etsy search URL.
#'
#' This function accepts the URL of an Etsy.com search and scrapes the html.
#'
#' @param url The URL of the Etsy.com search.
#'
#' @keywords url
#' @keywords html
#' @keywords xml
#'
#' @return Returns the xml from scraping the provided URL.
#'
#' @examples
#' etsyXML("https://www.etsy.com/search?q=hello+kitty+purse&explicit=1&order=most_relevant")
#'
#' @export

etsyXML <- function(url){
  return(xml2::read_html(url))
}
