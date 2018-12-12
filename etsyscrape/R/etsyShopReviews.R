#'
#' Function to obtain the customer text-based reviews of each shop selling an item returned by an Etsy.com search.
#'
#' This function scrapes the customer reviews of each Etsy shop.
#' \itemize{
#'   \item Utilizes the etsyShopResults() function to extract the name of each Etsy shop from the
#'   Etsy.com search results.
#'   \item Creates a URL for each individual shop, and scrapes the shop URL for the shop reviews.
#'   \item Collapses each individual review from the first page of the shop's reviews (only)
#'   into one text paragraph.
#' }
#'
#' @param results_xml An xml object, obtained from scraping an Etsy search results webpage
#'
#' @keywords html
#' @keywords xml
#' @keywords reviews
#' @keywords shop
#'
#' @return Returns a character vector of paragraphs representing the reviews from each individual shop.
#'
#' @example
#' etsyShopReviews(results_xml)
#' etsyShopReviews(xml_code)
#'
#' @export

etsyShopReviews <- function(results_xml) {
  shop <- etsyShopResults(results_xml)
  reviews <- vector(length = 0)

  for (i in 1:length(shop)) {
    reviews_url <- paste("https://www.etsy.com/shop/", shop[i], "/reviews", sep="")
    page <- xml2::read_html(reviews_url)
    review_text <- paste(rvest::html_text(rvest::html_nodes(page, ".m-xs-0")), collapse =" ", sep=" ")
    reviews <- c(reviews, review_text)
  }
  return(reviews)
}
