#' Generate dataset citations
#' 
#' \code{dataset_citations} returns dataset citations as text or as 
#' "\code{\link[utils]{bibentry}}" objects.
#' 
#' @usage dataset_citations(datasets, astext = TRUE)
#'   
#' @param datasets character vector. Code of the datasets. When o datasets are
#'   provided (default), then all citations are returned.
#' @param astext logical. When \code{TRUE} (default), then citations are 
#'   returned as text otherwise they are returned as 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @details Note that for some datasets multiple references are returned as some
#'   of the predictors might have been published separately.
#'   
#' @return Either a character vector or a list of 
#'   "\code{\link[utils]{bibentry}}" objects.
#'   
#' @examples
#' # print the Bio-ORACLE citation
#' print(dataset_citations("Bio-ORACLE"))
#' 
#' # print all citations as Bibtex
#' print(sapply(dataset_citations(astext = FALSE), toBibtex))
#' @export
#' @seealso \code{\link{list_datasets}} \code{\link[utils]{bibentry}}
dataset_citations <- function(datasets = c(), astext = TRUE) {
  bibentries <- list(
    "WorldClim" = bibentry(bibtype = "Article",
      author = c(
        person(c("Robert","J."), "Hijmans"), person(c("Susan", "E."), "Cameron"),
        person(c("Juan","L."), "Parra"), person(c("Peter", "G."), "Jones"), 
        person("Andy", "Jarvis")),
      title = "Very high resolution interpolated climate surfaces for global land areas.",
      journal = "International Journal of Climatology",
      year = 2005, volume = 25, number = 15, pages = "1965-1978",
      doi = "10.1002/joc.1276", key = "WorldClim"),
    "Bio-ORACLE" = bibentry(bibtype = "Article",
      author = c(
        person("Lennert", "Tyberghein"), person("Verbruggen", "Heroen"),
        person("Klaas", "Pauly"), person("Charles", "Troupin"),
        person("Frederic", "Mineur"), person("Olivier", "De Clerck")),
      title = "Bio-ORACLE: a global environmental dataset for marine species distribution modelling",
      journal = "Global Ecology and Biogeography",
      year = 2012, volume = 21, number = 2, pages = "272-281",
      doi = "10.1111/j.1466-8238.2011.00656.x", key = "Bio-ORACLE"),
    "MARSPEC" = c(bibentry(bibtype = "Article",
      author = c(
        person(c("Elizabeth", "J."), "Sbrocco"), person(c("Paul", "H."), "Barber")),
      title = "MARSPEC: ocean climate layers for marine spatial ecology",
      year = 2013, volume = 94, number = 4, pages = "979",
      journal = "Ecology",
      doi = "10.1890/12-1358.1", key = "MARSPEC"), 
      bibentry(bibtype = "Article",
      author = person(c("Elizabeth", "J."), "Sbrocco"),
      title = "Paleo-MARSPEC: gridded ocean climate layers for the mid-Holocene and Last Glacial Maximum",
      journal = "Ecology",
      year = 2014, volume = 95, number = 6, pages = "1710",
      doi = "10.1890/14-0443.1", key = "Paleo-MARSPEC")))
  
  if(is.data.frame(datasets)) {
    datasets <- datasets$dataset_code
  } else if (is.null(datasets)) {
    datasets <- names(bibentries)
  }
  if(astext) {
    as.vector(sub("\\n", " ", sapply(bibentries[datasets], format)))
  } else {
    bibentries[datasets]
  }
}