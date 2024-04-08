#' sort_by_name
#'
#' @param x a named vector
#'
#' @return sorted vector
#' @export
#'
#' @examples
#' a=setNames(c(1,2,3), nm=c("C", "A", "B"))
#' sort_by_name(a)
sort_by_name <- function(x){
  try(if(is.null(names(a))) stop("Vector is not named"))
  return(x[order(names(x))])
}
