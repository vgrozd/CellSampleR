#' Test.R
#'
#' @param message A string to be returned as message
#'
#' @return Success message
#' @export
#'
#' @examples
#' Test()
#' Test("The test was successfull!")
Test=function(message="Test successfull"){
  message(message)
}
