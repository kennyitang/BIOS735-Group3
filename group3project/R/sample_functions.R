#========================================================================================================
#========= some sample functions (with sample documentation) that can be used for sample tests ==========
#========================================================================================================
## note: if using importFrom, add Imports: packagename to DESCRIPTION 


#' Sum of two vectors of numbers
#' 
#' This function sums two vectors of numbers and optionally
#' allows for the negative of the sum to be returned.
#' 
#' @param x a vector of numbers
#' @param y a vector of numbers
#' @param negative logical, whether to flip the sign of the sum
#'
#' @return the sum of the two vectors
#' 
#' @examples
#'
#' add(1:5, 6:10)
#' add(1:5, 6:10, negative=TRUE)
#'
#' @importFrom gtools rdirichlet
#' 
#' @export
add <- function(x,y,negative=FALSE) {
  d <- gtools::rdirichlet(1, alpha=c(1,2,3))
  z <- x + y
  if (negative) {
    z <- -1 * z
  }
  z
}


#' Sum of two vectors of numbers
#' 
#' This function sums two vectors of numbers and optionally
#' allows for the negative of the sum to be returned.
#' 
#' This paragraph will start the Details section...
#' 
#' This will be the second paragraph of the Details section... 
#' 
#' @param x a vector of numbers
#' @param y a vector of numbers
#' @param negative logical, whether to flip the sign of the sum
#' 
#' @return the following elements:
#' \itemize{
#' \item{the sum of two vectors}
#' \item{...}
#' }
#' 
#' @examples
#'
#' add(1:5, 6:10)
#' add(1:5, 6:10, negative=TRUE)
#'
add2 <- function(x,y,negative=FALSE) {
  z <- x + y
  if (negative) {
    z <- -1 * z
  }
  if (any(z < 0)) {
    warning("some output values are negative")
  }
  z
}