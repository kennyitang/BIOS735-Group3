#' Calculate accuracy of classification 
#' 
#' This function calculates accuracy given vectors or matrices of actual class and predicted labels. 
#' 
#' The input vectors or matrices need to have the same dimension. 
#'
#' Any \code{NA} or \code{NaN} value in either input will result in \code{NA} of accuracy. 
#' 
#' @param actual a vector or a matrix of actual class labels with no \code{NA} or \code{NaN} values. 
#' @param predicted a vector or a matrix of predicted labels with no \code{NA} or \code{NaN} values. 
#' 
#' @return Accuracy, the percentage of correctly classified.
#' 
#' @examples
#' ## simple numeric example:
#' x = c(1,1)
#' y = c(1,2)
#' calc_acc(x, y)
#' 
#' ## works for vectors of different types of object
#' calc_acc(x, as.factor(y))
#' calc_acc(x, as.character(y))
#' 
#' @export
calc_acc = function(actual, predicted) {
  if(length(actual)!=length(predicted)){
    stop("actual and predicted do not have the same dimension")
  }
    else if (is.null(dim(actual))!=is.null(dim(predicted))) {
      stop("actual and predicted do not have the same dimension")
    } else if(is.null(dim(actual) & is.null(dim(predicted)))){
      if(dim(actual)!=dim(predicted)) stop("actual and predicted do not have the same dimension")
    } else{
  mean(actual == predicted)
    }
}