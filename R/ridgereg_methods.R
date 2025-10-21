#' Print method for ridgereg objects
#' Prints the call the object was created with and the calculated regression coefficients.
#'
#' @param x A ridgereg object
#' @param ... Additional arguments (not used)
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

#' Extract predicted values from ridgereg object
#'
#' @param object A ridgereg object
#' @param ... Additional arguments (not used)
#' @return Vector of fitted values
#' @export
predict <- function(object, ...) {
  UseMethod("predict")
}

#' @export
predict.ridgereg <- function(object, newdata=NULL, ...) {
  if(!is.null(newdata)){
    # fit model on the new data before getting the predicted values
    object <- ridgereg(formula=object$formula, data=newdata, lambda=formula$lambda)
  }
  return(object$fitted.values)
}

#' Extract coefficients from ridgereg object
#'
#' @param object A ridgereg object
#' @param ... Additional arguments (not used)
#' @return Named vector of coefficients
#' @export
coef.ridgereg <- function(object, ...) {
  return(object$coefficients)
}
