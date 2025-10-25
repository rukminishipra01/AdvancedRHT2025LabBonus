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
predict.ridgereg <- function(object, newdata=NULL, ...) {
  if(!is.null(newdata)){
    X <- stats::model.matrix(object$formula, newdata)
    # normalize all covariates
    for(column in 1:ncol(X)){
      x <- X[, column]
      if(object$variances[column]!=0){
        X[, column] <- (x-object$means[column])/object$variances[column]
      }
    }
    return(as.vector(X %*% object$coefficients))
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
#creates a plot that visualizes the mean delay of flights for different airports by longitude and latitude using ggplot2
visualize_airport_delays <- function() {
  # merge fright and airport data
  dest <- inner_join(x=flights, y=rename(airports, dest=faa), by="dest") %>%
    select("lon", "lat", "dest", "arr_delay") %>%
    rename(airport=dest, delay=arr_delay) %>%
    mutate(type="dest")
  origin <- inner_join(x=flights, y=rename(airports, origin=faa), by="origin") %>%
    select("lon", "lat", "origin", "dep_delay") %>%
    rename(airport=origin, delay=dep_delay) %>%
    mutate(type="origin")
  df <- bind_rows(dest, origin)

  data <- df %>%  group_by(airport, lon, lat) %>% summarize(mean_delay=mean(delay, na.rm=TRUE), count=nrow(type))
  ggplot(data) +
    geom_point(aes(x=lon, y=lat, color=mean_delay, size=mean_delay))
}
