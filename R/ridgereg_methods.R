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
#' @param newdata Optional new data for predictions
#' @param ... Additional arguments (not used)
#' @return Vector of fitted values
#' @export
predict.ridgereg <- function(object, newdata=NULL, ...) {
  if(!is.null(newdata)){
    X <- stats::model.matrix(object$formula, newdata)

    # Check if we have the normalization parameters
    if(!is.null(object$sds) && !is.null(object$means)){
      # normalize all covariates using the same normalization as training
      for(column in 1:ncol(X)){
        if(column <= length(object$sds)){
          x <- X[, column]
          # Only normalize if it was normalized during training (sd != 1)
          if(!is.na(object$sds[column]) && object$sds[column] != 1 && object$sds[column] != 0){
            X[, column] <- (x - object$means[column]) / object$sds[column]
          }
        }
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

# Declare global variables to avoid R CMD check notes
utils::globalVariables(c("faa", "lon", "lat", "arr_delay", "dep_delay",
                         "airport", "delay", "mean_delay", "count"))

#' Visualize Airport Delays
#'
#' Creates a plot that visualizes the mean delay of flights for different airports
#' by longitude and latitude using ggplot2 and dplyr
#'
#' @return A ggplot2 plot object
#' @export
#' @importFrom dplyr inner_join rename select mutate bind_rows group_by summarize n %>%
#' @importFrom ggplot2 ggplot geom_point aes labs theme_minimal scale_color_gradient scale_size_continuous
visualize_airport_delays <- function() {
  if (!requireNamespace("nycflights13", quietly = TRUE)) {
    stop("nycflights13 package is required. Please install it.")
  }

  # Load data
  flights <- nycflights13::flights
  airports <- nycflights13::airports

  # Process destination delays using dplyr
  dest <- dplyr::inner_join(x = flights,
                            y = dplyr::rename(airports, dest = faa),
                            by = "dest") %>%
    dplyr::select(lon, lat, dest, arr_delay) %>%
    dplyr::rename(airport = dest, delay = arr_delay) %>%
    dplyr::mutate(type = "dest")

  # Process origin delays using dplyr
  origin <- dplyr::inner_join(x = flights,
                              y = dplyr::rename(airports, origin = faa),
                              by = "origin") %>%
    dplyr::select(lon, lat, origin, dep_delay) %>%
    dplyr::rename(airport = origin, delay = dep_delay) %>%
    dplyr::mutate(type = "origin")

  # Combine data using dplyr
  df <- dplyr::bind_rows(dest, origin)

  # Calculate mean delays by airport using dplyr
  data <- df %>%
    dplyr::group_by(airport, lon, lat) %>%
    dplyr::summarize(mean_delay = mean(delay, na.rm = TRUE),
                     count = dplyr::n(),
                     .groups = "drop")

  # Create plot using ggplot2
  p <- ggplot2::ggplot(data, ggplot2::aes(x = lon, y = lat,
                                          color = mean_delay,
                                          size = count)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "blue", high = "red",
                                  name = "Mean Delay (min)") +
    ggplot2::scale_size_continuous(name = "Number of Flights") +
    ggplot2::labs(title = "Mean Flight Delays by Airport Location",
                  x = "Longitude",
                  y = "Latitude") +
    ggplot2::theme_minimal()

  return(p)
}
