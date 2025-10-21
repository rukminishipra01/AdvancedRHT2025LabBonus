#' Print method for linreg objects
#' Prints the call the object was created with and the calculated regression coefficients.
#'
#' @param x A linreg object
#' @param ... Additional arguments (not used)
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

#' Plot method for linreg objects
#'
#' Creates two diagnostic plots using ggplot2, one plotting the residuals versus
#' fitted values and the scale-location plot (the square root of the
#' standardized residuals force the fitted values).
#'
#' @param x A linreg object
#' @param ... Additional arguments (not used)
#' @return A list containing two ggplot objects, the Residuals vs Fitted and Scale-Location plots
#' @export
plot.linreg <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Please install it.")
  }

  # Prepare data for plotting
  fitted_vals <- x$fitted.values
  residuals <- x$residuals
  standardized_residuals <- sqrt(abs(residuals / x$sigma))

  # set to NULL first to get rid of check note that cannot see them in the df
  fitted <- NULL
  obs_number <- NULL
  std_residuals <- NULL

  # Create data frame for plotting
  plot_data <- data.frame(
    fitted = fitted_vals,
    residuals = residuals,
    std_residuals = standardized_residuals,
    obs_number = seq_along(residuals)
  )

  # Find outliers for labeling (top 3 by absolute residual)
  outlier_indices <- order(abs(residuals), decreasing = TRUE)[1:3]

  # Plot 1: Residuals vs Fitted
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = paste("Residuals vs Fitted\n", deparse(x$formula)),
      x = "Fitted values",
      y = "Residuals"
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_text(data = plot_data[outlier_indices, ],
                       ggplot2::aes(label = obs_number),
                       vjust = -0.5, size = 3)

  # Plot 2: Scale-Location
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = fitted, y = std_residuals)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 0.8) +
    ggplot2::labs(
      title = paste("Scale-Location\n", deparse(x$formula)),
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_text(data = plot_data[outlier_indices, ],
                       ggplot2::aes(label = obs_number),
                       vjust = -0.5, size = 3)

  # Print plots
  print(p1)
  print(p2)

  # Return plots invisibly for further manipulation if needed
  invisible(list(residuals_vs_fitted = p1, scale_location = p2))
}

#' Extract residuals from linreg object
#'
#' @param object A linreg object
#' @param ... Additional arguments (not used)
#' @return Vector of residuals
#' @export
resid <- function(object, ...) {
  UseMethod("resid")
}

#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}

#' Extract predicted values from linreg object
#'
#' @param object A linreg object
#' @param ... Additional arguments (not used)
#' @return Vector of fitted values
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' @export
pred.linreg <- function(object, ...) {
  return(object$fitted.values)
}

#' Extract coefficients from linreg object
#'
#' @param object A linreg object
#' @param ... Additional arguments (not used)
#' @return Named vector of coefficients
#' @export
coef.linreg <- function(object, ...) {
  return(object$coefficients)
}

#' Summary method for linreg objects
#' Prints the call used to create the linreg object, its coefficients, the
#' residual standard error, and degrees of freedom.
#'
#' @param object A linreg object
#' @param ... Additional arguments (not used)
#' @return Invisible summary object (prints automatically)
#' @export
summary.linreg <- function(object, ...) {

  # Create coefficients table
  coef_table <- data.frame(
    Estimate = as.vector(object$coefficients),
    `Std. Error` = object$std_error,
    `t value` = object$t_values,
    `Pr(>|t|)` = object$p_values,
    check.names = FALSE
  )
  rownames(coef_table) <- names(object$coefficients)

  # Add significance stars
  stars <- ifelse(object$p_values < 0.001, "***",
                  ifelse(object$p_values < 0.01, "**",
                         ifelse(object$p_values < 0.05, "*",
                                ifelse(object$p_values < 0.1, ".", " "))))
  coef_table$` ` <- stars

  # Print directly (like base R summary.lm)
  cat("Call:\n")
  print(object$call)
  cat("\nCoefficients:\n")
  print(coef_table)
  cat("\nResidual standard error:", round(object$sigma, 4),
      "on", object$df.residual, "degrees of freedom")
  cat("\n\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  # Return invisibly for potential further use
  result <- list(
    call = object$call,
    coefficients = coef_table,
    sigma = object$sigma,
    df = object$df.residual
  )
  class(result) <- "summary.linreg"
  invisible(result)
}
