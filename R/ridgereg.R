#' Ridge Regression Function
#'
#' Performs ridge regression analysis using least squares
#'
#' @param formula A formula object specifying the model
#' @param data A data.frame containing the data
#' @param lambda A numeric scalar
#'
#' @return An object of class "ridgereg" containing regression results
#'
#' @examples
#' data(iris)
#' mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda=0)
#' print(mod)
#'
#' @export
ridgereg <- function(formula, data, lambda){
  # Input validation
  if (missing(formula)) stop("formula argument is required")
  if (missing(data)) stop("data argument is required")
  if (missing(lambda)) stop("lambda argument is required")
  if (!inherits(formula, "formula")) stop("formula must be a formula object")
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!is.numeric(lambda)) stop("lambda must be numeric")


  # Extract variables from formula
  formula_vars <- all.vars(formula)
  response_var <- formula_vars[1]

  # Create model matrix and response vector
  X <- stats::model.matrix(formula, data)
  y <- data[[response_var]]


  # normalize all covariates
  means <- rep(NULL, ncol(X))
  variances <- rep(NULL, ncol(X))
  for(column in 1:ncol(X)){
    x <- X[, column]
    variances[column] <- stats::var(x)
    means[column] <- mean(x)
    if(variances[column]!=0){
      X[, column] <- (x-means[column])/variances[column]
    }
  }


  # Check dimensions
  n <- nrow(X)  # number of observations
  p <- ncol(X)  # number of parameters

  if (n <= p) stop("Number of observations must be greater than number of parameters")

  # Degrees of freedom
  df <- n - p

  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  beta_hat_ridge <- solve(XtX+lambda*diag(p)) %*% Xty

  # Fitted values: ŷ = Xβ̂
  y_hat <- as.vector(X %*% beta_hat_ridge)


  # Create coefficient names
  coef_names <- colnames(X)
  beta_hat_ridge <- as.vector(beta_hat_ridge)
  names(beta_hat_ridge) <- coef_names

  # Store call information
  call <- match.call()

  # Create result object
  result <- list(
    coefficients = beta_hat_ridge,
    fitted.values = y_hat,
    call = call,
    formula = formula,
    data_name = deparse(substitute(data)),
    model_matrix = X,
    response = y,
    lambda = lambda,
    means = means,
    variances = variances
  )

  class(result) <- "ridgereg"
  return(result)
}
