#' Linear Regression Function
#'
#' Performs linear regression analysis using matrix computations
#'
#' @param formula A formula object specifying the model
#' @param data A data.frame containing the data
#' @param method Method to use: "ols" for ordinary least squares or "qr" for QR decomposition
#'
#' @return An object of class "linreg" containing regression results
#'
#' @examples
#' data(iris)
#' mod <- linreg(Petal.Length ~ Species, data = iris)
#' print(mod)
#'
#' @export
linreg <- function(formula, data, method = "ols") {

  # Input validation
  if (missing(formula)) stop("formula argument is required")
  if (missing(data)) stop("data argument is required")
  if (!inherits(formula, "formula")) stop("formula must be a formula object")
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!(method %in% c("ols", "qr"))) stop("method must be 'ols' or 'qr'")

  # Extract variables from formula
  formula_vars <- all.vars(formula)
  response_var <- formula_vars[1]

  # Create model matrix and response vector
  X <- stats::model.matrix(formula, data)
  y <- data[[response_var]]

  # Check dimensions
  n <- nrow(X)  # number of observations
  p <- ncol(X)  # number of parameters

  if (n <= p) stop("Number of observations must be greater than number of parameters")

  # Degrees of freedom
  df <- n - p

  # Calculate regression coefficients based on method
  if (method == "ols") {
    # Method 1: Ordinary Least Squares (from textbook)
    # β̂ = (X'X)^(-1) X'y
    XtX <- t(X) %*% X
    Xty <- t(X) %*% y
    beta_hat <- solve(XtX) %*% Xty

    # Variance-covariance matrix: σ̂²(X'X)^(-1)
    XtX_inv <- solve(XtX)

  } else {
    # Method 2: QR Decomposition (from textbook)
    # X = QR decomposition
    qr_decomp <- qr(X)
    Q <- qr.Q(qr_decomp)
    R <- qr.R(qr_decomp)

    # From textbook: β̂ = R^(-1) Q'y
    Qty <- t(Q) %*% y
    beta_hat <- backsolve(R, Qty)

    # For variance calculation: Var(β̂) = σ̂²(R'R)^(-1) = σ̂²(X'X)^(-1)
    XtX_inv <- chol2inv(chol(t(R) %*% R))
  }

  # Fitted values: ŷ = Xβ̂
  y_hat <- as.vector(X %*% beta_hat)

  # Residuals: ê = y - ŷ
  residuals <- y - y_hat

  # Residual sum of squares
  rss <- sum(residuals^2)

  # Residual standard error: σ̂² = e'e / df
  sigma2 <- rss / df
  sigma <- sqrt(sigma2)

  # Variance of regression coefficients: Var(β̂) = σ̂²(X'X)^(-1)
  var_beta_hat <- sigma2 * XtX_inv

  # Standard errors of coefficients
  se_beta <- sqrt(diag(var_beta_hat))

  # t-values: t = β̂ / SE(β̂)
  t_values <- as.vector(beta_hat) / se_beta

  # p-values (two-tailed test)
  p_values <- 2 * stats::pt(-abs(t_values), df = df)

  # Create coefficient names
  coef_names <- colnames(X)
  beta_hat <- as.vector(beta_hat)
  names(beta_hat) <- coef_names
  names(se_beta) <- coef_names
  names(t_values) <- coef_names
  names(p_values) <- coef_names

  # Store call information
  call <- match.call()

  # Create result object
  result <- list(
    coefficients = beta_hat,
    residuals = residuals,
    fitted.values = y_hat,
    df.residual = df,
    sigma = sigma,
    var_coef = var_beta_hat,
    std_error = se_beta,
    t_values = t_values,
    p_values = p_values,
    call = call,
    formula = formula,
    data_name = deparse(substitute(data)),
    method = method,
    model_matrix = X,
    response = y
  )

  class(result) <- "linreg"
  return(result)
}
