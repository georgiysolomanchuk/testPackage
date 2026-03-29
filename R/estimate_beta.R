#' Estimate OLS coefficients by linear algebra
#'
#' @param y A numeric vector of outcomes.
#' @param X A numeric matrix of regressors.
#'
#' @return A numeric vector of estimated coefficients.
#' @export
#'
#' @examples
#' y <- c(1, 2, 3)
#' X <- cbind(1, c(1, 2, 3))
#' estimate_beta(y, X)
estimate_beta <- function(y, X) {
  y <- as.numeric(y)
  X <- as.matrix(X)

  if (nrow(X) != length(y)) {
    stop("Number of rows of X must equal length of y.")
  }

  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

  return(as.numeric(beta_hat))
}