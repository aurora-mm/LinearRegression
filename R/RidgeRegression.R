#' ridgereg function
#'
#' Performs ridge regression on a given formula and dataset.
#' It normalizes the covariates and calculates the ridge regression coefficients.
#'
#' @param formula A formula object describing the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param lambda A numeric value specifying the ridge regularization parameter.
#'
#' @return An S3 object of class ridgereg containing the fitted model.
#'
#' @export 
#' @importFrom stats model.response
#' @importFrom stats model.frame
#' @importFrom stats sd
#' @importFrom stats predict
#'
ridgereg <- function(formula, data, lambda) {
  # Validate the input
  if (!inherits(formula, "formula")) {
    stop("The formula argument must be a formula object.")
  }

  if (!is.data.frame(data)) {
    stop("The data argument must be a data frame.")
  }

  if (!is.numeric(lambda)) {
    stop("The lambda argument must be numeric.")
  }

  # Parse the formula and extract the response and predictor variables
  X <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))

  # Standardize predictor variables (excluding intercept)
  X_mean <- colMeans(X[, -1])
  X_sd <- apply(X[, -1], 2, sd)
  X[, -1] <- scale(X[, -1], center = X_mean, scale = X_sd)

  # Calculate ridge regression coefficients: (X'X + lambda*I)^(-1) X'y
  p <- ncol(X)
  I_lambda <- diag(lambda, p, p)
  I_lambda[1, 1] <- 0 # Do not regularize intercept term

  # Ridge regression coefficients
  beta <- solve(t(X) %*% X + I_lambda) %*% t(X) %*% y

  # Calculate fitted values
  fits <- X %*% beta

  # Create output object with class ridgereg
  ridgereg_obj <- list(
    coefficients = beta,
    fitted_values = fits,
    lambda = lambda,
    formula = formula,
    data = data,
    X_mean = X_mean,
    X_sd = X_sd
  )

  # Assign S3 class "ridgereg" to the output object
  class(ridgereg_obj) <- "ridgereg"
  return(ridgereg_obj)
}

#' Print method for ridgereg class
#'
#' Prints out the coefficients and coefficient names.
#'
#' @param x An S3 object of class ridgereg.
#' @param ... Additional arguments (unused).
#' @export 
print.ridgereg <- function(x, ...) {
  if (!inherits(x, "ridgereg")) {
    stop("This method is only for objects of class ridgereg.")
  }
  cat("\n\nCall:\n")
  cat(paste0("ridgereg(formula = ", deparse(x$formula), ", data = ", x$name, ")"))
  cat("\n\nCoefficients:\n")
  cat(names(x$coefficients))
  cat(paste0("\n", t(x$coefficients)))
}

#' Predict method for ridgereg class
#'
#' Returns the preducted values.
#'
#' @param object An S3 object of class ridgereg.
#' @param newdata A data frame with new data (optional)
#' @param ... Additional arguments (unused).
#' @export 
predict.ridgereg <- function(object, newdata = NULL, ...) {
  x <- object
  if (!inherits(x, "ridgereg")) {
    stop("This method is only for objects of class ridgereg.")
  }
  if (is.null(newdata)) {
    return(x$fitted_values)
  } else {
    # Normalize the new data using the training data's mean and sd
    X_new <- model.matrix(x$formula, newdata)
    X_new[, -1] <- scale(X_new[, -1], center = x$X_mean, scale = x$X_sd)

    # Predict using the ridge regression coefficients
    return(X_new %*% x$coefficients)
  }
}

#' Coefficients method for ridgereg class
#'
#' @param x An S3 object of class ridgereg.
#' @return A named vector of coefficients.
#' @export
coef.ridgereg <- function(x) {
  if (!inherits(x, "ridgereg")) {
    stop("This method is only for objects of class ridgereg.")
  }
  coeff <- as.vector(x$coefficients)
  names(coeff) <- "Coefficients"
  return(coeff)
}
