#' linreg function
#'
#' Fits a linear regression model using QR decomposition.
#'
#' A description of the QR decomposition can be found here:
#' https://pages.stat.wisc.edu/~st849-1/lectures/Orthogonal.pdf
#'
#' @param formula A formula object describing the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @return An S3 object of class linreg containing the fitted model.
#' @export linreg
#' @importFrom stats model.matrix
linreg <- function(formula, data) {
  # Validate the input
  if (!inherits(formula, "formula")) {
    stop("The formula argument must be a formula object.")
  }

  if (!is.data.frame(data)) {
    stop("The data argument must be a data frame.")
  }

  # Create design matrix X and response vector y
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]

  # QR decomposition of X
  qr_dec <- qr(X)
  Q <- qr.Q(qr_dec)
  R <- qr.R(qr_dec)

  # Calculate beta coefficients
  beta <- solve(R, t(Q) %*% y)
  names(beta) <- colnames(X)

  # Calculate fits and residuals
  y_hat <- X %*% beta
  resid <- y - y_hat

  # Variance of residuals
  n <- nrow(X)
  p <- ncol(X)
  sigma2 <- sum(resid^2) / (n - p)

  # Variance of betas
  var_beta <- sigma2 * solve(R) %*% t(solve(R))

  # Return S3 object of class linreg
  return(structure(list(
    coefficients = beta,
    residuals = resid,
    fitted_values = y_hat,
    formula = formula,
    data = data,
    name = deparse(substitute(data)),
    sigma2 = sigma2,
    var_beta = var_beta,
    df_residual = n - p
  ), class = "linreg"))
}

#' Print method for linreg class
#'
#' Prints out the coefficients and coefficient names.
#'
#' @param x An S3 object of class linreg.
#' @export print
print <- function(x) {
  UseMethod("print")
}

#' @rdname print
#'
#' @export
print.linreg <- function(x) {
  cat(paste0("linreg(formula = ", deparse(x$formula), ", data = ", x$name, ")"))
  cat("\n\nCoefficients:\n")
  cat(names(x$coefficients))
  cat(paste0("\n", t(x$coefficients)))
}

#' Plot method for linreg class
#'
#' Plots the Residuals vs. Fitted & Scale-Location plots with ggplot2.
#'
#' @param x An S3 object of class linreg.
#' @export plot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 geom_text
#' @importFrom stats median
#' @importFrom gridExtra grid.arrange
plot <- function(x) {
  UseMethod("plot")
}

#' @rdname plot
#'
#' @export
plot.linreg <- function(x) {
  data <- x$data
  y_hat <- x$fitted_values
  residuals <- x$residuals

  # Standardized residuals
  std_residuals <- sqrt(abs(x$residuals) / sqrt(abs(x$sigma2)))

  # Define outlier threshold
  outl_threshold <- 1.63
  outl <- abs(std_residuals) > outl_threshold
  obs <- 1:length(x$residuals)

  # Create data frame for plotting
  plot_data <- data.frame(y_hat, residuals, std_residuals, outl, obs
  )

  # Residuals vs Fitted values
  p1 <- ggplot2::ggplot(plot_data, aes(x = y_hat, y = residuals)) +
    geom_point(shape = 1, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(aes(x = y_hat, y = residuals, group = 1),
      fun = median, color = "red", geom = "line", group = 1
    ) +
    labs(
      title = "Residuals vs Fitted",
      x = paste0("Fitted values\n", "linreg(", deparse(x$formula), ")"), y = "Residuals"
    ) +
    geom_text(aes(label = ifelse(outl, obs, "")),
      hjust = -0.4, vjust = -0.4
    ) +
    theme_minimal()

  # Scale-Location
  p2 <- ggplot2::ggplot(plot_data, aes(x = y_hat, y = std_residuals)) +
    geom_point(shape = 1, size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(aes(x = y_hat, y = std_residuals, group = 1),
      fun = median, color = "red", geom = "line", group = 1
    ) +
    labs(
      title = "Scale-Location",
      x = paste0("Fitted values\n", "linreg(", deparse(x$formula), ")"), y = "Standardized residuals"
    ) +
    geom_text(aes(label = ifelse(outl, obs, "")),
      hjust = -0.4, vjust = -0.4
    ) +
    theme_minimal()

  # Plot both plots
  gridExtra::grid.arrange(p1, p2, nrow = 2)
}

#' Residuals method for linreg class
#'
#' @param x An S3 object of class linreg.
#' @return A vector of residuals.
#' @export resid
resid <- function(x) {
  UseMethod("resid")
}

#' @rdname resid
#'
#' @export
resid.linreg <- function(x) {
  return(as.vector(x$residuals))
}

#' Coefficients method for linreg class
#'
#' @param x An S3 object of class linreg.
#' @return A named vector of coefficients.
#' @export coef
coef <- function(x) {
  UseMethod("coef")
}

#' @rdname coef
#'
#' @export

coef.linreg <- function(x) {
  coeff <- as.vector(x$coefficients)
  names(coeff) <- "Coefficients"
  return(coeff)
}

#' Summary method for linreg class
#' @param x An S3 object of class linreg.
#' @export summary
#' @importFrom stats pt
summary <- function(x) {
  UseMethod("summary")
}

#' @rdname summary
#'
#' @export

summary.linreg <- function(x) {
  # Calculate standard errors, t-values, and p-values
  se <- sqrt(diag(x$var_beta))
  t_values <- x$coefficients / se
  p_values <- 2 * pt(-abs(t_values), df = x$df_residual)

  # Function for showing asterisks
  fixstars <- function(p_values) {
    for (i in seq_along(p_values)) {
      if (p_values[i] > 0.1) {
        return(" ")
      }
      if (p_values[i] > 0.05) {
        return(".")
      }
      if (p_values[i] > 0.01) {
        return("*")
      }
      if (p_values[i] > 0.001) {
        return("**")
      }
    }
    return("***")
  }

  # Derive asterisks from p-values
  stars_p <- fixstars(p_values)

  # Coefficients table
  coef_table <- data.frame(
    Estimate = x$coefficients,
    SE = se,
    t = t_values,
    P = p_values,
    Significance = stars_p
  )
  print(coef_table)

  cat("\nResidual standard error:", round(sqrt(x$sigma2), digits = 3), "on", x$df, "degrees of freedom\n")
}

#' Predicted values method for linreg class
#' @param x An S3 object of class linreg.
#' @return A vector of predicted values.
#' @export pred
pred <- function(x) {
  UseMethod("pred")
}

#' @rdname pred
#'
#' @export
pred.linreg <- function(x) {
  return(as.vector(x$fitted_values))
}
