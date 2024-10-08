% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LinearRegression.R
\name{linreg}
\alias{linreg}
\title{linreg function}
\usage{
linreg(formula, data)
}
\arguments{
\item{formula}{A formula object describing the model to be fitted.}

\item{data}{A data frame containing the variables in the model.}
}
\value{
An S3 object of class linreg containing the fitted model.
}
\description{
Fits a linear regression model using QR decomposition.
}
\details{
A description of the QR decomposition can be found here:
https://pages.stat.wisc.edu/~st849-1/lectures/Orthogonal.pdf
}
