% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{fit_regularized_linear}
\alias{fit_regularized_linear}
\alias{fit_regularized_quad}
\title{Wrapper for sparsediscrim models}
\usage{
fit_regularized_linear(x, y, method = "diagonal", ...)

fit_regularized_quad(x, y, method = "diagonal", ...)
}
\arguments{
\item{x}{A matrix or data frame.}

\item{y}{A factor column.}

\item{method}{A character string.}

\item{...}{Not currently used.}
}
\value{
A sparsediscrim object
}
\description{
Wrapper for sparsediscrim models
}
\keyword{internal}
