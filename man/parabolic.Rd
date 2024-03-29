% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{parabolic}
\alias{parabolic}
\title{Parabolic class boundary data}
\value{
\item{parabolic}{a data frame}
}
\description{
Parabolic class boundary data
}
\details{
These data were simulated. There are two correlated predictors and
two classes in the factor outcome.
}
\examples{
data(parabolic)

library(ggplot2)
ggplot(parabolic, aes(x = X1, y = X2, col = class)) +
  geom_point(alpha = .5) +
  theme_bw()

}
\keyword{datasets}
