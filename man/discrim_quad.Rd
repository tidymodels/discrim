% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrim_quad.R
\name{discrim_quad}
\alias{discrim_quad}
\alias{update.discrim_quad}
\title{General Interface for Quadratic Discriminant Models}
\usage{
discrim_quad(mode = "classification")

\method{update}{discrim_quad}(object, fresh = FALSE, ...)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "classification".}

\item{object}{A quadratic discriminant model specification.}

\item{fresh}{A logical for whether the arguments should be
modified in-place of or replaced wholesale.}

\item{...}{Not used for \code{update()}.}
}
\description{
\code{discrim_quad()} is a way to generate a \emph{specification} of a quadratic
discriminant analysis (QDA) model before fitting and allows the model to be
created using different packages in R.
}
\details{
For \code{discrim_quad()}, the mode will always be "classification".

The model can be created using the \code{fit()} function using the following
\emph{engines}:
\itemize{
\item \pkg{R}:  \code{"MASS"}
}

This argument is converted to its specific names at the time that the model
is fit. Other options and argument can be set using \code{set_engine()}. If left
to their defaults here (\code{NULL}), the values are taken from the underlying
model functions. If parameters need to be modified, \code{update()} can be used
in lieu of recreating the object from scratch.
}
\section{Engine Details}{
Engines may have pre-set default arguments when executing the model fit
call. For this type of model, the template of the fit calls are:\if{html}{\out{<div class="sourceCode r">}}\preformatted{discrim_linear() \%>\% 
  set_engine("MASS") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Linear Discriminant Model Specification (classification)
## 
## Computational engine: MASS 
## 
## Model fit template:
## MASS::lda(formula = missing_arg(), data = missing_arg())
}
}

\examples{
parabolic_grid <-
  expand.grid(X1 = seq(-5, 5, length = 100),
              X2 = seq(-5, 5, length = 100))

qda_mod <-
  discrim_quad() \%>\%
  set_engine("MASS") \%>\%
  fit(class ~ ., data = parabolic)

parabolic_grid$qda <-
  predict(qda_mod, parabolic_grid, type = "prob")$.pred_Class1

library(ggplot2)
ggplot(parabolic, aes(x = X1, y = X2)) +
  geom_point(aes(col = class), alpha = .5) +
  geom_contour(data = parabolic_grid, aes(z = qda), col = "black", breaks = .5) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()
}
