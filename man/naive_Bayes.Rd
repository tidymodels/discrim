% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/naive_Bayes.R
\name{naive_Bayes}
\alias{naive_Bayes}
\alias{update.naive_Bayes}
\title{General Interface for Naive Bayes Models}
\usage{
naive_Bayes(mode = "classification", smoothness = NULL, Laplace = NULL)

\method{update}{naive_Bayes}(object, smoothness = NULL, Laplace = NULL, fresh = FALSE, ...)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "classification".}

\item{smoothness}{An non-negative number representing the the relative
smoothness of the class boundary. Smaller examples result in model flexible
boundaries and larger values generate class boundaries that are less
adaptable}

\item{Laplace}{A non-negative value for the Laplace correction to smoothing
low-frequency counts.}

\item{object}{A linear discriminant model specification.}

\item{fresh}{A logical for whether the arguments should be
modified in-place of or replaced wholesale.}

\item{...}{Not used for \code{update()}.}
}
\description{
\code{naive_Bayes()} is a way to generate a \emph{specification} of a model before
fitting and allows the model to be created using different packages in R.
}
\details{
The main
arguments for the model are:
\itemize{
\item \code{smoothness}: The total amount of regularization
in the model. Note that this only used for the "klaR" engine where it is a
pure L2 smoothness (a.k.a ridge regression).
\item \code{Laplace}: Laplace correction for smoothing low-frequency counts.
}
These arguments are converted to their specific names at the time that the
model is fit. Other options and argument can be set using \code{set_engine()}. If
left to their defaults here (\code{NULL}), the values are taken from the
underlying model functions. If parameters need to be modified, \code{update()}
can be used in lieu of recreating the object from scratch.

For \code{naive_Bayes()}, the mode will always be "classification".

The model can be created using the \code{fit()} function using the following \emph{engines}:
\itemize{
\item \pkg{R}:  \code{"klaR"}(the default) or \code{"naivebayes"}
}
}
\section{Engine Details}{
Engines may have pre-set default arguments when executing the model fit
call. For this type of model, the template of the fit calls are:\if{html}{\out{<div class="sourceCode r">}}\preformatted{naive_Bayes() \%>\% 
  set_engine("klaR") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Naive Bayes Model Specification (classification)
## 
## Computational engine: klaR 
## 
## Model fit template:
## discrim::klar_bayes_wrapper(x = missing_arg(), y = missing_arg(), 
##     usekernel = TRUE)
}\if{html}{\out{<div class="sourceCode r">}}\preformatted{naive_Bayes() \%>\% 
  set_engine("naivebayes") \%>\% 
  translate()
}\if{html}{\out{</div>}}\preformatted{## Naive Bayes Model Specification (classification)
## 
## Computational engine: naivebayes 
## 
## Model fit template:
## naivebayes::naive_bayes(x = missing_arg(), y = missing_arg(), 
##     usekernel = TRUE)
}

The standardized parameter names in parsnip can be mapped to their
original names in each engine that has main parameters. Each engine
typically has a different default value (shown in parentheses) for each
parameter.\tabular{lll}{
   \strong{parsnip} \tab \strong{klaR} \tab \strong{naivebayes} \cr
   smoothness \tab adjust (1) \tab adjust (1) \cr
   Laplace \tab fL (0) \tab laplace (0) \cr
}


Note that \code{usekernel} is always set to \code{TRUE} for the \code{klaR} engine.
}

\examples{
parabolic_grid <-
  expand.grid(X1 = seq(-5, 5, length = 100),
              X2 = seq(-5, 5, length = 100))

nb_mod <-
  naive_Bayes(smoothness = .8) \%>\%
  set_engine("klaR") \%>\%
  fit(class ~ ., data = parabolic)

parabolic_grid$nb <-
  predict(nb_mod, parabolic_grid, type = "prob")$.pred_Class1

library(ggplot2)
ggplot(parabolic, aes(x = X1, y = X2)) +
  geom_point(aes(col = class), alpha = .5) +
  geom_contour(data = parabolic_grid, aes(z = nb), col = "black", breaks = .5) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()


model <- naive_Bayes(smoothness = 0.1)
model
update(model, smoothness = 1)
update(model, smoothness = 1, fresh = TRUE)
}
