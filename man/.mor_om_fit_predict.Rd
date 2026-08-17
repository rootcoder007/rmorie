% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_om_fit_predict}
\alias{.mor_om_fit_predict}
\title{.mor_om_fit_predict}
\usage{
.mor_om_fit_predict(X, y, rows, Xpred, outcome_model)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{rows}{See Usage.}
\item{Xpred}{A matrix; passed to \code{\%*\%}.}
\item{outcome_model}{Compared against \code{"logistic"}.}
}
\description{
A step of the causal implementation. Called by \code{morie_estimate_aipw}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
set.seed(1)
df <- data.frame(t = rbinom(200, 1, 0.4), y = rnorm(200), x = rnorm(200))
morie_estimate_aipw(df, "t", "y", "x")
}
