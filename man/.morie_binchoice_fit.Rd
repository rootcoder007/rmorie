% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_binchoice_fit}
\alias{.morie_binchoice_fit}
\title{Newton-Raphson binary choice, probit or logit, returning fitted}
\usage{
.morie_binchoice_fit(X, y, link = "probit", max_iter = 100L, tol = 1e-09)
}
\arguments{
\item{X}{See Usage.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{link}{Compared against \code{"probit"}. Defaults to \code{"probit"}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
probabilities. stats::pnorm/dnorm are used as the normal CDF and density -- arithmetic primitives, in the same sense as exp() -- not as a model-fitting delegate.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
