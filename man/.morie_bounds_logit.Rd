% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bounds_logit}
\alias{.morie_bounds_logit}
\title{.morie_bounds_logit}
\usage{
.morie_bounds_logit(X, y, max_iter = 100L, tol = 1e-10, ridge = 1e-08)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the bounds_native implementation. Called by \code{morie_efficiency_bound_ate}, \code{morie_text_ate}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fitted}, \code{separated}.
}
\keyword{internal}
