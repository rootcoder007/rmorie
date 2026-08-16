% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_glm_quantal}
\alias{.morie_glm_quantal}
\title{.morie_glm_quantal}
\usage{
.morie_glm_quantal(X, k, n, link, max_iter = 100L, tol = 1e-11)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{k}{Numeric; passed to \code{sum}.}
\item{n}{Numeric; passed to \code{sum}.}
\item{link}{One of \code{"logit"}, \code{"probit"}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-11}.}
}
\description{
Part of the median_frames_dose_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{cov}, \code{converged}, \code{fitted}.
}
\keyword{internal}
