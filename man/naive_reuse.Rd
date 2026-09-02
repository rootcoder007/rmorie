% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{naive_reuse}
\alias{naive_reuse}
\title{naive_reuse}
\usage{
naive_reuse(define_and_estimate, n, seed = 0L) 
}
\arguments{
\item{define_and_estimate}{Accepted by the signature and not used anywhere in the body.}
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Accepted by the signature and not used anywhere in the body. Defaults to \code{0L}.}
}
\description{
A step of the tldapar_native implementation. Called by \code{morie_tldapar}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{warning}.
}
\keyword{internal}
\examples{
r <- naive_reuse(function(idx) list(estimate = mean(idx)), n = 50)
str(r, max.level = 1)
}
