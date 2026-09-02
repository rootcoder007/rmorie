% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{log_evidence}
\alias{log_evidence}
\title{log_evidence}
\usage{
log_evidence(docs, T, V, z, alpha = 0.5, beta = 0.5, m = NULL, n = NULL) 
}
\arguments{
\item{docs}{Iterated over elementwise, with \code{lapply}.}
\item{T}{Coerced to integer by the body, with \code{as.integer}.}
\item{V}{Coerced to integer by the body, with \code{as.integer}.}
\item{z}{Passed to \code{.log_evidence}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{m}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bigtm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.log_evidence}.
}
\keyword{internal}
\examples{
log_evidence(docs = c(1, 2, 3, 4, 5, 6, 7, 8), T = 5L, V = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8))
}
